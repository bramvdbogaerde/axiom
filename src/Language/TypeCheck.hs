{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.TypeCheck(
  -- * Errors
  ModelError(..),
  Error(..),

  -- * Type checking
  runChecker,
  runChecker',
  runCheckTerm,
  runCheckerWithContext,

  -- * Contexts
  CheckingContext(..),

  -- * Re-exports
  TypingContext,
  Subtyping)
 where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Void (absurd)
import Control.Monad.State
import Control.Lens hiding (Context)
import Language.AST
import qualified Data.Graph as Graph
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Extra
import Language.Types
import Data.Tuple
import Data.Maybe (isJust, fromJust, catMaybes)


-----------------------------------------
-- Errors
-----------------------------------------

-- First, we define errors that could be produced in the type checker.
data ModelError = DuplicateVariable String Typ
                | DuplicateSort Typ
                | NoNestingAt Typ
                | SortNotDefined String
                | IncompatibleTypes [Typ] [Typ]
                | NameNotDefined String
                | ArityMismatch String Int Int  -- ^ functor name, expected arity, actual arity
                | HaskellExprTypeInferenceError  -- ^ HaskellExpr type inference failure
                | InvalidConstructor String  -- ^ term type cannot be used as constructor in syntax declaration
                deriving (Ord, Eq, Show)

data Error = Error { err :: ModelError, raisedAt :: Maybe Range,  ctx :: CheckingContext }
           deriving (Ord, Eq, Show)


-----------------------------------------
-- Monadic context
-----------------------------------------

-- Next, we define a monadic context in which most of the type checking will be performed.
type MonadCheck m = (MonadState CheckingContext m, -- the type checking context itself
                     MonadError Error m,           -- error-producing monad
                     MonadReader (Maybe Typ) m     -- propagation of types in parent used for Haskell expressions (cf. below)
                     )
-- In this monad, we can also raise errors.
-- | Throw an error with an optional range
throwErrorMaybe :: MonadCheck m => Maybe Range -> ModelError -> m a
throwErrorMaybe range err = get >>= Except.throwError . Error err range

-- | Throw an error at the given location in the sourcez by adding the current context to it 
throwErrorAt :: MonadCheck m => Range -> ModelError -> m a
throwErrorAt range = throwErrorMaybe (Just range)


-- Types are declared in syntax blocks. Types can be defined with our without data constructors.
-- (1) Types with data constructors:
-- ```
-- exp in Exp ::= if(exp, exp, exp) | app(exp, exp) | lam(x, exp)
-- ```
-- denotes a type called "Exp" which has three data constructors, "if", "app" and "lam".
-- The type of the "app" data constructor is "app :: Exp -> Exp -> Exp", while the "lam"
-- data constructor has type "Ident -> Exp -> Exp".
--
-- (2) Types without data constructors:
-- ```
-- vlu in Set(Val);
-- ```
-- states that atoms named "vlu\d+" are of type "Set(Val)" but does not associate any data constructors
-- with the type. Importantly, this association **always** happens regardless of whether there are
-- data constructors or not. The lack of data constructors does not mean that there are no terms for
-- that type. Indeed, for some builtin types such as "Int", the language provides **literal** support.
--

-- The typing context is tracked throughout the type checking, and corresponds to "Γ". The context also includes
-- the subtyping graph, as well as a set of defined sorts which is used in code generation to generate the appropriate
-- data types for embedded Haskell expressions.
data CheckingContext = CheckingContext {
                         _typingContext :: TypingContext
                       , _subtypingGraph :: Subtyping
                       , _definedSorts :: Map Typ (Set Range)
                       , _rewriteNames :: Set String
                       , _typeAliases :: Map String TypeCon
                      }
                     deriving (Ord, Eq, Show)

-- Next, we define some auxilary functions for interacting with the checking and type context data types.
emptyCheckingContext :: CheckingContext
emptyCheckingContext = CheckingContext Map.empty Graph.empty builtins Set.empty Map.empty
  where builtins = Map.fromList ((,Set.empty) <$> [IntType, StrType, BooType, AnyType, VoidType])

$(makeLenses ''CheckingContext)


-- For convenience, we also define some auxilary functions to interact with this typing context within the monad.
-- | Lookup the type associated with the given name. Returns an error if the type does not exist.
lookupType :: MonadCheck m => Maybe Range -> String -> m Typ
lookupType r nam = gets (^. typingContext) >>= maybe (throwErrorMaybe r (NameNotDefined nam)) return . Map.lookup nam

-- | Checks whether the name has already a type associated with it 
hasType :: MonadCheck m => String -> m Bool
hasType nam = gets (isJust . Map.lookup nam . _typingContext)

-- | Associate the given name with the given type. 
typedAsUpdated :: MonadCheck m => String -> Typ -> m ()
typedAsUpdated nam tpy = typingContext %= Map.insert nam tpy

-- | Same as 'typedAsUpdated' but produces an error if the term has already been typed 
typedAs :: MonadCheck m => Maybe Range -> String -> Typ -> m ()
typedAs r nam tpy =
  ifM (hasType nam)
      (throwErrorMaybe r (DuplicateVariable nam tpy))
      (typedAsUpdated nam tpy)

-- | Checks if the type has been defined  
isDefinedSort :: MonadCheck m => Typ -> m Bool
isDefinedSort tpy = gets (Map.member tpy . _definedSorts)

-- | Add a sort to the set of defined sorts
defineSort :: MonadCheck m => Typ -> Range -> m ()
defineSort sortName range = modify (over definedSorts (Map.insertWith Set.union sortName (Set.singleton range)))

-- | Add the name of a rewrite rule to the set of defined rewrites
defineRewriteName :: MonadCheck m => String -> m ()
defineRewriteName = modify . over rewriteNames . Set.insert

-- | Checks whether the given name refers to a rewrite rule
isRewriteRule :: MonadCheck m => String -> m Bool
isRewriteRule nam = gets (Set.member nam . _rewriteNames)

-- | Add a type alias to the context
defineTypeAlias :: MonadCheck m => String -> TypeCon -> m ()
defineTypeAlias name typeCon = modify (over typeAliases (Map.insert name typeCon))

-- | Resolve type aliases to their fully expanded form, detecting circularity
resolveTypeAlias :: MonadCheck m => Maybe Range -> TypeCon -> m TypeCon
resolveTypeAlias r = resolveTypeAlias' Set.empty
  where
    resolveTypeAlias' :: MonadCheck m => Set String -> TypeCon -> m TypeCon
    resolveTypeAlias' visited (TypeTyp name range) = do
      aliases <- gets _typeAliases
      case Map.lookup name aliases of
        Nothing -> return (TypeTyp name range)
        Just aliasDef ->
          if Set.member name visited
          then throwErrorMaybe r (SortNotDefined $ "Circular type alias detected: " ++ name)
          else resolveTypeAlias' (Set.insert name visited) aliasDef
    -- Handle TypeApp-wrapped simple type references (from parser: IDENT becomes TypeApp (TypeTyp name) [])
    resolveTypeAlias' visited (TypeApp (TypeTyp name range) [] _) = do
      aliases <- gets _typeAliases
      case Map.lookup name aliases of
        Nothing -> return (TypeApp (TypeTyp name range) [] range)
        Just aliasDef ->
          if Set.member name visited
          then throwErrorMaybe r (SortNotDefined $ "Circular type alias detected: " ++ name)
          else resolveTypeAlias' (Set.insert name visited) aliasDef
    resolveTypeAlias' visited (TypeApp con args range) = do
      resolvedCon <- resolveTypeAlias' visited con
      resolvedArgs <- mapM (resolveTypeAlias' visited) args
      return (TypeApp resolvedCon resolvedArgs range)
    resolveTypeAlias' _ tc@(TypeVar _ _) = return tc
    resolveTypeAlias' _ tc@(TypeHas _ _) = return tc

-----------------------------------------
-- Typing infrastructure
-----------------------------------------

-- We define subtyping checks through a type class so that this check is easier to perform on a range of types and combinations
-- thereof. -- The "subtypeOf" functions represents the "<:" judgement.
-- | Checks whether the given type is a subtype of the other type
class SubtypeOf s where
  subtypeOf :: MonadCheck m => s -> s -> m Bool

instance SubtypeOf s => SubtypeOf [s] where
  subtypeOf l1 l2
    | length l1 /= length l2 = return False
    | otherwise = and <$> zipWithM subtypeOf l1 l2

instance SubtypeOf Typ where
  subtypeOf s1 s2
    | s1 == s2 = return True -- (1) 
    | otherwise = gets (isSubtypeOf s1 s2 . _subtypingGraph)


-- | Records a subtyping relationship between the given types
subtype :: MonadCheck m => Typ -> Typ -> m ()
subtype from to =
  subtypingGraph %= Graph.addEdge () from to

-- Based on subtyping rules, types can also be widened. Viewing the subtyping relation as a partial
-- order between types, the widened type corresponds to the join of the lattice induced by partial order.
-- More concretely, the 'widenType' function  always returns the most specific common type between the given types.
-- 
-- | Generic widening function that works with any type.
widenType :: (MonadCheck m) => Range -> Typ  -> Typ -> m Typ
widenType range type1 type2 = do
  -- TODO: if two types are not directly compatible but share a common ancenstor then that common
  -- ancestor should be returned. The implementation is currently too restrictive.
  isSubset1 <- subtypeOf type1 type2
  isSubset2 <- subtypeOf type2 type1
  if isSubset1
    then return type2      -- type2 is more general
    else if isSubset2 then return type1 -- type1 is more general
    else throwErrorAt range (IncompatibleTypes [type1] [type2])


-- In effect, we differentiate between two different kinds of types:
-- - primitive types that do not contain any other types
-- - function types that need to be applied with values (data constructors)
--
-- To obtain the type resulting from applying a data constructor, one should
-- supply values to the data constructor. Here, we define a function that receives
-- a list of a types to apply to a curried function. If the types do not match an error is returned.
--
-- Matching typing rule (the capital Ti's on the left handside of the typing judgement are a shorthand for terms `Γ, ti |- Ti`):
--         T1' <: T1
--   ======================= T_App
--   Γ, (T1 -> T2) T1' |- T2
applyTpy :: MonadCheck m => Maybe Range -> Typ -> [m (a, Typ)] -> m ([a], Typ)
applyTpy r (FunType t1 ts) (mt1':ts') = do
   (v, t1') <- local (const $ Just t1) mt1'
   ifM (subtypeOf t1' t1)
       (do (vs, tr) <- applyTpy r ts ts'
           return (v:vs, tr))
       (throwErrorMaybe r (IncompatibleTypes [t1] [t1']))
-- TODO: also support partial functions consisting of more than one argument
applyTpy r (MapOf t1 t2) [mt1'] = do
  (v, t1') <- local (const $ Just t1) mt1'
  ifM (subtypeOf t1' t1)
       (return ([v], t2))
       (throwErrorMaybe r (IncompatibleTypes [t1] [t1']))
applyTpy _ t [] = return ([], t)
applyTpy r t ts = throwErrorMaybe r (ArityMismatch (show t) 0 (length ts))

-----------------------------------------
-- Type checker
-----------------------------------------

-- The type checker itself proceeds in three passes over the program.
-- The first pass associates all atoms on the left hand-side the "in" keyword with the type on the right-handside,
-- this makes all of these names available for the next pass.
-- The second pass then associates all data constructor names with their types, this also includes transitions
-- which are actually just data constructors. The third pass then considers rewrite and regular rules, and annotates 
-- the AST with the appropriate types.
--

----------------------------------
-- Type Alias Pass: collect type aliases (without resolution)
----------------------------------

-- This pass collects all type alias definitions before any resolution occurs.
-- This allows type aliases to reference types that are defined later in the program.
typeAliasPassVisitDecl :: MonadCheck m => Decl -> m ()
typeAliasPassVisitDecl (Syntax _ decls _) = mapM_ typeAliasPassVisitSyntaxDecl decls
  where
    typeAliasPassVisitSyntaxDecl (TypeAliasDecl name typeCon _) =
      defineTypeAlias name typeCon
    typeAliasPassVisitSyntaxDecl (SyntaxDecl {}) = return ()
typeAliasPassVisitDecl _ = return ()

typeAliasPass :: MonadCheck m => Program -> m ()
typeAliasPass (Program decls _) = mapM_ typeAliasPassVisitDecl decls

----------------------------------
-- Pass 0: declare all sorts and variable names
----------------------------------

pass0VisitDecl :: MonadCheck m => Decl -> m ()
pass0VisitDecl (Syntax _ decls _) = mapM_ pass0VisitSyntaxDecl decls
  where
    -- Type aliases are skipped in this pass (they were already collected in typeAliasPass)
    pass0VisitSyntaxDecl (TypeAliasDecl {}) = return ()
    -- In this phase, we associate the atoms in "vars" with the type denoted by "tpy".
    -- We also check whether the declaration has any data constructors. These are only allowed when
    -- the type denotes a user-defined sort (i.e., none of the builtin ones).
    pass0VisitSyntaxDecl (SyntaxDecl vars tpy ctors range) = do
      -- Resolve type aliases in the type constructor
      resolvedTpy <- resolveTypeAlias (Just range) tpy
      tpy' <- either (const $ throwErrorAt range (SortNotDefined (show resolvedTpy))) return $ fromTypeCon resolvedTpy
      defineSort tpy' range
      when (not (isUserDefined tpy') && not (null ctors)) $
        throwErrorAt range (InvalidConstructor (toSortName tpy'))
      mapM_ (flip (typedAs (Just range)) tpy') vars
pass0VisitDecl _ = return ()

pass0 :: MonadCheck m => Program -> m ()
pass0 (Program decls _) = do
    mapM_ pass0VisitDecl decls
    gets (Map.toList . fmap Set.toList . _definedSorts) >>= mapM_ (uncurry checkTypeInstantiated)
  where checkTypeInstantiated sort rs  =
            mapM_ (\sort -> ifM (isDefinedSort sort)
                                (return ())
                                (throwErrorMaybe (safeHead rs) (SortNotDefined (toSortName sort))))
                  (referencedTypes sort)
        safeHead [] = Nothing
        safeHead (r:_) = Just r

-----------------------------------------
-- Pass 1: associate functors in the syntax declarations with sorts, register subtyping for variables
-----------------------------------------

-- Here we just have to look for the data constructors and convert the atoms occuring in the terms
-- to their correct type, and then type the data constructor itself in the typing context.
-- | Process and associate the data constructors in the given sort
pass1VisitCtor :: MonadCheck m => Typ -> PureTerm -> m ()
pass1VisitCtor sortName = \case
  (Atom nam _ range) -> do
    sort <- lookupType (Just range) (runIdentity nam)
    subtype sort sortName
  (Functor nam ts _ range) -> do
    sorts <- mapM (\t -> ensureAtom t >>= lookupType (Just (rangeOf t))) ts
    let tpy = curryFunType (sorts ++ [sortName])
    typedAs (Just range) nam tpy
  (Eqq _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Equality terms cannot be used as constructors in syntax declarations"
  (Neq _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Inequality terms cannot be used as constructors in syntax declarations"
  (Transition _ _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Transition terms cannot be used as constructors in syntax declarations"
  (HaskellExpr _ _ range) -> throwErrorAt range $ InvalidConstructor "Haskell expressions cannot be used as constructors in syntax declarations"
  (TermValue _ _ range) -> throwErrorAt range $ InvalidConstructor "Values cannot be used as constructors in syntax declarations"
  (IncludedIn _ _ range) -> throwErrorAt range $ InvalidConstructor "Inclusion terms cannot be used as constructors in syntax declarations"
  (SetOfTerms _ _ range) -> throwErrorAt range $ InvalidConstructor "Set literals cannot be used as constructors in syntax declarations"
  (TermExpr _ range) -> throwErrorAt range $ InvalidConstructor "Expression cannot be used as constructors in the syntax declarations"
  (TermMap _ _ range)    -> throwErrorAt range $ InvalidConstructor "Map literals cannot be used as constructors in the syntax declarations"
  w@Wildcard {} -> throwErrorAt (rangeOf w) $ InvalidConstructor "Wildcards cannot be used as constructors in the syntax declarations"

  (TermHask v _ _) -> absurd v
  where
    ensureAtom (Atom nam _ _) = return (runIdentity nam)
    ensureAtom t = throwErrorAt (rangeOf t) $ NoNestingAt sortName

pass1VisitDecl :: MonadCheck m => Decl -> m ()
pass1VisitDecl (Syntax _ decls _) = mapM_ pass1VisitSyntaxDecl decls
  where
    -- Type aliases are skipped - they don't have constructors
    pass1VisitSyntaxDecl (TypeAliasDecl {}) = return ()
    pass1VisitSyntaxDecl (SyntaxDecl _vars tpy ctors range) = do
      -- Resolve type aliases before processing constructors
      resolvedTpy <- resolveTypeAlias (Just range) tpy
      case fromTypeCon resolvedTpy of
        Left err -> throwErrorAt range (SortNotDefined err)
        Right sortTyp -> mapM_ (pass1VisitCtor sortTyp) ctors
pass1VisitDecl (TransitionDecl nam (Sort from, _) (Sort to, _) range) = do
  -- transition State ~> State is equivalent to the syntax rule:
  -- ~> ::= ~>(state, state) from a typing perspective
  let tpy = curryFunType [Sort from, Sort to, Sort nam]
  typedAs (Just range) nam tpy
pass1VisitDecl (Rewrite (RewriteDecl nam _ _ _) _) =
  defineRewriteName nam
pass1VisitDecl _ = return ()

pass1 :: MonadCheck m => Program -> m ()
pass1 (Program decls _) = mapM_ pass1VisitDecl decls

-----------------------------------------
-- Pass 2: infer the types in the rewrite rules
-----------------------------------------

-- TODO: Also ensure that the variables used in the body of the rewrite rule
-- are actually defined in its head.

-- | Helper function for bidirectional type checking of binary equality terms
checkBinaryEqualityTerm :: MonadCheck m =>
  (TypedTerm -> TypedTerm -> Typ -> Range -> TypedTerm) ->
  PureTerm -> PureTerm -> Range -> m (TypedTerm, Typ)
checkBinaryEqualityTerm constructor term1 term2 range = do
  -- Try bidirectional type checking: try term1 first, if it succeeds use its type for term2
  result1 <- tryCheckTerm term1
  case result1 of
    Right (term1', t1') -> do
      (term2', t2') <- local (const (Just t1')) (checkTerm term2)
      generalType <- widenType range t1' t2'
      return (constructor term1' term2' generalType range, generalType)
    Left _ -> do
      -- If term1 fails, try term2 first and use its type for term1
      (term2', t2') <- checkTerm term2
      (term1', t1') <- local (const (Just t2')) (checkTerm term1)
      generalType <- widenType range t1' t2'
      return (constructor term1' term2' generalType range, generalType)
  where
    tryCheckTerm term = catchError (Right <$> checkTerm term) (return . Left)

-- | Check an individual term and returns its sort
checkTerm :: MonadCheck m => PureTerm -> m (TypedTerm, Typ)
checkTerm (Atom nam _ range) = do
  let varName = variableName (runIdentity nam)
  tpy <- lookupType (Just range) varName
  return (Atom nam tpy range, tpy)
checkTerm (Functor nam terms _ range) = do
  -- For funtors we lookup the type associated with its name,
  -- and then try to apply "applyTpy" to it to ensure that all arguments
  -- have the correct type.
  functorTpy <- lookupType (Just range) nam
  -- Obtain the type of the arguments and pass them to the `applyTpy` function
  (typedTerms, resultTpy) <- applyTpy (Just range) functorTpy (map checkTerm terms)
  return (Functor nam typedTerms resultTpy range, resultTpy)
checkTerm (Eqq term1 term2 _ range) =
  checkBinaryEqualityTerm Eqq term1 term2 range
checkTerm (Neq term1 term2 _ range) =
  checkBinaryEqualityTerm Neq term1 term2 range
checkTerm (Transition transitionName t1 t2 tpy range) = do
  -- transitions are registered in the typing context as sorts with a single data constructor
  -- NOTE: the assumption is that checkTerm always returns a functor of the same shape, hence the pattern match on the lines below never fails.
  term <- checkTerm (Functor transitionName [t1, t2] tpy range)
  case term of
    (Functor nam [term1', term2'] t range, sortname) ->
      return (Transition nam term1' term2' t range, sortname)
    _ -> error "checkTerm: transition should always return a functor with 2 arguments"
checkTerm (HaskellExpr expr _ range) =
  maybe (throwErrorAt range HaskellExprTypeInferenceError)
        (\typ -> return (HaskellExpr expr typ range, typ)) =<< ask
checkTerm (TermValue val _ range) =
  return (TermValue val (typeOf val) range, typeOf val)
checkTerm (IncludedIn var term range) = do
  -- Check the term on the right side
  (checkedTerm, termType) <- checkTerm term
  -- Ensure the term is a set type
  case termType of
    SetOf elementType -> do
      -- Ensure the variable is defined and has the right type
      varSort <- lookupType (Just range) var
      -- Check if the variable type is compatible with the set element type
      ifM (subtypeOf varSort elementType)
          (return (IncludedIn var checkedTerm range, VoidType)) -- Set inclusion has VoidType
          (throwErrorAt range (IncompatibleTypes [varSort] [elementType]))
    _ -> do
      -- Get the variable type to show what kind of set was expected
      varSort <- lookupType (Just range) var
      throwErrorAt range (IncompatibleTypes [SetOf varSort] [termType]) -- Expected a set of the variable's type
checkTerm (SetOfTerms terms _ range) = do
  -- Check all terms in the set and ensure they have compatible types
  checkedTerms <- mapM checkTerm (Set.toList terms)
  let (typedTerms, types) = unzip checkedTerms
  -- Find the most general type that encompasses all element types, VoidType for empty sets
  generalType <- foldM (widenType range) VoidType types
  let resultSet = Set.fromList typedTerms
  return (SetOfTerms resultSet (SetOf generalType) range, SetOf generalType)
checkTerm (TermExpr expr r) = do
  (exprTyped, tpy) <- checkExpr expr
  return (TermExpr exprTyped r, tpy)
checkTerm (TermMap mapping _ range) =
    liftA2 (,) (TermMap <$> checkMapping mapping <*> asks fromJust <*> pure range) (asks fromJust)
  where checkMapping =
          -- Ensure that all key and values match the expected parent type
          fmap Map.fromList . mapM (\(k, v) -> do
              (kt, ktpy) <- checkTerm k
              (vt, vtpy) <- checkTerm v
              parentType <- asks fromJust
              ifM (subtypeOf (MapOf ktpy vtpy) parentType)
                  (return (kt, vt))
                  (throwErrorAt range (IncompatibleTypes [MapOf vtpy ktpy] [parentType]))
            ) . Map.toList
checkTerm (Wildcard _ range) =
  maybe (throwErrorAt range HaskellExprTypeInferenceError)
        (\typ -> return (Wildcard typ range, typ)) =<< ask
  
checkTerm (TermHask v _ _) = absurd v

checkTerm_ :: MonadCheck m => PureTerm -> m Typ
checkTerm_ = fmap snd . checkTerm

checkExpr :: MonadCheck m => PureExpr -> m (Expr TypingPhase Identity, Typ)
checkExpr (LookupMap mapTerm keyTerm _ range) = do
  (mapTerm', mapType) <- checkTerm mapTerm
  (keyTerm', keyType) <- checkTerm keyTerm
  case mapType of
    MapOf kType vType -> do
      -- Ensure the key type is compatible with the map's key type
      ifM (subtypeOf keyType kType)
          (return (LookupMap mapTerm' keyTerm' vType range, vType))
          (throwErrorAt range (IncompatibleTypes [keyType] [kType]))
    _ -> throwErrorAt range (IncompatibleTypes [MapOf keyType AnyType] [mapType]) -- Expected a map type
checkExpr (UpdateMap mapTerm keyTerm valueTerm _ range) = do
  (mapTerm', mapType) <- checkExpr mapTerm
  (keyTerm', keyType) <- checkTerm keyTerm
  (valueTerm', valueType) <- checkTerm valueTerm
  ifM (subtypeOf (MapOf keyType valueType) mapType)
      (return (UpdateMap mapTerm' keyTerm' valueTerm' mapType range, mapType))
      (throwErrorAt range (IncompatibleTypes [MapOf keyType valueType] [mapType]))
checkExpr (GroundTerm term _ range) = do
  (typedTerm, tpy) <- checkTerm term
  return (GroundTerm typedTerm tpy range, tpy)
checkExpr (EmptyMap _ range) =
  liftA2 (,) (asks (flip EmptyMap range . fromJust)) (asks fromJust)
checkExpr (SetUnion t1 t2 _ range) = do
  (t1', tpy1) <- checkTerm t1
  (t2', tpy2) <- checkTerm t2
  -- TODO: use subtyping and widen the type of the unioned set to the more
  -- general one.
  when (tpy1 /= tpy2)
    (throwErrorAt range (IncompatibleTypes [tpy1] [tpy2]))
  return (SetUnion t1' t2' tpy1 range, tpy1)
  
checkExpr _ = error "checkExpr: only LookupMap, UpdateMap and GroundTerm are supported in expressions"

-- | Update or widen the sort name for a rewrite rule
updateOrWidenSort :: MonadCheck m => String -> Typ -> Range -> m ()
updateOrWidenSort nam newSort range = ifM (hasType nam)
    (do existingSort <- lookupType (Just range) nam
        widenedSort  <- widenType range existingSort newSort
        typedAsUpdated nam widenedSort)
    (typedAs (Just range) nam newSort)

pass2VisitDecl :: MonadCheck m => Decl -> m ()
pass2VisitDecl (Rewrite (RewriteDecl nam args bdy range) _) = do
  tpys <- mapM checkTerm_ args
  bdyTpy <- checkTerm_ bdy
  let rewriteTpy = curryFunType (tpys ++ [bdyTpy])
  -- Update or widen the constructor for the rewrite rule head
  updateOrWidenSort nam rewriteTpy range
pass2VisitDecl _ = return ()

pass2 :: MonadCheck m => Program -> m ()
pass2 (Program decls _) = mapM_ pass2VisitDecl decls

-----------------------------------------
-- Pass 3: type check the terms in the rules
-----------------------------------------

-- | Check a rule in the given context
checkRule :: MonadCheck m => RuleDecl -> m TypedRuleDecl
checkRule (RuleDecl nam precedent consequent range) = RuleDecl nam <$> mapM (fmap fst . checkTerm) precedent <*> mapM (fmap fst . checkTerm) consequent <*> pure range

-- | Type a rewrite rule
typeRewrite :: MonadCheck m => RewriteDecl -> m (TypedRewriteDecl Identity)
typeRewrite (RewriteDecl name args body range) = do
  typedArgs <- mapM (fmap fst . checkTerm) args
  typedBody <- fmap fst (checkTerm body)
  return $ RewriteDecl name typedArgs typedBody range

typeSyntax :: MonadCheck m => SyntaxDecl -> m (Maybe TypedSyntaxDecl)
typeSyntax (TypeAliasDecl {}) = return Nothing  -- Type aliases are eliminated from typed AST
typeSyntax (SyntaxDecl vars tpy prods range) = do
  -- Resolve type aliases in the type constructor
  resolvedTpy <- resolveTypeAlias (Just range) tpy
  typedProds <- mapM (fmap fst . checkTerm) prods
  return $ Just $ SyntaxDecl vars resolvedTpy typedProds range

pass3VisitDecl :: MonadCheck m => Decl -> m TypedDecl
pass3VisitDecl (RulesDecl name rules range) =
  RulesDecl name <$> mapM checkRule rules <*> pure range
pass3VisitDecl (TransitionDecl nam s1@(Sort {}, _) s2@(Sort {}, _) range) = return (TransitionDecl nam s1 s2 range)
pass3VisitDecl (TransitionDecl _ (_, r1) _ _) =
  throwErrorAt r1 $ NameNotDefined "Only Sort types are supported in transition declarations"
pass3VisitDecl (Rewrite rewrite range) =
  Rewrite <$> typeRewrite rewrite <*> pure range
pass3VisitDecl (Syntax name syntax range) =
  Syntax name <$> (catMaybes <$> mapM typeSyntax syntax) <*> pure range
pass3VisitDecl (HaskellDecl s isPre range) =
  return $ HaskellDecl s isPre range
pass3VisitDecl (Import filename range) =
  return $ Import filename range

pass3 :: MonadCheck m => Program -> m TypedProgram
pass3 (Program decls comments) = Program <$> mapM pass3VisitDecl decls <*> pure (map typeComment comments)

-----------------------------------------
-- Pass 4: type specialisations
-----------------------------------------

pass4VisitDecl :: MonadCheck m => TypedDecl -> m TypedDecl
pass4VisitDecl (RulesDecl name rules range) =
  RulesDecl name <$> mapM pass4VisitRule rules <*> pure range
pass4VisitDecl (TransitionDecl nam s1 s2 range) =
  return (TransitionDecl nam s1 s2 range)
pass4VisitDecl (Rewrite rewrite range) =
  Rewrite <$> pass4VisitRewrite rewrite <*> pure range
pass4VisitDecl (Syntax name syntax range) =
  -- NOTE: syntax definitions never contain any MapOf expressions
  -- as they are not allowed by the previous passes.
  return $ Syntax name syntax range
pass4VisitDecl (HaskellDecl s isPre range) =
  return $ HaskellDecl s isPre range
pass4VisitDecl (Import filename range) =
  return $ Import filename range

pass4VisitRule :: MonadCheck m => TypedRuleDecl -> m TypedRuleDecl
pass4VisitRule (RuleDecl nam precedent consequent range) =
  RuleDecl nam <$> mapM pass4VisitTerm precedent <*> mapM pass4VisitTerm consequent <*> pure range

pass4VisitRewrite :: MonadCheck m => TypedRewriteDecl Identity -> m (TypedRewriteDecl Identity)
pass4VisitRewrite (RewriteDecl name args body range) =
  RewriteDecl name <$> mapM pass4VisitTerm args <*> pass4VisitTerm body <*> pure range

pass4VisitTerm :: MonadCheck m => TypedTerm -> m TypedTerm
pass4VisitTerm term@(Atom {}) = return term
pass4VisitTerm (Functor nam terms functorTpy range) = do
  typedTerms <- mapM pass4VisitTerm terms
  tpy <- lookupType (Just range) nam
  -- Check if this functor represents a map lookup or rewrite rule application
  case tpy of
    MapOf _ resultType -> do
      -- Transform to map lookup expression
      case typedTerms of
        [keyTerm] -> return $ TermExpr (LookupMap (Atom (Identity nam) tpy range) keyTerm resultType range) range
        _ -> error $ "MapOf type should always have exactly 1 argument, got " ++ show (length typedTerms)
    _ ->
      ifM (isRewriteRule nam)
      (return (TermExpr (RewriteApp nam typedTerms functorTpy range) range))
      (return (Functor nam typedTerms functorTpy range))
pass4VisitTerm (Eqq term1 term2 tpy range) =
  Eqq <$> pass4VisitTerm term1 <*> pass4VisitTerm term2 <*> pure tpy <*> pure range
pass4VisitTerm (Neq term1 term2 tpy range) =
  Neq <$> pass4VisitTerm term1 <*> pass4VisitTerm term2 <*> pure tpy <*> pure range
pass4VisitTerm (Transition nam term1 term2 tpy range) =
  Transition nam <$> pass4VisitTerm term1 <*> pass4VisitTerm term2 <*> pure tpy <*> pure range
pass4VisitTerm term@(HaskellExpr {}) = return term
pass4VisitTerm term@(TermValue {}) = return term
pass4VisitTerm (IncludedIn var term range) =
  IncludedIn var <$> pass4VisitTerm term <*> pure range
pass4VisitTerm (SetOfTerms terms tpy range) =
  (SetOfTerms . Set.fromList <$> mapM pass4VisitTerm (Set.toList terms)) <*> pure tpy <*> pure range
pass4VisitTerm (TermHask v _ _) = absurd v
pass4VisitTerm (TermExpr expr range) =
  TermExpr <$> pass4VisitExpr expr <*> pure range
pass4VisitTerm (TermMap mapping tpy r) =
  (TermMap . Map.fromList <$> mapM (\(k, v) -> (,) <$> pass4VisitTerm k <*> pass4VisitTerm v) (Map.toList mapping)) <*> pure tpy <*> pure r
pass4VisitTerm w@(Wildcard {}) = return w

pass4VisitExpr :: MonadCheck m => Expr TypingPhase Identity -> m (Expr TypingPhase Identity)
pass4VisitExpr (LookupMap mapTerm keyTerm tpy range) =
  LookupMap <$> pass4VisitTerm mapTerm <*> pass4VisitTerm keyTerm <*> pure tpy <*> pure range
pass4VisitExpr (UpdateMap mapTerm keyTerm valTerm tpy range) =
  UpdateMap <$> pass4VisitExpr mapTerm <*> pass4VisitTerm keyTerm <*> pass4VisitTerm valTerm <*> pure tpy <*> pure range
pass4VisitExpr (RewriteApp nam args tpy range) =
  RewriteApp nam <$> mapM pass4VisitTerm args <*> pure tpy <*> pure range
pass4VisitExpr e@(EmptyMap {}) = return e
pass4VisitExpr (GroundTerm t tpy range) =
  GroundTerm <$> pass4VisitTerm t <*> pure tpy <*> pure range
pass4VisitExpr (SetUnion s1 s2 tpy range) =
  SetUnion <$> pass4VisitTerm s1 <*> pass4VisitTerm s2 <*> pure tpy <*> pure range

-- Some terms have to be rewritten based on their types.
-- For instance, a lookup 'mapping(key)' looks like a functor term but based on its
-- type it actually is a lookup into a mapping.
--
-- A functor could similarly also represent an application of a rewrite rule.
pass4 :: MonadCheck m => TypedProgram -> m TypedProgram
pass4 (Program decls comments) = Program <$>  mapM pass4VisitDecl decls <*> pure comments

-----------------------------------------
-- Entrypoint
-----------------------------------------

runChecker :: Program -> Either Error CheckingContext
runChecker = fmap fst . runChecker'

-- | Run the type checker in its entirety
runChecker' :: Program -> Either Error (CheckingContext, TypedProgram)
runChecker' = runCheckerWithContext emptyCheckingContext

runCheckerWithContext :: CheckingContext -> Program -> Either Error (CheckingContext, TypedProgram)
runCheckerWithContext initialCtx program =
    swap <$> runStateT (runReaderT (typeAliasPass program >> pass0 program >> pass1 program >> pass2 program >> pass3 program >>= pass4) Nothing) initialCtx

-- | Type check a single term in the given context
runCheckTerm :: CheckingContext -> PureTerm -> Either Error TypedTerm
runCheckTerm ctx term =
  fst <$> evalStateT (runReaderT (checkTerm term) Nothing) ctx
