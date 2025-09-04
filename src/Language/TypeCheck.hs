{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.TypeCheck where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import Control.Lens hiding (Context)
import Language.AST
import qualified Data.Graph as Graph
import Data.Graph (UnlabeledGraph)
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Extra
import Language.Types
import Data.Tuple

-----------------------------------------
-- Errors
-----------------------------------------

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
-- Data types
-----------------------------------------


-- | Variables
type Var = String

-- | A data constructor is either an atom or a tag paired with a list of sorts
data DataCtor = DataAtom Typ | DataTagged String [Typ]
              deriving (Ord, Eq, Show)

-- | Typeclass for extracting sorts from data types
class HasSorts s where
  getSorts :: s -> [Typ]

instance HasSorts Typ where
  getSorts s = [s]

instance HasSorts DataCtor where
  getSorts (DataAtom s) = [s]
  getSorts (DataTagged _ ss) = ss


-----------------------------------------
-- Environment
-----------------------------------------

-- | An environment is a mapping from variables and functors to their data constructors
newtype Environment = Environment { getEnvironment :: Map String DataCtor }
                    deriving (Ord, Eq, Show)


lookupEnvironment :: String -> Environment -> Maybe DataCtor
lookupEnvironment k = Map.lookup k . getEnvironment

-- | Extend the environment with the given functor name and data constructor
extendEnvironment :: String -> DataCtor -> Environment -> Environment
extendEnvironment k d = Environment . Map.insert k d . getEnvironment

-- | Create an empty environment
emptyEnvironment :: Environment
emptyEnvironment = Environment Map.empty

-----------------------------------------
-- Typing context
-----------------------------------------

-- | A typing context maps atoms and functors to their sorts
newtype Gamma = Gamma { getGamma :: Map String Typ }
              deriving (Ord, Eq, Show)

-- | Lookup the type of the given functor or variable in the typing contex
lookupGamma :: String -> Gamma -> Maybe Typ
lookupGamma k = Map.lookup k . getGamma

-- | Associate the given type with the given variable or atom
typesAs :: String -> Typ -> Gamma -> Gamma
typesAs k s =
  Gamma . Map.insert k s . getGamma

-- | Create an empty typing context
emptyTypingContext :: Gamma
emptyTypingContext = Gamma Map.empty

-----------------------------------------
-- Subtyping relation
-----------------------------------------

-- | Keeps track of subtyping information
newtype Subtyping = Subtyping { subtypingSupertypes :: UnlabeledGraph Typ }
                  deriving (Ord, Eq, Show)

-- | Registers the first argument as a subtype of the second
makeSubtypeOf :: Typ -> Typ -> Subtyping -> Subtyping
makeSubtypeOf from to = Subtyping . Graph.addEdge () from to . subtypingSupertypes

-- | Checks whether the first argument is a subtype of the second
isSubtypeOf :: Typ -> Typ -> Subtyping -> Bool
isSubtypeOf from to = Graph.isReachable from to . subtypingSupertypes

-- | Create an empty subtyping graph
emptySubtyping :: Subtyping
emptySubtyping = Subtyping Graph.empty

-----------------------------------------
-- Full checking context
-----------------------------------------

data CheckingContext = CheckingContext {
                        _environment :: Environment
                      , _typingContext :: Gamma
                      , _subtypingContext :: Subtyping
                      , _definedSorts :: Set Typ
                      , _sortToDefSite :: Map String Range
                      , _typeNameMapping :: Map String Typ  -- ^ maps type names to their Typ representation
                      , _typeCheckingErrors :: [Error]
                     } deriving (Ord, Eq, Show)


$(makeLenses ''CheckingContext)

emptyCheckingContext :: CheckingContext
emptyCheckingContext = CheckingContext emptyEnvironment emptyTypingContext emptySubtyping Set.empty Map.empty builtinTypeMapping []
  where
    builtinTypeMapping = Map.fromList [("Int", IntType), ("String", StrType), ("Any", AnyType)]

-----------------------------------------
-- Monadic context
-----------------------------------------

type MonadCheck m = (MonadState CheckingContext m, MonadError Error m, MonadReader (Maybe Typ) m)

-------------------------
-- Monadic error handling
-------------------------

-- | Registers an error boundary where all the thrown errors are catched and added
-- to the context instead of being propagated to the caller
boundary :: MonadCheck m => m () -> m ()
boundary = (`catchError` (\e -> modify (over typeCheckingErrors (e:))))

-- | Throw an error with an optional range
throwErrorMaybe :: MonadCheck m => Maybe Range -> ModelError -> m a
throwErrorMaybe range err = get >>= Except.throwError . Error err range

-- | Throw an error by adding the current context to it
throwError :: MonadCheck m => ModelError -> m a
throwError = throwErrorMaybe Nothing

-- | Throw an error at the given location in the sourcez by adding the current context to it 
throwErrorAt :: MonadCheck m => Range -> ModelError -> m a
throwErrorAt range = throwErrorMaybe (Just range)

maybeOrError :: MonadCheck m => Maybe Range -> ModelError -> Maybe a -> m a
maybeOrError range err = maybe (throwErrorMaybe range err) return

---------------------------
-- Context ineractions
---------------------------

-- | Find the associated data constructor of the given variable or functor,
-- raises an error if the variable or functor is not defined.
lookupCtor :: MonadCheck m => Maybe Range -> String -> m DataCtor
lookupCtor maybeRange nam =
  gets (lookupEnvironment nam . _environment) >>= maybeOrError maybeRange (NameNotDefined nam)

-- | Looks up the sort associated with the variable and raises an error if the sort does not exist
lookupSort :: MonadCheck m => Maybe Range -> String -> m Typ
lookupSort maybeRange nam =
  gets (lookupGamma nam . _typingContext) >>= maybeOrError maybeRange (SortNotDefined nam)

-- | Check if a sort is defined
isSortDefined :: MonadCheck m => Typ -> m Bool
isSortDefined sortName = gets (Set.member sortName . _definedSorts)

-- | Add a sort to the set of defined sorts
defineSort :: MonadCheck m => Typ -> m ()
defineSort sortName = do
  modify (over definedSorts (Set.insert sortName))
  -- all sorts is a subtype of 'Any'
  subtyped sortName AnyType

-- | Define a sort by name, registering both the type mapping and adding it to defined sorts
defineSortByName :: MonadCheck m => String -> m ()
defineSortByName typeName = do
  typ <- resolveTypeName typeName
  registerTypeName typeName typ
  defineSort typ

-- | Record the definition site of a sort
recordSortDefSite :: MonadCheck m => String -> Range -> m ()
recordSortDefSite sortName range = modify (over sortToDefSite (Map.insert sortName range))

-- | Register a type name with its corresponding Typ
registerTypeName :: MonadCheck m => String -> Typ -> m ()
registerTypeName typeName typ = modify (over typeNameMapping (Map.insert typeName typ))

-- | Resolve a type name to its Typ representation
resolveTypeName :: MonadCheck m => String -> m Typ
resolveTypeName typeName = do
  mapping <- gets _typeNameMapping
  maybe (throwError $ NameNotDefined typeName) return (Map.lookup typeName mapping)

-- | Associate the given type with the given variable or atom in the typing context
-- and add the sort to the set of defined sorts
typedAs :: MonadCheck m => String -> Typ -> m ()
typedAs var sortName = do
  modify (over typingContext (typesAs var sortName))
  defineSort sortName

-- | Registers the first argument as a subtype of the second
subtyped :: MonadCheck m => Typ -> Typ -> m ()
subtyped subtype parenttype =
  modify (over subtypingContext (makeSubtypeOf subtype parenttype))

-----------------------------------------
-- Subtyping relations
-----------------------------------------

class SubtypeOf s where
  subtypeOf :: MonadCheck m => s -> s -> m Bool

instance SubtypeOf DataCtor where
  subtypeOf (DataAtom nam1) (DataAtom nam2) = nam1 `subtypeOf` nam2
  subtypeOf (DataTagged nam1 s1) (DataTagged nam2 s2) =
    (nam1 == nam2 &&) <$> s1 `subtypeOf` s2
  subtypeOf _ _ = return False

instance SubtypeOf s => SubtypeOf [s] where
  subtypeOf l1 l2
    | length l1 /= length l2 = return False
    | otherwise = and <$> zipWithM subtypeOf l1 l2

instance SubtypeOf Typ where
  subtypeOf VoidType _ = return False  -- VoidType is never a subtype of anything
  subtypeOf _ VoidType = return False  -- Nothing is ever a subtype of VoidType
  subtypeOf s1 s2 = do
    subCtx <- gets _subtypingContext
    return $ isSubtypeOf s1 s2 subCtx

-- | Return a list of sorts associated with the functor
sortsInFunctor :: MonadCheck m => String -> m [Typ]
sortsInFunctor functorName = do
  env <- gets _environment
  case lookupEnvironment functorName env of
    Nothing -> throwError (NameNotDefined functorName)
    Just (DataAtom sortName) -> return [sortName]
    Just (DataTagged _ sorts) -> return sorts

-----------------------------------------
-- Pass 0: declare all sorts and variable names
-----------------------------------------

-- | Associate a single variable to the given type or return
-- an error if there is already a type associated with the given variable.
-- Also associates the atom with the specified constructor in the environment.
typeAsUnique :: MonadCheck m => DataCtor -> Typ -> Var -> m ()
typeAsUnique ctor sortName var = do
  env <- gets _environment
  case lookupEnvironment var env of
    Just _ -> throwError (DuplicateVariable var sortName)
    Nothing -> do
      modify (over environment (extendEnvironment var ctor))
      typedAs var sortName

pass0VisitDecl :: MonadCheck m => Decl -> m ()
pass0VisitDecl (Syntax decls _) = mapM_ pass0VisitSyntaxDecl decls
  where
    -- no productions denotes a reference to an existing type, this means that:
    -- * variables are associated with types
    -- * more relaxed set of types allowed (i.e., type constructors of arity 0)
    -- * checks whether type is actually valid
    pass0VisitSyntaxDecl (SyntaxDecl vars tpy [] range) = do
      case fromTypeCtor tpy of
        Left err -> throwErrorAt range (SortNotDefined err)
        Right sortTyp -> do
          -- Add the type to the environment if not already defined
          ifM (isSortDefined sortTyp)
              (return ())
              (defineSort sortTyp)
          -- Type all variables as unique with this type
          mapM_ (typeAsUnique (DataAtom sortTyp) sortTyp) vars
    -- one or more productions denotes a declaration of a type, the checker will:
    -- * associate the variables with types
    -- * declare the type
    -- * ensure the type is not duplicate
    -- * more strict type constructors (only arity 0) 
    pass0VisitSyntaxDecl (SyntaxDecl vars tpy _ctors range) = do
      case fromTypeCtor tpy of
        Left err -> throwErrorAt range (SortNotDefined err)
        Right sortTyp -> do
          ifM (isSortDefined sortTyp)
              (throwError (DuplicateSort sortTyp))
              (do recordSortDefSite (ctorName tpy) range
                  registerTypeName (ctorName tpy) sortTyp
                  mapM_ (typeAsUnique (DataAtom sortTyp) sortTyp) vars)
pass0VisitDecl (TransitionDecl nam _from _to range) = do
  recordSortDefSite nam range
  -- Register the transition name as its own type
  let transitionTyp = Sort nam
  registerTypeName nam transitionTyp
pass0VisitDecl _ = return ()

pass0 :: MonadCheck m => Program -> m ()
pass0 (Program decls _) = mapM_ pass0VisitDecl decls

-----------------------------------------
-- Pass 1: associate functors in the syntax declarations with sorts, register subtyping for variables
-----------------------------------------

-- | Process and associate the data constructors in the given sort
pass1VisitCtor :: MonadCheck m => Typ -> PureTerm -> m ()
pass1VisitCtor sortName = \case
  (Atom nam _ range) -> do
    sort <- lookupSort (Just range) (runIdentity nam)
    subtyped sort sortName
  (Functor nam ts _ _range) -> do
    sorts <- mapM (\t -> collectAtoms t >>= lookupSort (Just (rangeOf t))) ts
    let ctor = DataTagged nam sorts
    typeAsUnique ctor sortName nam
  (Eqq _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Equality terms cannot be used as constructors in syntax declarations"
  (Neq _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Inequality terms cannot be used as constructors in syntax declarations"
  (Transition _ _ _ _ range) -> throwErrorAt range $ InvalidConstructor "Transition terms cannot be used as constructors in syntax declarations"
  (HaskellExpr _ _ range) -> throwErrorAt range $ InvalidConstructor "Haskell expressions cannot be used as constructors in syntax declarations"
  (TermValue _ _ range) -> throwErrorAt range $ InvalidConstructor "Values cannot be used as constructors in syntax declarations"
  (IncludedIn _ _ range) -> throwErrorAt range $ InvalidConstructor "Inclusion terms cannot be used as constructors in syntax declarations"
  (SetOfTerms _ _ range) -> throwErrorAt range $ InvalidConstructor "Set literals cannot be used as constructors in syntax declarations"
  where
    collectAtoms (Atom nam _ _) = return (runIdentity nam)
    collectAtoms t = throwErrorAt (rangeOf t) $ NoNestingAt sortName

pass1VisitDecl :: MonadCheck m => Decl -> m ()
pass1VisitDecl (Syntax decls _) = mapM_ pass1VisitSyntaxDecl decls
  where
    pass1VisitSyntaxDecl (SyntaxDecl _vars tpy ctors range) = do
      case fromTypeCtor tpy of
        Left err -> throwErrorAt range (SortNotDefined err)
        Right sortTyp -> mapM_ (pass1VisitCtor sortTyp) ctors
pass1VisitDecl (TransitionDecl nam (Sort from, _) (Sort to, _) _range) = do
  -- transition State ~> State is equivalent to the syntax rule:
  -- ~> ::= ~>(state, state) from a typing perspective
  fromTyp <- resolveTypeName from
  toTyp <- resolveTypeName to
  transitionTyp <- resolveTypeName nam
  let ctor = DataTagged nam [fromTyp, toTyp]
  typeAsUnique ctor transitionTyp nam
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
      if t1' /= t2'
        then throwErrorAt range (IncompatibleTypes [t1'] [t2'])
        else return (constructor term1' term2' t1' range, t1')
    Left _ -> do
      -- If term1 fails, try term2 first and use its type for term1
      (term2', t2') <- checkTerm term2
      (term1', t1') <- local (const (Just t2')) (checkTerm term1)
      if t1' /= t2'
        then throwErrorAt range (IncompatibleTypes [t1'] [t2'])
        else return (constructor term1' term2' t1' range, t1')
  where
    tryCheckTerm term = catchError (Right <$> checkTerm term) (return . Left)

-- | Check an individual term and returns its sort
checkTerm :: MonadCheck m => PureTerm -> m (TypedTerm, Typ)
checkTerm (Atom nam _ range) = do
  let varName = variableName (runIdentity nam)
  -- Check that the atom is not associated with a functor (DataTagged constructor)
  ctor <- lookupCtor (Just range) varName
  case ctor of
    DataTagged _ _ -> throwErrorAt range (NameNotDefined varName) -- Should be used as functor, not atom
    DataAtom _ -> do
      sort <- lookupSort (Just range) varName
      return (Atom nam sort range, sort)
checkTerm (Functor tpy terms _ range) = do
  -- Check if functor is defined and get its arity from the constructor
  ctor <- lookupCtor (Just range) tpy
  let expected = getSorts ctor
  functorSort <- lookupSort (Just range) tpy
  -- Check arity matches
  if length terms /= length expected
    then throwErrorAt range $ ArityMismatch tpy (length expected) (length terms)
    else do
      -- Type check each argument with its expected type in the reader context
      let expectedTypes = map Just expected
      (terms', ts') <- mapAndUnzipM (\(term, expectedType) ->
        local (const expectedType) (checkTerm term)) (zip terms expectedTypes)
      ok <- and <$> zipWithM subtypeOf ts' expected
      if ok
        then return (Functor tpy terms' functorSort range, functorSort)
        else throwErrorAt range $ IncompatibleTypes expected ts'
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
      varSort <- lookupSort (Just range) var
      -- Check if the variable type is compatible with the set element type
      ifM (subtypeOf varSort elementType)
          (return (IncludedIn var checkedTerm range, VoidType)) -- Set inclusion has VoidType
          (throwErrorAt range (IncompatibleTypes [varSort] [elementType]))
    _ -> do
      -- Get the variable type to show what kind of set was expected
      varSort <- lookupSort (Just range) var
      throwErrorAt range (IncompatibleTypes [SetOf varSort] [termType]) -- Expected a set of the variable's type
checkTerm (SetOfTerms terms _ range) = do
  -- Check all terms in the set and ensure they have compatible types
  checkedTerms <- mapM checkTerm (Set.toList terms)
  let (typedTerms, types) = unzip checkedTerms
  -- Find the most general type that encompasses all element types, VoidType for empty sets
  generalType <- foldM (widenType range) VoidType types
  let resultSet = Set.fromList typedTerms
  return (SetOfTerms resultSet (SetOf generalType) range, SetOf generalType)

checkTerm_ :: MonadCheck m => PureTerm -> m Typ
checkTerm_ = fmap snd . checkTerm

-- | Generic widening function that works with any type that has SubtypeOf and HasSorts instances
widenType :: (SubtypeOf s, HasSorts s, MonadCheck m) => Range -> s -> s -> m s
widenType range type1 type2 = do
  isSubset1 <- subtypeOf type1 type2
  isSubset2 <- subtypeOf type2 type1
  if isSubset1
    then return type2      -- type2 is more general
    else if isSubset2 then return type1 -- type1 is more general
    else throwErrorAt range (IncompatibleTypes (getSorts type1) (getSorts type2))

-- | Update or widen the constructor for a rewrite rule head
updateOrWidenCtor :: MonadCheck m => String -> DataCtor -> Range -> m ()
updateOrWidenCtor nam newCtor range = do
  env <- gets _environment
  case lookupEnvironment nam env of
    Nothing -> modify (over environment (extendEnvironment nam newCtor))
    Just existingCtor -> do
      widenedCtor <- widenType range existingCtor newCtor
      modify (over environment (extendEnvironment nam widenedCtor))

-- | Update or widen the sort name for a rewrite rule
updateOrWidenSort :: MonadCheck m => String -> Typ -> Range -> m ()
updateOrWidenSort nam newSort range = do
  gamma <- gets _typingContext
  case lookupGamma nam gamma of
    Nothing -> typedAs nam newSort
    Just existingSort -> do
      widenedSort <- widenType range existingSort newSort
      typedAs nam widenedSort

pass2VisitDecl :: MonadCheck m => Decl -> m ()
pass2VisitDecl (Rewrite (RewriteDecl nam args bdy range) _) = do
  sorts <- mapM checkTerm_ args
  bdySort <- checkTerm_ bdy
  let ctor = DataTagged nam sorts
  -- Update or widen the constructor for the rewrite rule head
  updateOrWidenCtor nam ctor range
  -- Update or widen the sort for the rewrite rule itself (body sort)
  updateOrWidenSort nam bdySort (rangeOf bdy)
pass2VisitDecl _ = return ()

pass2 :: MonadCheck m => Program -> m ()
pass2 (Program decls _) = mapM_ pass2VisitDecl decls

-----------------------------------------
-- Pass 3: type check the terms in the rules
-----------------------------------------

-- | Check that the given sort exists at a specific location
assertSortDefinedAt :: MonadCheck m => Range -> String -> m ()
assertSortDefinedAt range sortName = do
  sortTyp <- resolveTypeName sortName
  ifM (isSortDefined sortTyp)
      (return ())
      (throwErrorAt range (SortNotDefined sortName))

-- | Check a rule in the given context
checkRule :: MonadCheck m => RuleDecl -> m TypedRuleDecl
checkRule (RuleDecl nam precedent consequent range) = do
  RuleDecl nam <$> mapM (fmap fst . checkTerm) precedent <*> mapM (fmap fst . checkTerm) consequent <*> pure range

-- | Type a rewrite rule
typeRewrite :: MonadCheck m => RewriteDecl -> m TypedRewriteDecl
typeRewrite (RewriteDecl name args body range) = do
  -- TODO: annotate with the correct type instead of AnyType
  typedArgs <- mapM (fmap fst . checkTerm) args
  typedBody <- fmap fst (checkTerm body)
  return $ RewriteDecl name typedArgs typedBody range

typeSyntax :: MonadCheck m => SyntaxDecl -> m TypedSyntaxDecl
typeSyntax (SyntaxDecl vars tpy prods range) = do
  -- TODO: annotate with the correct type instead of AnyType
  typedProds <- mapM (fmap fst . checkTerm) prods
  return $ SyntaxDecl vars tpy typedProds range

pass3VisitDecl :: MonadCheck m => Decl -> m TypedDecl
pass3VisitDecl (RulesDecl rules range) =
  RulesDecl <$> mapM checkRule rules <*> pure range
pass3VisitDecl (TransitionDecl nam s1@(Sort fromSort, r1) s2@(Sort toSort, r2) range) = do
  assertSortDefinedAt r1 fromSort
  assertSortDefinedAt r2 toSort
  return (TransitionDecl nam s1 s2 range)
pass3VisitDecl (TransitionDecl _ (_, r1) _ _) = 
  throwErrorAt r1 $ NameNotDefined "Only Sort types are supported in transition declarations"
pass3VisitDecl (Rewrite rewrite range) =
  Rewrite <$> typeRewrite rewrite <*> pure range
pass3VisitDecl (Syntax syntax range) =
  Syntax <$> mapM typeSyntax syntax <*> pure range
pass3VisitDecl (HaskellDecl s range) =
  return $ HaskellDecl s range

pass3 :: MonadCheck m => Program -> m TypedProgram
pass3 (Program decls comments) = Program <$> mapM pass3VisitDecl decls <*> pure (map typeComment comments)

-----------------------------------------
-- Entrypoint
-----------------------------------------

runChecker :: Program -> Either Error CheckingContext
runChecker = fmap fst . runChecker'

-- | Run the type checker in its entirety
runChecker' :: Program -> Either Error (CheckingContext, TypedProgram)
runChecker' program = do
  let initialContext = emptyCheckingContext
  swap <$> runStateT (runReaderT (pass0 program >> pass1 program >> pass2 program >> pass3 program) Nothing) initialContext

-- | Type check a single term in the given context
runCheckTerm :: CheckingContext -> PureTerm -> Either Error TypedTerm
runCheckTerm ctx term =
  fst <$> evalStateT (runReaderT (checkTerm term) Nothing) ctx


