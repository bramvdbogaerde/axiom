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
import Data.Functor.Identity
import qualified Data.Graph as Graph
import Data.Graph (Graph, UnlabeledGraph)
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader
import Control.Monad
import Data.Kind
import Data.Maybe (fromMaybe)
import Control.Monad.Extra

-----------------------------------------
-- Errors
-----------------------------------------

data ModelError = DuplicateVariable String SortName
                | DuplicateSort SortName
                | NoNestingAt SortName
                | SortNotDefined String
                | IncompatibleTypes [SortName] [SortName]
                | NameNotDefined String
                deriving (Ord, Eq, Show)

data Error = Error { err :: ModelError, raisedAt :: Maybe Range,  ctx :: CheckingContext }
           deriving (Ord, Eq, Show)

-----------------------------------------
-- Data types
-----------------------------------------

-- | Names to refer to sorts
newtype SortName = SortName { getSortName :: String } deriving (Ord, Eq, Show)

-- | Variables
type Var = String

-- | A data constructor is either an atom or a tag paired with a list of sorts
data DataCtor = DataAtom SortName | DataTagged String [SortName]
              deriving (Ord, Eq, Show)

-- | Typeclass for extracting sorts from data types
class HasSorts s where
  getSorts :: s -> [SortName]

instance HasSorts SortName where
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
newtype Gamma = Gamma { getGamma :: Map String SortName }
              deriving (Ord, Eq, Show)

-- | Lookup the type of the given functor or variable in the typing contex
lookupGamma :: String -> Gamma -> Maybe SortName
lookupGamma k = Map.lookup k . getGamma

-- | Associate the given type with the given variable or atom
typesAs :: String -> SortName -> Gamma -> Gamma
typesAs k s =
  Gamma . Map.insert k s . getGamma

-- | Create an empty typing context
emptyTypingContext :: Gamma
emptyTypingContext = Gamma Map.empty

-----------------------------------------
-- Subtyping relation
-----------------------------------------

-- | Keeps track of subtyping information
newtype Subtyping = Subtyping { subtypingSupertypes :: UnlabeledGraph SortName }
                  deriving (Ord, Eq, Show)

-- | Registers the first argument as a subtype of the second
makeSubtypeOf :: SortName -> SortName -> Subtyping -> Subtyping
makeSubtypeOf from to = Subtyping . Graph.addEdge () from to . subtypingSupertypes 

-- | Checks whether the first argument is a subtype of the second
isSubtypeOf :: SortName -> SortName -> Subtyping -> Bool
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
                      , _definedSorts :: Set SortName
                      , _sortToDefSite :: Map String Range
                      , _typeCheckingErrors :: [Error]
                     } deriving (Ord, Eq, Show)


$(makeLenses ''CheckingContext)

emptyCheckingContext :: CheckingContext
emptyCheckingContext = CheckingContext emptyEnvironment emptyTypingContext emptySubtyping Set.empty Map.empty []

-----------------------------------------
-- Monadic context
-----------------------------------------

type MonadCheck m = (MonadState CheckingContext m, MonadError Error m)

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
lookupSort :: MonadCheck m => Maybe Range -> String -> m SortName
lookupSort maybeRange nam =
  gets (lookupGamma nam . _typingContext) >>= maybeOrError maybeRange (SortNotDefined nam)

-- | Check if a sort is defined
isSortDefined :: MonadCheck m => SortName -> m Bool
isSortDefined sortName = gets (Set.member sortName . _definedSorts)

-- | Add a sort to the set of defined sorts
defineSort :: MonadCheck m => SortName -> m ()
defineSort sortName = do
  modify (over definedSorts (Set.insert sortName))
  -- all sorts is a subtype of 'Any'
  subtyped sortName (SortName "Any")

-- | Record the definition site of a sort
recordSortDefSite :: MonadCheck m => String -> Range -> m ()
recordSortDefSite sortName range = modify (over sortToDefSite (Map.insert sortName range))

-- | Associate the given type with the given variable or atom in the typing context
-- and add the sort to the set of defined sorts
typedAs :: MonadCheck m => String -> SortName -> m ()
typedAs var sortName = do
  modify (over typingContext (typesAs var sortName))
  defineSort sortName

-- | Registers the first argument as a subtype of the second
subtyped :: MonadCheck m => SortName -> SortName -> m ()
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

instance SubtypeOf SortName where
  subtypeOf s1 s2 = do
    subCtx <- gets _subtypingContext
    return $ isSubtypeOf s1 s2 subCtx

-- | Return a list of sorts associated with the functor
sortsInFunctor :: MonadCheck m => String -> m [SortName]
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
typeAsUnique :: MonadCheck m => DataCtor -> SortName -> Var -> m ()
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
    pass0VisitSyntaxDecl (SyntaxDecl vars tpy ctors range) = 
      ifM (isSortDefined (SortName tpy))
          (throwError (DuplicateSort (SortName tpy)))
          (do recordSortDefSite tpy range
              mapM_ (typeAsUnique (DataAtom (SortName tpy)) (SortName tpy)) vars)
pass0VisitDecl (TransitionDecl nam from to range) = do
  recordSortDefSite nam range
pass0VisitDecl _ = return ()

pass0 :: MonadCheck m => Program -> m ()
pass0 (Program decls _) = mapM_ pass0VisitDecl decls

-----------------------------------------
-- Pass 1: associate functors in the syntax declarations with sorts, register subtyping for variables
-----------------------------------------

-- | Process and associate the data constructors in the given sort
pass1VisitCtor :: MonadCheck m => SortName -> PureTerm -> m ()
pass1VisitCtor sortName = \case 
  (Atom nam range) -> do
    sort <- lookupSort (Just range) (runIdentity nam)
    subtyped sort sortName
  (Functor nam ts range) -> do
    sorts <- mapM (\t -> collectAtoms t >>= lookupSort (Just (rangeOf t))) ts
    let ctor = DataTagged nam sorts
    typeAsUnique ctor sortName nam
  where 
    collectAtoms (Atom nam _) = return (runIdentity nam)
    collectAtoms t = throwErrorAt (rangeOf t) $ NoNestingAt sortName

pass1VisitDecl :: MonadCheck m => Decl -> m ()
pass1VisitDecl (Syntax decls _) = mapM_ pass1VisitSyntaxDecl decls
  where
    pass1VisitSyntaxDecl (SyntaxDecl vars tpy ctors range) = 
      mapM_ (pass1VisitCtor (SortName tpy)) ctors
pass1VisitDecl (TransitionDecl nam from to range) = do
  -- transition State ~> State is equivalent to the syntax rule:
  -- ~> ::= ~>(state, state) from a typing perspective
  let ctor = DataTagged nam [SortName (fst from), SortName (fst to)]
  typeAsUnique ctor (SortName nam) nam
pass1VisitDecl _ = return ()

pass1 :: MonadCheck m => Program -> m ()
pass1 (Program decls _) = mapM_ pass1VisitDecl decls

-----------------------------------------
-- Pass 2: infer the types in the rewrite rules
-----------------------------------------

-- TODO: Also ensure that the variables used in the body of the rewrite rule
-- are actually defined in its head.

-- | Check an individual term and returns its sort
checkTerm :: MonadCheck m => PureTerm -> m SortName
checkTerm (Atom nam range) = do
  let varName = variableName (runIdentity nam)
  -- Check that the atom is not associated with a functor (DataTagged constructor)
  ctor <- lookupCtor (Just range) varName
  case ctor of
    DataTagged _ _ -> throwErrorAt range (NameNotDefined varName) -- Should be used as functor, not atom
    DataAtom _ -> lookupSort (Just range) varName
checkTerm (Functor tpy ts range) = do
  ts' <- mapM checkTerm ts
  -- Check if functor is defined and get its arity from the constructor
  ctor <- lookupCtor (Just range) tpy
  let expected = getSorts ctor
  functorSort <- lookupSort (Just range) tpy
  -- Check arity matches
  if length ts' /= length expected
    then throwErrorAt range $ IncompatibleTypes expected ts'
    else do
      ok <- and <$> zipWithM subtypeOf ts' expected
      if ok
        then return functorSort
        else throwErrorAt range $ IncompatibleTypes expected ts' 
checkTerm (Eqq t1 t2 range) = do
  t1' <- checkTerm t1
  t2' <- checkTerm t2
  if t1' /= t2'
    then throwErrorAt range (IncompatibleTypes [t1'] [t2'])
    else return t1'
checkTerm (Transition transitionName t1 t2 range) =
  -- transitions are registered in the typing context as sorts with a single data constructor
  checkTerm (Functor transitionName [t1, t2] range)

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
updateOrWidenSort :: MonadCheck m => String -> SortName -> Range -> m ()
updateOrWidenSort nam newSort range = do
  gamma <- gets _typingContext
  case lookupGamma nam gamma of
    Nothing -> typedAs nam newSort
    Just existingSort -> do
      widenedSort <- widenType range existingSort newSort
      typedAs nam widenedSort

pass2VisitDecl :: MonadCheck m => Decl -> m ()
pass2VisitDecl (Rewrite (RewriteDecl nam args bdy range) _) = do
  sorts <- mapM checkTerm args
  bdySort <- checkTerm bdy
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
assertSortDefinedAt range sortName = 
  ifM (isSortDefined (SortName sortName))
      (return ())
      (throwErrorAt range (SortNotDefined sortName))

-- | Check a rule in the given context
checkRule :: MonadCheck m => RuleDecl -> m ()
checkRule (RuleDecl nam precedent consequent _) = do
  mapM_ checkTerm (precedent ++ consequent)

pass3VisitDecl :: MonadCheck m => Decl -> m ()
pass3VisitDecl (RulesDecl rules _) = mapM_ checkRule rules
pass3VisitDecl (TransitionDecl _ (fromSort, r1) (toSort, r2) range) = do
  assertSortDefinedAt r1 fromSort
  assertSortDefinedAt r2 toSort 
pass3VisitDecl _ = return ()

pass3 :: MonadCheck m => Program -> m ()
pass3 (Program decls _) = mapM_ pass3VisitDecl decls

-----------------------------------------
-- Entrypoint
-----------------------------------------

-- | Run the type checker in its entirety
runChecker :: Program -> Either Error CheckingContext
runChecker program = do
  let initialContext = emptyCheckingContext
  execStateT (pass0 program >> pass1 program >> pass2 program >> pass3 program) initialContext


