{-# LANGUAGE UndecidableInstances #-}
module Language.Axiom.Solver.Monad
  ( -- * Solver Monad
    Solver (..),
    liftUnification,
    liftST,
    branch,
    failSearch,
    lookupCache,
    addToOutCache,
    findRules,
    findOnRules,
    addConclusionFunctor,

    -- * Running the solver
    runSolverToCompletion,
    dequeueAndRun,
    runAllQueued,

    -- * Monadic context
    SearchGoal (..),
    SearchState (..),
    QueuedSearch (..),
    EngineCtx (..),

    -- * Context construction
    emptyEngineCtx,
    fromProgram,

    -- * Context lenses
    conclusionFunctors,
    searchQueue,
    outCache,
    rewriteRules,
    onRules,
    subtyping,
    currentSearchState,
    searchStateId,
    searchSnapshot,
  )
where

import Control.Applicative (Alternative (..))
import Control.Lens
import Control.Monad (MonadPlus (..), foldM)
import Control.Monad.Except (catchError)
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans (lift)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.AST
import qualified Language.Solver.BacktrackingST as ST
import Language.Solver.Unification (RefTerm, UnificationM)
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist (Queue (..))
import Language.TypeCheck
import qualified Debug.Trace as Debug
import qualified Debug.TraceExtra as Debug

-------------------------------------------------------------
-- Solver monad with free monad structure
-------------------------------------------------------------

-- | Free monad functor for search with branching
data SearchF p s next
  = -- | Create a branch point: both alternatives to try
    Branch next next
  | -- | Failure: this search branch failed
    Fail
  | -- | Execute a unification action
    Unify (UnificationM p s next)
  | -- | Lookup a term in the out-cache
    LookupCache (PureTerm' p) (Set (PureTerm' p) -> next)
  | -- | Add a term to the out-cache with its result
    AddToOutCache (PureTerm' p) (PureTerm' p) next
  | -- | Find rules matching a functor name
    FindRules String ([RuleDecl' p] -> next)
    -- | Find "on" rules matching a functor name
  | FindOnRules String ([RuleDecl' p] -> next)
  deriving (Functor)

-- | The solver monad as a free monad over SearchF
data Solver p s a
  = Pure a
  | Free (SearchF p s (Solver p s a))

instance Show (Solver p s a) where
  show = const "<<solver>>"


instance Functor (Solver p s) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Applicative (Solver p s) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free fa = Free (fmap (fmap f) fa)
  Free ff <*> a = Free (fmap (<*> a) ff)

instance Monad (Solver p s) where
  Pure a >>= f = f a
  Free fa >>= f = Free (fmap (>>= f) fa)

-- | Lift a unification action into the solver monad
liftUnification :: UnificationM p s a -> Solver p s a
liftUnification m = Free (Unify (fmap Pure m))

-- | Lift an ST action into the solver monad
liftST :: ST.ST s a -> Solver p s a
liftST = liftUnification . Unification.liftST

-- | Create a branch point with two alternatives
-- Both branches should produce the same result type
branch :: Solver p s a -> Solver p s a -> Solver p s a
branch left right = Free (Branch left right)

-- | Represent failure in the search
failSearch :: Solver p s a
failSearch = Free Fail


-- | Lookup a term in the out-cache
lookupCache :: PureTerm' p -> Solver p s (Set (PureTerm' p))
lookupCache term = Free (LookupCache term Pure)

-- | Add a term to the out-cache with its result
addToOutCache :: PureTerm' p -> PureTerm' p -> Solver p s ()
addToOutCache key value = Free (AddToOutCache key value (Pure ()))

-- | Find rules matching a functor name
findRules :: String -> Solver p s [RuleDecl' p]
findRules name = Free (FindRules name Pure)

-- | Find 'on' rules that unify with the given term 
findOnRules :: String -> Solver p s [RuleDecl' p]
findOnRules name = Free (FindOnRules name Pure)


-- | Alternative instance for non-deterministic search
-- empty represents failure, <|> represents branching
instance Alternative (Solver p s) where
  empty = failSearch
  (<|>) = branch

-- | MonadPlus instance
instance MonadPlus (Solver p s) where
  mzero = empty
  mplus = (<|>)

-------------------------------------------------------------
-- Solver context & Search states
-------------------------------------------------------------

-- | A new goal for search, keeps track of the term
-- to be solved and also from which rule this term
-- originated.
data SearchGoal p s = SearchGoal
  { _goalRuleName :: String,
    _goalTerm :: RefTerm p s
  }

deriving instance (ForAllPhases Show p) => Show (SearchGoal p s)

data SearchState p s = SearchState
  { -- | Unique identifier for this search state
    _searchStateId :: Int,
    -- | Snapshot to restore when resuming this search
    _searchSnapshot :: Unification.Snapshot p s
  }

deriving instance (ForAllPhases Show p) => Show (SearchState p s)

-- | A queued search: pairs a computation with its search state
-- Uses existential quantification to hide the result type
data QueuedSearch p s = forall a. QueuedSearch
  { _queuedComputation :: !(Solver p s a),
    _queuedState :: !(SearchState p s)
  }


deriving instance (ForAllPhases Show p) => Show (QueuedSearch p s)

$(makeLenses ''SearchState)

data EngineCtx p q s = EngineCtx
  { -- | Mapping from functor names to their appropriate rule, used in backwards reasoning
    _conclusionFunctors :: !(Map String (Set (RuleDecl' p))),
    -- | Queue of states that need to be considered
    _searchQueue :: !(q (QueuedSearch p s)),
    -- | Out-caching mechanism as a mapping from terms to ground terms
    _outCache :: !(Map (PureTerm' p) (Set (PureTerm' p))),
    -- | Information about rewrite rules
    _rewriteRules :: Unification.Rewrites p,
    -- | Set of rules declared with "on" for forward reasoning
    -- represented as a map and indexed by the name of the facts
    -- in the precedent.
    _onRules :: Map String (Set (RuleDecl' p)),
    -- | Subtyping graph for type-aware unification
    _subtyping :: !Subtyping,
    -- | The current state of the search
    _currentSearchState :: !(SearchState p s)
  }

$(makeLenses ''EngineCtx)

-------------------------------------------------------------
-- Context construction
-------------------------------------------------------------

-- | Create an empty engine context with initial search state
emptyEngineCtx :: (Queue q) => Subtyping -> EngineCtx p q s
emptyEngineCtx subtyping =
  EngineCtx
    { _conclusionFunctors = Map.empty,
      _searchQueue = emptyQueue,
      _outCache = Map.empty,
      _rewriteRules = Map.empty,
      _onRules = Map.empty,
      _subtyping = subtyping,
      _currentSearchState = SearchState 0 undefined -- snapshot will be set when needed
    }

-- | Associate a functor name with a rule
addConclusionFunctor :: (ForAllPhases Ord p) => String -> RuleDecl' p -> EngineCtx p q s -> EngineCtx p q s
addConclusionFunctor nam decl =
  over conclusionFunctors (Map.insertWith Set.union nam (Set.singleton decl))

addOnFunctor :: (ForAllPhases Ord p) => String -> RuleDecl' p -> EngineCtx p q s -> EngineCtx p q s
addOnFunctor  nam decl =
  over onRules (Map.insertWith Set.union nam (Set.singleton decl))

-- | Construct an initial solver engine context from a program
fromProgram ::
  forall q s p.
  (Queue q, ForAllPhases Ord p) =>
  -- | subtyping context for the given program
  Subtyping ->
  -- | the program to create a context for
  Program' p ->
  EngineCtx p q s
fromProgram subtyping (Program decls _) =
   (flip $ foldr visitRewrite) rewrites
 $ (flip $ foldr visitRule) rules 
 $ emptyEngineCtx subtyping
  where
    rules = [r | RulesDecl _ rulesDecl _ <- decls, r <- rulesDecl]
    rewrites = [r | Rewrite r _ <- decls]
    visitRule rule@(RuleDecl _ _precedent consequents _) =
      flip (foldr (`addConclusionFunctor` rule)) (mapMaybe functorName (List.singleton (head consequents)))
    visitRule rule@(OnRuleDecl _ precedents _ _) =
      flip (foldr (`addOnFunctor` rule)) (mapMaybe functorName (List.singleton (head precedents)))
    visitRewrite decl@(RewriteDecl nam _ _ _) =
      over rewriteRules (Map.insertWith (++) nam [decl])

-----------------------------------------------------------
-- Interpreter: run the free monad and manage search states
-------------------------------------------------------------

-- | Run a single step of the solver computation
-- When we encounter a Branch, we capture the snapshot and in-cache,
-- enqueue the right branch, and continue with the left
-- Returns Nothing if this branch failed
runSolverStep :: (Queue q, ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) (Maybe (Either a (Solver p s a)))
runSolverStep (Pure a) = return (Just (Left a))
runSolverStep (Free Fail) =
  return (Debug.trace "fail" Nothing) -- This branch fails, try next from queue
runSolverStep (Free (Branch left right)) = do
  -- Take a snapshot for the right branch
  snapshot <- lift Unification.snapshot

  -- Create search state for the right branch
  let rightState = SearchState 0 snapshot
  let rightQueued = QueuedSearch right rightState

  -- Enqueue only the right branch
  modify (over searchQueue (enqueue rightQueued))

  -- Continue immediately with the left branch
  return (Just (Right left))
runSolverStep (Free (Unify m)) = do
  a <- lift m
  return (Just (Right a))
runSolverStep (Free (LookupCache term k)) = do
  ctx <- get
  let cache = ctx ^. outCache
  -- Find all cache entries whose keys unify with the query term
  matchingResults <- lift $ foldM (checkCacheEntry term) Set.empty (Map.toList cache)
  return (Just (Right (k matchingResults)))
  where
    checkCacheEntry :: (ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
                    => PureTerm' p
                    -> Set (PureTerm' p)
                    -> (PureTerm' p, Set (PureTerm' p))
                    -> UnificationM p s (Set (PureTerm' p))
    checkCacheEntry queryTerm accum (cacheKey, cacheValues) = do
      -- Take snapshot before unification attempt
      snapshot <- Unification.snapshot

      -- Make both terms unique to avoid permanent unification
      queryTerm' <- Unification.renameTerm queryTerm
      cacheKey' <- Unification.renameTerm cacheKey

      queryRef <- Unification.refTerm' (Debug.traceShowPrefix "trying>>" queryTerm')
      cacheKeyRef <- Unification.refTerm' cacheKey'

      -- Try to unify
      unifyResult <- fmap Right (Unification.unifyTerms queryRef cacheKeyRef) `catchError` (return . Left)

      -- Restore snapshot regardless of result
      Unification.restore snapshot

      -- If unification succeeded, add the cache values to results
      return $ case unifyResult of
        Right () -> Set.union accum cacheValues
        Left _ -> accum
runSolverStep (Free (AddToOutCache key value k)) = do
  modify (over outCache (Map.insertWith Set.union key (Set.singleton value)))
  return (Just (Right k))
runSolverStep (Free (FindRules name k)) = do
  ctx <- get
  let rules = Set.toList $ Map.findWithDefault Set.empty name (ctx ^. conclusionFunctors)
  return (Just (Right (k rules)))
runSolverStep (Free (FindOnRules name k)) = do
  ctx <- get
  let rules = Set.toList $ Map.findWithDefault Set.empty name (ctx ^. onRules)
  return (Just (Right (k rules)))

-- | Run a solver computation to completion, looping until we get a result or it fails
-- Returns Nothing if this branch failed
runSolverToCompletion :: (Queue q, ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) (Maybe a)
runSolverToCompletion solver = do
  result <- runSolverStep solver
  case Debug.traceWith (("failed>> " ++) . show . isNothing) result of
    Nothing -> return Nothing -- This branch failed
    Just (Left a) -> return (Just a) -- Got a result
    Just (Right nextSolver) -> runSolverToCompletion nextSolver -- Continue

-- | Dequeue the next search state and run it
-- Returns Nothing if the queue is empty
dequeueAndRun :: (Queue q, ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p) => StateT (EngineCtx p q s) (UnificationM p s) (Maybe ())
dequeueAndRun = do
  ctx <- get
  case dequeue (ctx ^. searchQueue) of
    Nothing -> return Nothing -- Queue is empty
    Just (QueuedSearch computation state, restQueue) -> do
      -- Update the queue
      modify (set searchQueue restQueue)

      -- Restore the snapshot from the dequeued state
      lift $ Unification.restore (state ^. searchSnapshot)

      -- Update current search state
      modify (set currentSearchState state)

      -- Run the computation to completion
      -- If this branch fails, that's okay - we'll just try the next one from the queue
      _ <- runSolverToCompletion computation

      return (Just ())

-- | Run all queued searches until the queue is empty
-- This processes all branches in the search tree
runAllQueued :: (Queue q, ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p) => StateT (EngineCtx p q s) (UnificationM p s) ()
runAllQueued = do
  maybeMore <- dequeueAndRun
  case maybeMore of
    Nothing -> return () -- Queue empty, done
    Just () -> runAllQueued -- Continue processing

