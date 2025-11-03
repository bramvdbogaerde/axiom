{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.SolverNew where

import Control.Applicative (Alternative(..))
import Control.Lens
import Control.Monad (MonadPlus(..))
import Control.Monad.State (StateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import Data.Set (Set)
import Language.AST
import qualified Language.Solver.BacktrackingST as ST
import Language.Solver.Unification (RefTerm, UnificationM)
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist (Queue(..))
import Language.TypeCheck

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
  | -- | Pop the first goal from the goal list (returns Nothing if empty)
    PopGoal (Maybe (SearchGoal p s) -> next)
  | -- | Add goals to the current goal list
    AddGoals [SearchGoal p s] next
  | -- | Get the in-cache (whenSucceeds) for the current search state
    GetInCache ([PureTerm' p] -> next)
  | -- | Add a term to the in-cache
    AddToInCache (PureTerm' p) next
  | -- | Lookup a term in the out-cache
    LookupCache (PureTerm' p) (Set (PureTerm' p) -> next)
  | -- | Add a term to the out-cache with its result
    AddToOutCache (PureTerm' p) (PureTerm' p) next
  | -- | Find rules matching a functor name
    FindRules String ([RuleDecl' p] -> next)
  deriving (Functor)

-- | The solver monad as a free monad over SearchF
data Solver p s a
  = Pure a
  | Free (SearchF p s (Solver p s a))

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

-- | Create a branch point with two alternatives
-- Both branches should produce the same result type
branch :: Solver p s a -> Solver p s a -> Solver p s a
branch left right = Free (Branch left right)

-- | Represent failure in the search
failSearch :: Solver p s a
failSearch = Free Fail

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

data SearchState p s = SearchState
  { -- | Unique identifier for this search state
    _searchStateId :: Int,
    -- | Remaining goals to be solved
    _searchGoals :: ![SearchGoal p s],
    -- | Snapshot to restore when resuming this search
    _searchSnapshot :: ST.Snapshot s,
    -- | Goals to add to the cache when all the search goals in this state
    -- have been solved. (i.e., in-cache)
    _searchWhenSucceeds :: ![PureTerm' p]
  }

-- | A queued search: pairs a computation with its search state
-- Uses existential quantification to hide the result type
data QueuedSearch p s = forall a. QueuedSearch
  { _queuedComputation :: !(Solver p s a),
    _queuedState :: !(SearchState p s)
  }

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
-- Interpreter: run the free monad and manage search states
-------------------------------------------------------------

-- | Run a single step of the solver computation
-- When we encounter a Branch, we capture the snapshot and in-cache,
-- enqueue the right branch, and continue with the left
runSolverStep :: (Queue q) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) (Either a (Solver p s a))
runSolverStep (Pure a) = return (Left a)

runSolverStep (Free Fail) = return (Left (error "Search failed"))  -- This branch fails

runSolverStep (Free (Branch left right)) = do
  -- Take a snapshot for the right branch
  snapshot <- lift $ Unification.liftST ST.snapshot

  -- Get current state information
  ctx <- get
  let currentState = ctx ^. currentSearchState
  let inCache = currentState ^. searchWhenSucceeds
  let goals = currentState ^. searchGoals

  -- Create search state for the right branch
  let rightState = SearchState 0 goals snapshot inCache
  let rightQueued = QueuedSearch right rightState

  -- Enqueue only the right branch
  modify (over searchQueue (enqueue rightQueued))

  -- Continue immediately with the left branch
  return (Right left)

runSolverStep (Free (Unify m)) = do
  a <- lift m
  return (Right a)

-- | Run a solver computation to completion, looping until we get a result
runSolverToCompletion :: (Queue q) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) a
runSolverToCompletion solver = do
  result <- runSolverStep solver
  case result of
    Left a -> return a
    Right nextSolver -> runSolverToCompletion nextSolver

-- | Dequeue the next search state and run it
-- Returns Nothing if the queue is empty
dequeueAndRun :: (Queue q) => StateT (EngineCtx p q s) (UnificationM p s) (Maybe ())
dequeueAndRun = do
  ctx <- get
  case dequeue (ctx ^. searchQueue) of
    Nothing -> return Nothing  -- Queue is empty
    Just (QueuedSearch computation state, restQueue) -> do
      -- Update the queue
      modify (set searchQueue restQueue)

      -- Restore the snapshot from the dequeued state
      lift $ Unification.liftST $ ST.restore (state ^. searchSnapshot)

      -- Update current search state
      modify (set currentSearchState state)

      -- Run the computation to completion
      _ <- runSolverToCompletion computation

      return (Just ())

-- | Run all queued searches until the queue is empty
-- This processes all branches in the search tree
runAllQueued :: (Queue q) => StateT (EngineCtx p q s) (UnificationM p s) ()
runAllQueued = do
  maybeMore <- dequeueAndRun
  case maybeMore of
    Nothing -> return ()  -- Queue empty, done
    Just () -> runAllQueued  -- Continue processing
