{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.SolverNew where

import Control.Applicative (Alternative(..))
import Control.Lens
import Control.Monad (MonadPlus(..), msum, when, unless)
import Control.Monad.Except (catchError)
import Control.Monad.Extra (ifM)
import Control.Monad.State (StateT, get, gets, modify, evalStateT)
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Proxy
import qualified Data.List as List
import Language.AST
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Renamer as Renamer
import Language.Solver.Unification (RefTerm, UnificationM)
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist (Queue(..))
import Language.TypeCheck
import Language.Types
import qualified Debug.Trace as Debug

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

-- | Pop the first goal from the goal list
popGoal :: Solver p s (Maybe (SearchGoal p s))
popGoal = Free (PopGoal Pure)

-- | Add goals to the current goal list
addGoals :: [SearchGoal p s] -> Solver p s ()
addGoals goals = Free (AddGoals goals (Pure ()))

-- | Get the in-cache for the current search state
getInCache :: Solver p s [PureTerm' p]
getInCache = Free (GetInCache Pure)

-- | Add a term to the in-cache
addToInCache :: PureTerm' p -> Solver p s ()
addToInCache term = Free (AddToInCache term (Pure ()))

-- | Lookup a term in the out-cache
lookupCache :: PureTerm' p -> Solver p s (Set (PureTerm' p))
lookupCache term = Free (LookupCache term Pure)

-- | Add a term to the out-cache with its result
addToOutCache :: PureTerm' p -> PureTerm' p -> Solver p s ()
addToOutCache key value = Free (AddToOutCache key value (Pure ()))

-- | Find rules matching a functor name
findRules :: String -> Solver p s [RuleDecl' p]
findRules name = Free (FindRules name Pure)

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
-- Context construction
-------------------------------------------------------------

-- | Create an empty engine context with initial search state
emptyEngineCtx :: (Queue q) => Subtyping -> EngineCtx p q s
emptyEngineCtx subtyping = EngineCtx
  { _conclusionFunctors = Map.empty
  , _searchQueue = emptyQueue
  , _outCache = Map.empty
  , _rewriteRules = Map.empty
  , _onRules = Map.empty
  , _subtyping = subtyping
  , _currentSearchState = SearchState 0 [] undefined []  -- snapshot will be set when needed
  }

-- | Associate a functor name with a rule
addConclusionFunctor :: (ForAllPhases Ord p) => String -> RuleDecl' p -> EngineCtx p q s -> EngineCtx p q s
addConclusionFunctor nam decl =
  over conclusionFunctors (Map.insertWith Set.union nam (Set.singleton decl))

-- | Construct an initial solver engine context from a program
fromProgram :: forall q s p. (Queue q, ForAllPhases Ord p)
            => Subtyping    -- ^ subtyping context for the given program
            -> Program' p   -- ^ the program to create a context for
            -> EngineCtx p q s
fromProgram subtyping (Program decls _) =
     (flip $ foldr visitRewrite) rewrites
   $ (flip $ foldr visitRule) rules
   $ emptyEngineCtx subtyping
  where rules    = [r | RulesDecl _ rulesDecl _ <- decls, r <- rulesDecl]
        rewrites = [r | Rewrite r _ <- decls]
        visitRule rule@(RuleDecl _ _precedent consequents _) =
          flip (foldr (`addConclusionFunctor` rule)) (mapMaybe functorName (List.singleton (head consequents)))
        visitRule rule@(OnRuleDecl _ precedents _ _) =
          flip (foldr (`addConclusionFunctor` rule)) (mapMaybe functorName (List.singleton (head precedents)))
        visitRewrite decl@(RewriteDecl nam _ _ _) =
          over rewriteRules (Map.insertWith (++) nam [decl])

-------------------------------------------------------------
-- Solver helper functions
-------------------------------------------------------------

-- | Convert a PureTerm to RefTerm
refTerm :: (ForAllPhases Ord p, AnnotateType p) => PureTerm' p -> Solver p s (RefTerm p s)
refTerm = liftUnification . Unification.refTerm'

-- | Convert a RefTerm back to a pure term
pureTerm :: (ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p) => RefTerm p s -> Solver p s (PureTerm' p)
pureTerm = liftUnification . Unification.pureTerm''

-- | Unify two terms
unify :: (ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
      => RefTerm p s -> RefTerm p s -> Solver p s (Either String ())
unify left right =
  liftUnification $ fmap Right (Unification.unifyTerms left right) `catchError` (return . Left)
  where catchError m h = m `Control.Monad.Except.catchError` h

-- | Check if two terms unify
doesUnify :: (ForAllPhases Ord p, ForAllPhases Show p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
          => RefTerm p s -> RefTerm p s -> Solver p s Bool
doesUnify t1 t2 = do
  snapshot <- liftST ST.snapshot
  result <- unify t1 t2
  liftST $ ST.restore snapshot
  return $ either (const False) (const True) result


-------------------------------------------------------------
-- Core search functions
-------------------------------------------------------------

-- | Main solve loop: pop goals and expand them until no goals remain
solveGoals :: (AnnotateType p, ForAllPhases Show p, HaskellExprExecutor p, HaskellExprRename p, ForAllPhases Ord p)
           => Solver p s ()
solveGoals = do
  maybeGoal <- popGoal
  case maybeGoal of
    Nothing -> do
      -- No more goals, we have a solution!
      -- Add all in-cache terms to the out-cache
      inCacheTerms <- getInCache
      mapM_ addToOutCacheGround $ Debug.traceWith (("in-cache>> " ++) . show) inCacheTerms
    Just goal -> do
      expandGoal goal
      solveGoals

-- | Helper to add a term to the out-cache (only if it's ground)
addToOutCacheGround :: (AnnotateType p, ForAllPhases Show p, HaskellExprRename p, ForAllPhases Ord p)
                    => PureTerm' p -> Solver p s ()
addToOutCacheGround t = do
  -- Normalize by removing unique variable names
  let t' = Renamer.unrenameTerm t
  tr <- refTerm t
  tground <- pureTerm tr
  -- Only add ground terms to cache
  when (isTermGround tground) $
    addToOutCache t' tground

-- | Check if a RefTerm is in the in-cache
isInInCache :: (AnnotateType p, ForAllPhases Show p, HaskellExprRename p, HaskellExprExecutor p, ForAllPhases Ord p)
            => RefTerm p s -> Solver p s Bool
isInInCache goal = do
  inCacheTerms <- getInCache
  or <$> mapM (checkUnifies goal) inCacheTerms
  where
    checkUnifies goalRef cacheTerm = do
      cacheTerm' <- uniqueTerm cacheTerm
      cacheRef <- refTerm cacheTerm'
      doesUnify goalRef cacheRef

-- | Create a unique copy of a term by renaming all variables
uniqueTerm :: (HaskellExprRename p, ForAllPhases Ord p) => PureTerm' p -> Solver p s (PureTerm' p)
uniqueTerm t = liftUnification (Unification.renameRule (RuleDecl "dummy" [] [t] dummyRange)) >>= \case
  RuleDecl _ _ [t'] _ -> return t'
  _ -> error "uniqueTerm: unexpected result"

-- | Expand a single goal by trying all matching rules
expandGoal :: (AnnotateType p, ForAllPhases Show p, HaskellExprRename p, HaskellExprExecutor p, ForAllPhases Ord p)
           => SearchGoal p s -> Solver p s ()
expandGoal (SearchGoal _ruleName goalTerm) = do
  case goalTerm of
    -- Functors: look for rules with the name of the functor in its conclusion
    Functor name _ _ _ -> do
      rules <- findRules name
      -- Check if goal is in in-cache and add if not
      isInCache <- isInInCache goalTerm
      goalPure <- pureTerm goalTerm
      unless isInCache $ addToInCache goalPure
      -- Try each rule as a branch
      msum $ map (processRule goalTerm isInCache) rules

    -- Transitions: look for rules with the transition name in the conclusion
    Transition nam _from _to _ _s -> do
      rules <- findRules nam
      -- Check if goal is in in-cache and add if not
      isInCache <- isInInCache goalTerm
      goalPure <- pureTerm goalTerm
      unless isInCache $ addToInCache goalPure
      msum $ map (processRule goalTerm isInCache) rules

    -- Equality: try to unify
    Eqq left right _ _ -> do
      result <- unify left right
      case result of
        Left _ -> mzero  -- Unification failed
        Right () -> return ()  -- Success, continue

    -- Inequality: fail if unifies, succeed otherwise
    Neq left right _ _ ->
      ifM (doesUnify left right) mzero (return ())

    -- Set membership
    IncludedIn vrr set _ ->
      expandSet vrr set

    _ -> return ()  -- Variables and other terms can't be expanded

-- | Expand set membership: vrr ∈ set
expandSet :: forall p s. (AnnotateType p, ForAllPhases Show p, HaskellExprRename p, HaskellExprExecutor p, ForAllPhases Ord p)
          => String -> RefTerm p s -> Solver p s ()
expandSet nam set = do
  vrrTerm <- refTerm (Atom (Identity nam) (typeAnnot (Proxy @p) AnyType) dummyRange)
  setTermMaybe <- liftST $ Unification.termValue set
  case setTermMaybe of
    Nothing -> error "non-ground sets are not supported"
    Just setTerm -> case setTerm of
      SetOfTerms ts _ _ ->
        -- Try unifying vrr with each element of the set
        msum $ map (\t -> do
          result <- unify vrrTerm t
          case result of
            Left _ -> mzero
            Right () -> return ()
        ) (Set.toList ts)
      _ -> error "unreachable branch found, set terms should only resolve to sets"

-- | Process a rule: try to unify the goal with the rule's consequent
processRule :: forall p s. (AnnotateType p, ForAllPhases Show p, HaskellExprRename p, HaskellExprExecutor p, ForAllPhases Ord p)
            => RefTerm p s -> Bool -> RuleDecl' p -> Solver p s ()
processRule goal isInCache rule = do
  case rule of
    OnRuleDecl {} -> error "unexpected 'on' declaration"
    RuleDecl ruleName _precedents consequents _ -> do
      case consequents of
        [] -> error $ "Rule " ++ ruleName ++ " has no consequents"
        _ -> do
          -- Rename the rule to ensure variable freshness
          renamedRule <- liftUnification (Unification.renameRule rule)

          -- Extract the renamed parts
          case renamedRule of
            OnRuleDecl {} -> error "unexpected 'on' declaration after renaming"
            RuleDecl _ renamedPrecedents renamedConsequents _ -> do
              case renamedConsequents of
                [] -> error $ "Rule " ++ ruleName ++ " has no consequents after renaming"
                (renamedConsequent:renamedOtherConsequents) -> do
                  -- Convert consequent to RefTerm
                  refConsequent <- refTerm renamedConsequent

                  -- Create unification goal
                  let unificationGoal = SearchGoal ("Csq-" ++ ruleName)
                        (Eqq goal refConsequent (typeAnnot (Proxy @p) AnyType) dummyRange)

                  -- Add other consequents to in-cache
                  mapM_ addToInCache renamedOtherConsequents

                  if null renamedPrecedents
                    then do
                      -- Rules WITHOUT precedents: Process directly without caching
                      addGoals [unificationGoal]
                    else do
                      -- Rules WITH precedents: Apply caching logic
                      goalPure <- pureTerm goal

                      if isInCache
                        then do
                          -- Goal is in in-cache: only use cached results
                          cachedResults <- lookupCache goalPure
                          msum $ map (processCachedResult goal ruleName "In") (Set.toList cachedResults)
                        else do
                          -- Goal not in in-cache: check cache and also process normally
                          cachedResults <- lookupCache goalPure

                          -- Create normal processing branch
                          let normalBranch = do
                                precedentRefs <- mapM refTerm renamedPrecedents
                                let precedentGoals = map (SearchGoal ruleName) precedentRefs
                                addGoals (unificationGoal : precedentGoals)

                          -- Create branches for cached results
                          let cachedBranches = map (processCachedResult goal ruleName "Old") (Set.toList cachedResults)

                          -- Try all branches (normal + cached)
                          msum (normalBranch : cachedBranches)
  where
    processCachedResult :: RefTerm p s -> String -> String -> PureTerm' p -> Solver p s ()
    processCachedResult goalRef ruleName prefix cachedResult = do
      cachedRef <- refTerm cachedResult
      let eqqGoal = SearchGoal ("Cached-" ++ prefix ++ "-" ++ ruleName)
            (Eqq goalRef cachedRef (typeAnnot (Proxy @p) AnyType) dummyRange)
      addGoals [eqqGoal]

-------------------------------------------------------------
-- Interpreter: run the free monad and manage search states
-------------------------------------------------------------

-- | Run a single step of the solver computation
-- When we encounter a Branch, we capture the snapshot and in-cache,
-- enqueue the right branch, and continue with the left
-- Returns Nothing if this branch failed
runSolverStep :: (Queue q, ForAllPhases Ord p) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) (Maybe (Either a (Solver p s a)))
runSolverStep (Pure a) = return (Just (Left a))

runSolverStep (Free Fail) = return Nothing  -- This branch fails, try next from queue

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
  return (Just (Right left))

runSolverStep (Free (Unify m)) = do
  a <- lift m
  return (Just (Right a))

runSolverStep (Free (PopGoal k)) = do
  ctx <- get
  let goals = ctx ^. currentSearchState . searchGoals
  case goals of
    [] -> return (Just (Right (k Nothing)))
    (g:gs) -> do
      -- Update the goals in the current search state
      modify (over currentSearchState (set searchGoals gs))
      return (Just (Right (k (Just g))))

runSolverStep (Free (AddGoals newGoals k)) = do
  modify (over currentSearchState (over searchGoals (++ newGoals)))
  return (Just (Right k))

runSolverStep (Free (GetInCache k)) = do
  ctx <- get
  let inCache = ctx ^. currentSearchState . searchWhenSucceeds
  return (Just (Right (k inCache)))

runSolverStep (Free (AddToInCache term k)) = do
  modify (over currentSearchState (over searchWhenSucceeds (term :)))
  return (Just (Right k))

runSolverStep (Free (LookupCache term k)) = do
  ctx <- get
  let cache = ctx ^. outCache
  -- Find entries in cache that unify with the term (simplified for now)
  let results = Map.findWithDefault Set.empty term cache
  return (Just (Right (k results)))

runSolverStep (Free (AddToOutCache key value k)) = do
  modify (over outCache (Map.insertWith Set.union key (Set.singleton value)))
  return (Just (Right k))

runSolverStep (Free (FindRules name k)) = do
  ctx <- get
  let rules = Set.toList $ Map.findWithDefault Set.empty name (ctx ^. conclusionFunctors)
  return (Just (Right (k rules)))

-- | Run a solver computation to completion, looping until we get a result or it fails
-- Returns Nothing if this branch failed
runSolverToCompletion :: (Queue q, ForAllPhases Ord p) => Solver p s a -> StateT (EngineCtx p q s) (UnificationM p s) (Maybe a)
runSolverToCompletion solver = do
  result <- runSolverStep solver
  case result of
    Nothing -> return Nothing  -- This branch failed
    Just (Left a) -> return (Just a)  -- Got a result
    Just (Right nextSolver) -> runSolverToCompletion nextSolver  -- Continue

-- | Dequeue the next search state and run it
-- Returns Nothing if the queue is empty
dequeueAndRun :: (Queue q, ForAllPhases Ord p) => StateT (EngineCtx p q s) (UnificationM p s) (Maybe ())
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
      -- If this branch fails, that's okay - we'll just try the next one from the queue
      _ <- runSolverToCompletion computation

      return (Just ())

-- | Run all queued searches until the queue is empty
-- This processes all branches in the search tree
runAllQueued :: (Queue q, ForAllPhases Ord p) => StateT (EngineCtx p q s) (UnificationM p s) ()
runAllQueued = do
  maybeMore <- dequeueAndRun
  case maybeMore of
    Nothing -> return ()  -- Queue empty, done
    Just () -> runAllQueued  -- Continue processing

-------------------------------------------------------------
-- High-level API
-------------------------------------------------------------

-- | Process all queued searches (ignoring their results, just for side effects like caching)
processAllQueued :: (Queue q, ForAllPhases Ord p) => StateT (EngineCtx p q s) (UnificationM p s) [Bool]
processAllQueued = do
  ctx <- get
  case dequeue (ctx ^. searchQueue) of
    Nothing -> return []  -- Queue is empty
    Just (QueuedSearch computation state, restQueue) -> do
      modify (set searchQueue restQueue)
      lift $ Unification.liftST $ ST.restore (state ^. searchSnapshot)
      modify (set currentSearchState state)
      result <- runSolverToCompletion computation  -- Run for side effects (cache updates)
      fmap (isJust result :) processAllQueued  -- Continue with rest of queue

-- | Solve until the cache stabilizes
-- Repeatedly reinitializes and solves the query until the cache stops growing
solveUntilStable :: forall p q s. (AnnotateType p, ForAllPhases Show p, HaskellExprExecutor p, HaskellExprRename p, ForAllPhases Ord p, Queue q)
                 => PureTerm' p -> StateT (EngineCtx p q s) (UnificationM p s) Bool
solveUntilStable pureQuery = do
  -- Save initial cache
  initialCache <- gets (^. outCache)

  -- Re-create ref term from the pure query (fresh ref terms after snapshot restore)
  refQuery <- lift $ Unification.refTerm' pureQuery
  snapshot <- lift $ Unification.liftST ST.snapshot

  let initialGoal = SearchGoal "initial" refQuery
  let initialState = SearchState 0 [initialGoal] snapshot []

  modify (set currentSearchState initialState)

  -- Run the solver computation
  firstResult <- runSolverToCompletion solveGoals

  -- Process all queued searches (for cache population)
  results <- (isJust firstResult :) <$> processAllQueued

  -- Check if cache changed
  finalCache <- gets (^. outCache)

  if initialCache == Debug.traceShowId finalCache
    then do
      -- Cache stable - return whether we found a solution
      return (or results)
    else solveUntilStable pureQuery  -- Cache changed, re-run with same query

-- | Main entry point for solving a query
-- Sets up initial state and runs solver until cache stabilizes
-- Returns True if any solution found, False otherwise
solve :: forall p q s. (AnnotateType p, ForAllPhases Show p, HaskellExprExecutor p, HaskellExprRename p, ForAllPhases Ord p, Queue q)
      => PureTerm' p -> StateT (EngineCtx p q s) (UnificationM p s) Bool
solve pureQuery = do
  -- Run until stable
  solveUntilStable pureQuery

-- | Run a solver on a query with cache stabilization
-- Returns True if any solution found, False otherwise
runSolver :: forall p q s. (HaskellExprExecutor p, AnnotateType p, ForAllPhases Ord p, Queue q, ForAllPhases Show p, HaskellExprRename p)
          => EngineCtx p q s -> PureTerm' p -> ST.ST s Bool
runSolver ctx query = do
  result <- Unification.runUnificationM
    (_subtyping ctx)
    (_rewriteRules ctx)
    (evalStateT (solve query) ctx)
  case result of
    Left err -> error err
    Right hasSolution -> return hasSolution
