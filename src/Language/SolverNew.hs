{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Language.SolverNew(
    EngineCtx,
    fromProgram
  , runSolver) where

import Control.Lens
import Control.Monad (MonadPlus(..), msum, when, unless)
import Control.Monad.Except (catchError)
import Control.Monad.Extra (ifM)
import Control.Monad.State (StateT, get, gets, modify, evalStateT)
import Control.Monad.Trans (lift)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Proxy
import Language.AST
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Renamer as Renamer
import Language.Solver.Unification (RefTerm, UnificationM)
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist (Queue(..))
import Language.Types
import qualified Debug.Trace as Debug
import Language.Axiom.Solver.Monad
import Data.Either (isRight)

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
  return $ isRight result

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

                  -- Add other consequents to in-cache, so that they get added to the out-cache
                  -- once the precedents are solved.
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
-- High-level API
-------------------------------------------------------------

-- | Process all queued searches (ignoring their results, just for side effects like caching)
processAllQueued :: (Queue q, ForAllPhases Ord p, ForAllPhases Show p) => StateT (EngineCtx p q s) (UnificationM p s) [Bool]
processAllQueued = do
  ctx <- get
  case dequeue (ctx ^. searchQueue) of
    Nothing -> return []  -- Queue is empty
    Just (QueuedSearch computation state, restQueue) -> do
      modify (set searchQueue restQueue)
      lift $ Unification.liftST $ ST.restore (state ^. searchSnapshot)
      modify (set currentSearchState (Debug.traceWith (show . _searchGoals) state))
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

  if initialCache == Debug.traceWith (("out >> " ++) . show) finalCache
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
