{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections, MonoLocalBinds #-}

module Language.Solver where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Proxy
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.AST
import Language.Types
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Renamer as Renamer
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist
import Language.Solver.Unification (RefTerm)
import qualified Debug.Trace as Debug
import Control.Monad.Extra (ifM, when, unless)
import Data.Either
import Data.Maybe (catMaybes, fromMaybe)
import Control.Monad ((>=>))

------------------------------------------------------------
-- Search data structures
------------------------------------------------------------

-- | A new goal for search, keeps track of the term
-- to be solved and also from which rule this term
-- originated.
data SearchGoal p s = SearchGoal
  { _goalRuleName :: String,
    _goalTerm :: RefTerm p s }

type InOut p s = (PureTerm' p)

data SearchState p s = SearchState
  { -- | Unique identifier for this search state
    _searchStateId :: Int,
    -- | Remaining goals to be solved
    _searchGoals :: ![SearchGoal p s],
    -- | Snapshot to restore when resuming this search    
    _searchSnapshot :: ST.Snapshot s,
    -- | Goals to add to the cache when all the search goals in this state
    -- have been solved.
    _searchWhenSucceeds :: ![InOut p s]
  }

data SearchCtx p q s = SearchCtx
  { _searchQueue :: !(q (SearchState p s)),
    _currentMapping :: !(Unification.VariableMapping p s)
  }

type SolverResult = Either String (Map String PureTerm)

$(makeLenses ''SearchState)
$(makeLenses ''SearchCtx)

------------------------------------------------------------
-- Core data structures
------------------------------------------------------------

data EngineCtx p q s = EngineCtx
  { -- | Mapping from functor names to their appropriate rule, used in backwards reasoning
    _conclusionFunctors :: !(Map String (Set (RuleDecl' p))),
    -- | Search context for query resolution
    _searchCtx :: !(SearchCtx p q s),
    -- | Out-caching mechanism as a mapping from terms to ground terms
    _outCache :: !(Map (PureTerm' p) (Set (PureTerm' p))),
    -- | Information about rewrite rules
    _rewriteRules :: Unification.Rewrites p,
    -- | Subtyping graph for type-aware unification
    _subtyping :: !Subtyping
  }

-- | Create an empty search context
emptySearchCtx :: (Queue q) => SearchCtx p q s
emptySearchCtx = SearchCtx emptyQueue Map.empty

-- | Create an empty solver engine context
emptyEngineCtx :: (Queue q) => Subtyping -> EngineCtx p q s
emptyEngineCtx = EngineCtx Map.empty emptySearchCtx Map.empty Map.empty

$(makeLenses ''EngineCtx)

-- | Associate the given functor with the given rule, indicating that it can be found in the conclusion of that rule.
addConclusionFunctor :: (ForAllPhases Ord p) => String -> RuleDecl' p -> EngineCtx p q s -> EngineCtx p q s
addConclusionFunctor nam decl =
  over conclusionFunctors (Map.insertWith Set.union nam (Set.singleton decl))

-- | Construct an initial context from the rules defined the program
fromRules :: forall q s p . (Queue q, ForAllPhases Ord p) => Subtyping -> [RuleDecl' p] -> [PureRewriteDecl p] -> EngineCtx p q s
fromRules subtyping rules rewrites =
      flip (foldr visitRewrite) rewrites
    $ foldr visit (emptyEngineCtx subtyping) rules
  where
    visit rule@(RuleDecl _ _precedent consequent _) =
      flip (foldr (`addConclusionFunctor` rule)) (foldMap functorName consequent)
    visitRewrite decl@(RewriteDecl nam _ _ _) =
      over rewriteRules (Map.insertWith (++) nam [decl])

------------------------------------------------------------
-- Monad context
------------------------------------------------------------

-- | The solver monadic context
newtype Solver p q s a = Solver {getSolver :: StateT (EngineCtx p q s) (Unification.UnificationM p s) a }
  deriving (Applicative, Functor, Monad, MonadState (EngineCtx p q s))


runSolver :: (HaskellExprExecutor p, AnnotateType p, ForAllPhases Ord p) => EngineCtx p q s -> Solver p q s a -> ST.ST s a
runSolver ctx (Solver unificationM) =
  either error id <$> Unification.runUnificationM (_subtyping ctx) (_rewriteRules ctx) (evalStateT unificationM ctx)

------------------------------------------------------------
-- Solver monad operations
------------------------------------------------------------

liftUnification :: Unification.UnificationM p s a -> Solver p q s a
liftUnification = Solver . lift

liftST :: ST.ST s a -> Solver p q s a
liftST = liftUnification . Unification.liftST

takeSnapshot :: Solver p q s (ST.Snapshot s)
takeSnapshot = liftST ST.snapshot

restoreSnapshot :: ST.Snapshot s -> Solver p q s ()
restoreSnapshot = liftST . ST.restore

-- | Convert a PureTerm to RefTerm and update the solver's current mapping
refTerm :: (ForAllPhases Ord p, AnnotateType p) => PureTerm' p -> Solver p q s (RefTerm p s)
refTerm = liftUnification . Unification.refTerm'


-- | Convert a RefTerm back to a pure term
pureTerm :: (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> Solver p q s (PureTerm' p)
pureTerm = liftUnification . Unification.pureTerm''

-- | Generate unique variables in a given pure term
uniqueTerm :: (HaskellExprRename p, ForAllPhases Ord p) => PureTerm' p -> Solver p q s (PureTerm' p)
uniqueTerm term = do
   Solver $ lift $ zoom Unification.numUniqueVariables $ lift $ Renamer.renameState (Renamer.renameTerm term)

------------------------------------------------------------
-- Caching
------------------------------------------------------------

data CacheResult p   = InCache (PureTerm' p)        -- ^ indicates that the item is in the in-cache, together with its cached result
                     | InCacheMissing               -- ^ indicates that the item is in the in-cache but not in the out-cache
                     | NotInCache                   -- ^ indicates that the item is not in the in-cache

-- | Add a term to the cache together with its unification result 
addToCache :: (HaskellExprRename p, ForAllPhases Ord p, Show (PureTerm' p), AnnotateType p) => PureTerm' p -> Solver p q s ()
addToCache t = do
  -- use a normalizer that results in variables without unique names
  -- this is important so that the cache satisfies the ascending chain condition
  let t' = Renamer.unrenameTerm t
  tr <- refTerm t
  mapping <- gets (^. searchCtx . currentMapping)
  tground <- liftST $ Unification.pureTerm tr mapping
  -- Only if the term is ground can it be added to the cache
  when (isTermGround tground) $ do
    cachedTerm <- liftST $ Unification.pureTermGround tr mapping
    modify (over outCache (Map.insertWith Set.union t' (Set.singleton cachedTerm)))

-- | Adds an element to the in-cache if there is no already existing term that unifies with this one
-- in the cache 
addToInCache :: (HaskellExprRename p, ForAllPhases Ord p, HaskellExprExecutor p, AnnotateType p)
             => PureTerm' p
            -> Unification.VariableMapping p s
            -> [InOut p s]
            -> Solver p q s [InOut p s]
addToInCache t mapping theInCache = do
  t' <- refTerm t
  ifM (inCache t' theInCache mapping) (return theInCache) (return $ t : theInCache)

-- | Looks up the result(s) from the term in the out-cache
lookupCache :: (HaskellExprRename p, AnnotateType p, HaskellExprExecutor p, ForAllPhases Ord p)
            => PureTerm' p -> Solver p q s (Set (PureTerm' p))
lookupCache queryTerm = do
  cache <- gets (^. outCache)
  -- ensure that the variables are unique in both the queryTerm and the key so that
  -- they are not unified forever.
  queryTerm' <- uniqueTerm queryTerm
  queryRef <- refTerm queryTerm'
  -- check whether one of the keys matches the query
  results <- mapM (checkCacheEntry queryRef) (Map.toList cache)
  return $ Set.unions (catMaybes results)
  where
    checkCacheEntry queryRef (cacheKey, cacheResults) = do
      snapshot <- takeSnapshot
      -- ensures that variables in the key are unique so that they
      -- are not unified forever. Alternatively we could also store
      -- and save snapshots here.
      cacheKey' <- uniqueTerm cacheKey
      cacheKeyRef <- refTerm cacheKey'
      unified <- doesUnify queryRef cacheKeyRef
      restoreSnapshot snapshot
      return $ if unified then Just cacheResults else Nothing

-- | Unifies the query with the solutions from the out-cache
cachedSolutions  :: (HaskellExprRename p, AnnotateType p, HaskellExprExecutor p, ForAllPhases Ord p, Show (PureTerm' p))
                  => PureTerm' p                 -- ^ Original query term
                  -> Solver p q s [Map String (PureTerm' p)]  -- ^ Returns unified results as pure terms
cachedSolutions query = do
  -- Use lookupCache to find all matching cache values
  cacheValues <- lookupCache query


  -- For each cache value, try to unify with goal and collect successful results
  catMaybes <$> mapM processValue (Set.toList cacheValues)
  where
    processValue cacheValue = do
      modify (over (searchCtx . currentMapping) (const Map.empty))
      snapshot <- takeSnapshot
      query' <- refTerm query
      cacheValueRef <- refTerm cacheValue

      unifyResult <- unify query' cacheValueRef
      case unifyResult of
        Right _ -> do
          -- Unification succeeded, capture the result as pure term
          mapping <- gets (^. searchCtx . currentMapping)
          Just <$> liftST (Unification.buildMapping mapping) <* restoreSnapshot snapshot
        Left _ -> do
    -- Unification failed, restore snapshot and continue
          restoreSnapshot snapshot
          return Nothing

-- | Checks whether the given term unifies with a term in the in-cache
inCache :: (HaskellExprRename p, AnnotateType p, HaskellExprExecutor p, ForAllPhases Ord p)
        => RefTerm p s
        -> [InOut p s]
        -> Unification.VariableMapping p s
        -> Solver p q s Bool
inCache t inCache _mapping = or <$> mapM (uniqueTerm >=> refTerm >=> doesUnify t) inCache

------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------

-- | Unifies two terms together or returns Left if the terms
-- cannot be unified.
unify :: (ForAllPhases Ord p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
      => RefTerm p s
      -> RefTerm p s
      -> Solver p q s (Either String ())
unify left right =
  liftUnification $ fmap Right (Unification.unifyTerms left right) `catchError` (return . Left)

-- | Returns "True" if the given terms can/are unified
doesUnify :: (ForAllPhases Ord p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
          => RefTerm p s
          -> RefTerm p s
          -> Solver p q s Bool
doesUnify t1 t2 = do
 snapshot <- takeSnapshot
 isRight <$> unify t1 t2 <* restoreSnapshot snapshot

-- | Force the unification of a pure term and returns the unified pure term
forceUnify :: (ForAllPhases Ord p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
           => PureTerm' p
           -> Solver p q s (PureTerm' p)
forceUnify t1 =
  refTerm t1 >>= pureTerm

------------------------------------------------------------
-- Core search functions
------------------------------------------------------------

-- | Initialize the queue with the query
initialWL :: (ForAllPhases Ord p, Queue q, AnnotateType p) => PureTerm' p -> Solver p q s ()
initialWL query = do
  refQuery <- refTerm query
  snapshot <- takeSnapshot
  let initialState = SearchState 0 [SearchGoal "initial" refQuery] snapshot []
  modify (over (searchCtx . searchQueue) (enqueue initialState))

-- | Main entry point for solving a query - returns lazy list of all solutions
solve :: (AnnotateType p, HaskellExprExecutor p, Queue q, HaskellExprRename p, ForAllPhases Ord p, Show (PureTerm' p))
      => PureTerm' p -> Solver p q s [Map String (PureTerm' p)]
solve = solveUntilStable

-- | Solve a single step: dequeue one state and process it
-- Returns Nothing if queue is empty, Just (Left solution) if solution found,
-- Just (Right ()) if search state was processed and search should continue
solveSingle :: forall p q s . (AnnotateType p, HaskellExprExecutor p, Queue q, HaskellExprRename p, ForAllPhases Ord p, Show (PureTerm' p))
            => Solver p q s (Maybe (Either (Map String (PureTerm' p)) ()))
solveSingle = do
  queue <- gets (^. searchCtx . searchQueue)
  case dequeue queue of
    Nothing -> return Nothing -- Queue empty
    Just (state, restQueue) -> do
      modify (set (searchCtx . searchQueue) restQueue)
      restoreSnapshot (state ^. searchSnapshot)

      case state ^. searchGoals of
        [] -> do
          -- No goals left - we have a solution
          mapping <- gets (^. searchCtx . currentMapping)
          result <- liftST $ mapM (\cell -> Unification.pureTerm (Atom cell (typeAnnot (Proxy @p) AnyType) dummyRange) mapping) mapping
          -- Update the out-cache by adding the 'whenSucceeds' goals to it
          mapM_ addToCache  (state ^. searchWhenSucceeds)
          return $ Just (Left result)
        (goal:remainingGoals) -> do
          expandGoal goal remainingGoals (state ^. searchWhenSucceeds)
          return $ Just (Right ())

-- | Solve until the out-cache stabilizes (no longer changes between iterations)
solveUntilStable :: (AnnotateType p, HaskellExprExecutor p, Queue q, HaskellExprRename p, ForAllPhases Ord p, Show (PureTerm' p))
                 => PureTerm' p -> Solver p q s [Map String (PureTerm' p)]
solveUntilStable query = do
  initialCache <- gets (^. outCache)
  initialWL query
  _solutions <- solveAll
  finalCache <- gets (^. outCache)

  -- If cache changed, restart from the original query
  if initialCache == finalCache
    then cachedSolutions query
    else solveUntilStable query

-- | Collect all solutions by repeatedly calling solveSingle
solveAll :: (AnnotateType p, HaskellExprExecutor p, Queue q, HaskellExprRename p, ForAllPhases Ord p, Show (PureTerm' p))
         => Solver p q s [Map String (PureTerm' p)]
solveAll = do
  result <- solveSingle
  case result of
    Nothing -> return []  -- No more states to process
    Just (Left solution) -> (solution:) <$> solveAll  -- Found solution, continue
    Just (Right ()) -> solveAll  -- Processed state, continue

------------------------------------------------------------
-- Engine rules
------------------------------------------------------------

-- | Continue solving with the given remaining goals
continue :: (Queue q) => [SearchGoal p s] -> [InOut p s] -> Solver p q s ()
continue remainingGoals whenSucceeds = do
  newState <- SearchState 0 remainingGoals <$> takeSnapshot <*> pure whenSucceeds
  modify (over (searchCtx . searchQueue) (enqueue newState))

-- | Expand a goal by trying all matching rules
expandGoal :: (AnnotateType p, HaskellExprRename p, HaskellExprExecutor p, Queue q, ForAllPhases Ord p)
           => SearchGoal p s
           -> [SearchGoal p s]
           -> [InOut p s]
           -> Solver p q s ()
expandGoal (SearchGoal _ruleName goal) remainingGoals whenSucceeds = do
  mapping <- gets (^. searchCtx . currentMapping)
  goal' <- liftST $ Unification.pureTerm goal mapping
  case goal of
      -- Functors: look for rules with the name of the functor in its
      -- conclusion, add that rule as a goal to the context.
      Functor name _ _ _ -> do
        rules <- findMatchingRules name
        isInCache <- inCache goal whenSucceeds mapping
        whenSucceeds' <- addToInCache goal' mapping whenSucceeds
        mapM_ (processRule goal remainingGoals whenSucceeds' isInCache) rules

      Transition nam _from _to _ _s -> do
        -- Transitions: look for rules with the transition name in the conclusion
        rules <- findMatchingRules nam
        isInCache <- inCache goal whenSucceeds mapping
        whenSucceeds' <- addToInCache goal' mapping whenSucceeds
        mapM_ (processRule goal remainingGoals whenSucceeds' isInCache) rules

      Eqq left right _ _ -> do
        -- Equality: try to unify, if it succeeds then = succeeds
        either (const (return ()) . Debug.traceShowId) (const $ continue remainingGoals whenSucceeds) =<< unify left right
      Neq left right _ _ -> do
        -- Inequality: fail if unifies, otherwise succeed
        either (const $ continue remainingGoals whenSucceeds) (const $ return ()) =<< unify left right

      IncludedIn vrr set _ -> do
        expandSet vrr set remainingGoals whenSucceeds
      _ -> return () -- Variables and other terms can't be expanded

-- | Expand the elements of a set into a variable by generating subgoals for
-- each element of the set, requires that the term referring to the set is
-- already unified with a "SetOfTerms" term.
expandSet :: forall p q s . (AnnotateType p, HaskellExprRename p, HaskellExprExecutor p, Queue q, ForAllPhases Ord p)
          => String        -- X in y
          -> RefTerm p s   -- x in Y
          -> [SearchGoal p s] -- remaining goals
          -> [InOut p s]      -- in-cache
          -> Solver p q s ()
expandSet nam set remainingGoals whenSucceeds = do
  vrrTerm  <- refTerm (Atom (Identity nam) (typeAnnot (Proxy @p) AnyType) dummyRange)
  setTerm  <- fromMaybe (error "non-ground sets are not supported")  <$> liftST (Unification.termValue set)
  case setTerm of
    SetOfTerms ts _ _ -> mapM_ (\t -> continue (SearchGoal "SetIn" (Eqq vrrTerm t (typeAnnot (Proxy @p) AnyType) dummyRange) : remainingGoals) whenSucceeds) ts

    _ -> error "unreachable branch found, set terms should only resolve to sets, otherwise type check is unsound"

------------------------------------------------------------
-- Rule processing functions
------------------------------------------------------------

-- | Find all rules that could potentially match a functor name
findMatchingRules :: String -> Solver p q s [RuleDecl' p]
findMatchingRules functorName = do
  gets (Set.toList . Map.findWithDefault Set.empty functorName . (^. conclusionFunctors))

-- | Try to unify a goal with a rule and add new search states
processRule :: forall p q s . (AnnotateType p, Queue q, HaskellExprRename p, HaskellExprExecutor p, ForAllPhases Ord p)
            => RefTerm p s      -- ^ the unification goal
            -> [SearchGoal p s] -- ^ list of remainong goals
            -> [InOut p s]      -- ^ in-cache
            -> Bool             -- ^ whether the goal was originally in the in-cache
            -> RuleDecl' p      -- ^ the rule to match with
            -> Solver p q s ()
processRule goal remainingGoals whenSucceeds0 isInCache rule = do
  RuleDecl ruleName precedents consequents _ <- Solver $ lift $ zoom Unification.numUniqueVariables $ lift $ Renamer.renameRuleState rule

  -- TODO: the other consequents at the moment are only **secondary** meaning that they are resolved when the main goals
  -- are resolved by using the in-cache.
  case consequents of
    (consequent:consequents) -> do
      refConsequent <- refTerm consequent
      unifiedConsequents <- mapM forceUnify consequents

      -- Add the the other consequents to the in-cache (i.e., whenSucceeds)
      let whenSucceeds' = unifiedConsequents ++ whenSucceeds0

      -- Create unification goal: current goal = consequent
      let unificationGoal = SearchGoal ("Csq-" ++ ruleName) (Eqq goal refConsequent (typeAnnot (Proxy @p) AnyType) dummyRange)

      if null precedents
        then do
          -- Rules WITHOUT precedents: Process directly without caching
          continue (unificationGoal : remainingGoals) whenSucceeds'
        else do
          -- Rules WITH precedents: Apply caching logic
          mapping <- gets (^. searchCtx . currentMapping)
          pureGoal <- liftST $ Unification.pureTerm goal mapping

          if isInCache
            then do -- Goal is in whenSucceeds (in-cache)
              cachedResults <- lookupCache pureGoal
              if Set.null cachedResults
                then continue remainingGoals whenSucceeds' -- Out-cache empty: skip this rule entirely
                else do
                  -- Out-cache has results: create Eqq goals for each cached result
                  let resultsList = Set.toList cachedResults
                  mapM_ (processResult "In" remainingGoals whenSucceeds') resultsList
            else do -- Goal not in-cache: add it, process precedents, and check out-cache
              -- Check out-cache for existing results and create additional search states
              cachedResults <- lookupCache pureGoal
              unless (Set.null cachedResults) $ do
                let resultsList = Set.toList cachedResults
                mapM_ (processResult "Old" remainingGoals whenSucceeds') resultsList

              -- Also proceed with normal rule processing
              precedentRefs <- mapM refTerm precedents
              let precedentGoals = map (SearchGoal ruleName) precedentRefs
              let newGoals = unificationGoal : precedentGoals ++ remainingGoals
              continue newGoals whenSucceeds'
      where
        processResult prefix remainingGoals whenSucceeds' cachedResult = do
          cachedRef <- refTerm cachedResult
          let eqqGoal = SearchGoal ("Cached-" ++ prefix ++ "-" ++ ruleName) (Eqq goal cachedRef (typeAnnot (Proxy @p) AnyType) dummyRange)
          continue (eqqGoal : remainingGoals) whenSucceeds'
    _ -> error $ "Rule " ++ ruleName ++ " has " ++ show (length consequents) ++ " consequents, expected to be 1 or more"

------------------------------------------------------------
-- Inspection
------------------------------------------------------------

getQueueSize :: Queue q => Solver p q s Int
getQueueSize =
  gets ((length . queueToList) . (^. searchCtx . searchQueue))
