{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Solver where

import Control.Applicative
import Control.Lens
import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State
import Data.Functor.Identity
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Language.AST
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Renamer as Renamer
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist
import qualified Data.List as List
import Language.Solver.Unification (RefTerm)
import qualified Debug.Trace as Debug

------------------------------------------------------------
-- Search data structures
------------------------------------------------------------

-- | A new goal for search, keeps track of the term
-- to be solved and also from which rule this term
-- originated.
data SearchGoal s = SearchGoal
  { _goalRuleName :: String,
    _goalTerm :: RefTerm s }

data SearchState s = SearchState
  { _searchGoals :: [SearchGoal s],
    _searchSnapshot :: ST.Snapshot s,
    _searchMapping :: Unification.VariableMapping s
  }

data SearchCtx q s = SearchCtx
  { _searchQueue :: q (SearchState s),
    _currentMapping :: Unification.VariableMapping s
  }

type SolverResult = Either String (Map String PureTerm)

$(makeLenses ''SearchState)
$(makeLenses ''SearchCtx)


------------------------------------------------------------
-- Core data structures
------------------------------------------------------------

data EngineCtx q s = EngineCtx
  { -- | Mapping from functor names to their appropriate rule, used in backwards reasoning
    _conclusionFunctors :: Map String (Set RuleDecl),
    -- | Unique variables counter
    _numUniqueVariables :: Int,
    -- | Search context for query resolution
    _searchCtx :: SearchCtx q s
  }

-- | Create an empty search context
emptySearchCtx :: (Queue q) => SearchCtx q s
emptySearchCtx = SearchCtx emptyQueue Map.empty

-- | Create an empty solver engine context
emptyEngineCtx :: (Queue q) => EngineCtx q s
emptyEngineCtx = EngineCtx Map.empty 0 emptySearchCtx

$(makeLenses ''EngineCtx)

-- | Associate the given functor with the given rule, indicating that it can be found in the conclusion of that rule.
addConclusionFunctor :: String -> RuleDecl -> EngineCtx q s -> EngineCtx q s
addConclusionFunctor nam decl =
  over conclusionFunctors (Map.insertWith Set.union nam (Set.singleton decl))

-- | Construct an initial context from the rules defined the program
fromRules :: (Queue q) => [RuleDecl] -> EngineCtx q s
fromRules = foldr visit emptyEngineCtx
  where
    visit rule@(RuleDecl _ precedent consequent _) =
      flip (foldr (`addConclusionFunctor` rule)) (foldMap functorName consequent)

------------------------------------------------------------
-- Monad context
------------------------------------------------------------

-- | The solver monadic context
newtype Solver q s a = Solver {getSolver :: StateT (EngineCtx q s) (ST.ST s) a}
  deriving (Applicative, Functor, Monad, MonadState (EngineCtx q s))


runSolver :: EngineCtx q s -> Solver q s a -> ST.ST s a
runSolver ctx (Solver stateT) = evalStateT stateT ctx

------------------------------------------------------------
-- Solver monad operations
------------------------------------------------------------

liftST :: ST.ST s a -> Solver q s a
liftST = Solver . lift

takeSnapshot :: Solver q s (ST.Snapshot s)
takeSnapshot = liftST ST.snapshot

restoreSnapshot :: ST.Snapshot s -> Solver q s ()
restoreSnapshot = liftST . ST.restore

-- | Convert a PureTerm to RefTerm and update the solver's current mapping
refTerm :: PureTerm -> Solver q s (RefTerm s)
refTerm term = do
  mapping <- gets (^. searchCtx . currentMapping)
  (refTerm, newMapping) <- liftST $ Unification.refTerm term mapping
  modify (set (searchCtx . currentMapping) newMapping)
  return refTerm

------------------------------------------------------------
-- Auxiliary functions
------------------------------------------------------------

unify :: RefTerm s -> RefTerm s -> Solver q s (Either String ())
unify left  =
  liftST .  runExceptT .  Unification.unifyTerms left

------------------------------------------------------------
-- Core search functions
------------------------------------------------------------

-- | Initialize the queue with the query
initialWL :: Queue q => PureTerm -> Solver q s ()
initialWL query = do
  refQuery <- refTerm query
  mapping <- gets (^. searchCtx . currentMapping)
  snapshot <- takeSnapshot
  let initialState = SearchState [SearchGoal "initial" refQuery] snapshot mapping
  modify (over (searchCtx . searchQueue) (enqueue initialState))

-- | Main entry point for solving a query - returns lazy list of all solutions
solve :: (Queue q) => PureTerm -> Solver q s [Map String PureTerm]
solve query = initialWL query >> solveAll

-- | Solve a single step: dequeue one state and process it
-- Returns Nothing if queue is empty, Just (Left solution) if solution found,
-- Just (Right ()) if search state was processed and search should continue
solveSingle :: (Queue q) => Solver q s (Maybe (Either (Map String PureTerm) ()))
solveSingle = do
  queue <- gets (^. searchCtx . searchQueue)
  case dequeue queue of
    Nothing -> return Nothing -- Queue empty
    Just (state, restQueue) -> do
      modify (set (searchCtx . searchQueue) restQueue)
      restoreSnapshot (state ^. searchSnapshot)
      modify (set (searchCtx . currentMapping) (state ^. searchMapping))

      case state ^. searchGoals of
        [] -> do
          -- No goals left - we have a solution
          mapping <- gets (^. searchCtx . currentMapping)
          result <- liftST $ mapM (\cell -> Unification.pureTerm (Atom cell dummyRange) mapping) mapping
          return $ Just (Left result)
        (goal:remainingGoals) -> do
          expandGoal goal remainingGoals
          return $ Just (Right ())

-- | Collect all solutions by repeatedly calling solveSingle
solveAll :: (Queue q) => Solver q s [Map String PureTerm]
solveAll = do
  result <- solveSingle
  case result of
    Nothing -> return []  -- No more states to process
    Just (Left solution) -> (solution:) <$> solveAll  -- Found solution, continue
    Just (Right ()) -> solveAll  -- Processed state, continue

-- | Continue solving with the given remaining goals
continue :: (Queue q) => [SearchGoal s] -> Solver q s ()
continue remainingGoals = do
  newState <- SearchState remainingGoals <$> takeSnapshot <*> gets (^. searchCtx . currentMapping)
  modify (over (searchCtx . searchQueue) (enqueue newState))

-- | Expand a goal by trying all matching rules
expandGoal :: (Queue q) => SearchGoal s -> [SearchGoal s] -> Solver q s ()
expandGoal (SearchGoal ruleName goal) remainingGoals = do
  case goal of
    -- Functors: look for rules with the name of the functor in its
    -- conclusion, add that rule as a goal to the context.
    Functor name _ _ -> do
      rules <- findMatchingRules name
      mapM_ (processRule goal remainingGoals) rules
    
    Transition nam from to s -> do
      -- Transitions: look for rules with the transition name in the conclusion
      rules <- findMatchingRules nam
      mapM_ (processRule goal remainingGoals) rules
    
    Eqq left right _ -> do
      -- Equality: try to unify, if it succeeds then = succeeds
      either (const $ return ()) (const $ continue remainingGoals) =<< unify left right
    Neq left right _ -> do
      -- Inequality: fail if unifies, otherwise succeed
      either (const $ continue remainingGoals) (const $ return ()) =<< unify left right
    _ -> return () -- Variables and other terms can't be expanded

------------------------------------------------------------
-- Rule processing functions
------------------------------------------------------------

-- | Find all rules that could potentially match a functor name
findMatchingRules :: String -> Solver q s [RuleDecl]
findMatchingRules functorName = do
  gets (Set.toList . Map.findWithDefault Set.empty functorName . (^. conclusionFunctors))

-- | Try to unify a goal with a rule and add new search states
processRule :: (Queue q) => RefTerm s -> [SearchGoal s] -> RuleDecl -> Solver q s ()
processRule goal remainingGoals rule = do
  RuleDecl ruleName precedents consequents _ <- Solver $ zoom numUniqueVariables $ Renamer.renameRuleState rule

  -- Assert that there's only one consequent for now
  case consequents of
    [consequent] -> do
      refConsequent <- refTerm consequent
      precedentRefs <- mapM refTerm precedents

      -- Create unification goal: current goal = consequent
      let unificationGoal = SearchGoal ruleName (Eqq goal refConsequent dummyRange)
      let precedentGoals = map (SearchGoal ruleName) precedentRefs
      let newGoals = unificationGoal : precedentGoals ++ remainingGoals

      continue newGoals
    _ -> error $ "Rule " ++ ruleName ++ " has " ++ show (length consequents) ++ " consequents, expected exactly 1"

------------------------------------------------------------
-- Inspection
------------------------------------------------------------

getQueueSize :: Queue q => Solver q s Int
getQueueSize =
  gets ((length . queueToList) . (^. searchCtx . searchQueue))
