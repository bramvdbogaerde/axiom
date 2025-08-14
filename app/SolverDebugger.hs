{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module SolverDebugger where

import Language.Solver
import Language.AST
import Language.Parser (parseProgram, parseTerm)
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Unification as Unification
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Lens ((^.))
import Data.List (intercalate)
import GHC.IO.Handle
import System.IO
import qualified Data.List as List
import qualified Text.Printf as Printf
import Control.Monad
import System.Timeout (timeout)

-- | A single step trace entry
data TraceEntry = TraceEntry
  { traceStep :: Int
  , traceAction :: String
  , traceGoals :: [String]
  , traceQueueSize :: Int
  , traceSolution :: Maybe (Map String PureTerm)
  } deriving (Show)

-- | The complete trace of a solving session
type SolverTrace = [TraceEntry]

-- | Pretty print a trace entry
prettyTraceEntry :: TraceEntry -> String
prettyTraceEntry entry = unlines
  [ "Step " ++ show (traceStep entry) ++ ": " ++ traceAction entry
  , "  Goals: [" ++ intercalate ", " (traceGoals entry) ++ "]"
  , "  Queue size: " ++ show (traceQueueSize entry)
  , case traceSolution entry of
      Just sol -> "  Solution found: " ++ show sol
      Nothing -> "  No solution yet"
  ]

-- | Pretty print the entire trace
prettyTrace :: SolverTrace -> String
prettyTrace = foldMap prettyTraceEntry

-- | Traced solver monad - wraps the solver with tracing capability
newtype TracedSolver q s a = TracedSolver
  { runTracedSolver :: WriterT SolverTrace (Solver q s) a }
  deriving (Functor, Applicative, Monad, MonadWriter SolverTrace)

-- | Lift a solver action into the traced solver
liftSolver :: Solver q s a -> TracedSolver q s a
liftSolver = TracedSolver . lift

-- | Add a trace entry
trace :: TraceEntry -> TracedSolver q s ()
trace = tell . List.singleton

-- | Convert goal terms to string representation for tracing
goalToString :: Unification.VariableMapping s -> SearchGoal s -> TracedSolver q s String
goalToString mapping (SearchGoal ruleName goal) =
   flip (Printf.printf "%s via %s") ruleName . show <$> liftSolver (liftST $ Unification.pureTerm goal mapping)

-- | Traced version of solveSingle with debugging information
tracedSolveSingle :: (Queue q) => Int -> TracedSolver q s (Maybe (Either (Map String PureTerm) ()))
tracedSolveSingle stepNum = do
  queueSize <- liftSolver getQueueSize

  -- Get the current state's goals before processing
  currentGoals <- liftSolver $ do
    queue <- gets (^. searchCtx . searchQueue)
    case dequeue queue of
      Nothing -> return []
      Just (state, _) -> return (state ^. searchGoals)

  -- Get current mapping for goal conversion
  mapping <- liftSolver $ gets (^. searchCtx . currentMapping)
  goalStrings <- mapM (goalToString mapping) currentGoals

  result <- liftSolver solveSingle

  case result of
    Nothing -> do
      trace $ TraceEntry stepNum "Queue empty - search finished" [] 0 Nothing
      return Nothing
    Just (Left solution) -> do
      trace $ TraceEntry stepNum "Solution found!" goalStrings queueSize (Just solution)
      return $ Just (Left solution)
    Just (Right ()) -> do
      newQueueSize <- liftSolver getQueueSize
      trace $ TraceEntry stepNum "Processed search state" goalStrings newQueueSize Nothing
      return $ Just (Right ())

-- | Traced version of solveAll
tracedSolveAll :: (Queue q) => TracedSolver q s [Map String PureTerm]
tracedSolveAll = go 1 []
  where
    go stepNum solutions = do
      result <- tracedSolveSingle stepNum
      case result of
        Nothing -> return $ reverse solutions
        Just (Left solution) -> go (stepNum + 1) (solution : solutions)
        Just (Right ()) -> go (stepNum + 1) solutions

-- | Main traced solve function
tracedSolve :: (Queue q) => PureTerm -> TracedSolver q s [Map String PureTerm]
tracedSolve query = do
  liftSolver $ initialWL query
  trace $ TraceEntry 0 ("Starting to solve: " ++ show query) [] 1 Nothing
  tracedSolveAll

-- | Run a traced solver computation and return results with trace
runSolverWithTrace :: (Queue q) => EngineCtx q s -> TracedSolver q s a -> ST.ST s (a, SolverTrace)
runSolverWithTrace ctx tracedComp = do
  runSolver ctx $ runWriterT $ runTracedSolver tracedComp

-- | Debug solve a query and return solutions with trace  
debugSolve :: [RuleDecl] -> PureTerm -> IO ([Map String PureTerm], SolverTrace)
debugSolve rules query = do
  return $ ST.runST $ do
    let ctx = fromRules rules :: EngineCtx [] s
    runSolverWithTrace ctx (tracedSolve query)

-- | Interactive debugging session
debugSession :: FilePath -> IO ()
debugSession semFile = do
  putStrLn $ "Loading " ++ semFile ++ "..."
  content <- readFile semFile
  case parseProgram content of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right (Program decls _) -> do
      let rules = [rule | RulesDecl rules _ <- decls, rule <- rules]

      putStrLn "Loaded rules successfully. Enter queries to debug (or 'quit' to exit):"
      mapM_ print rules

      debugLoop rules
  where
    debugLoop rules = forever $ do
      putStr "debug> " >> hFlushAll stdout
      getLine >>=
        \case
          "quit" -> putStrLn "Goodbye!"
          query -> do
            case parseTerm query of
              Left err -> putStrLn $ "Parse error: " ++ show err
              Right term -> do
                (solutions, trace) <- debugSolve rules term
                putStrLn "\n=== TRACE ==="
                putStrLn $ prettyTrace trace
                putStrLn "=== RESULTS ==="
                if null solutions
                  then putStrLn "No solutions found."
                  else do
                    putStrLn $ "Found " ++ show (length solutions) ++ " solution(s):"
                    mapM_ (putStrLn . ("  " ++) . show) solutions
                putStrLn ""
