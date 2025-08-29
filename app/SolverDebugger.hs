{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module SolverDebugger where

import Language.Solver
import Language.AST
import Language.Parser (parseProgram, parseTerm)
import qualified Language.Solver.BacktrackingST as ST
import qualified Language.Solver.Unification as Unification
import Language.Solver.Worklist (Queue, dequeue)
import qualified Data.Map as Map
import Data.Map ( Map, Map )
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.State (gets)
import Control.Lens ((^.), (.~), (&), makeLenses)
import Data.List (intercalate)
import GHC.IO.Handle
import System.IO
import qualified Data.List as List
import qualified Text.Printf as Printf
import Control.Monad
import System.Timeout (timeout)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Read (readMaybe)
import Control.Lens.Setter ((?~))


-------------------------------------------------------------
-- Configuration
-------------------------------------------------------------

-- | Debugger configuration
data DebugConfig = DebugConfig
  { _configStepLimit :: Maybe Int  -- ^ Optional limit on number of solver steps
  , _configShowInternalVars :: Bool  -- ^ Whether to show internal variables in trace
  } deriving (Show)

-- | Debugger context combining config and user variables
data DebugContext = DebugContext
  { _debugConfig :: DebugConfig
  , _userVariables :: Set String  -- ^ Variables that came from the original query
  } deriving (Show)

$(makeLenses ''DebugConfig)
$(makeLenses ''DebugContext)

-- | Default configuration
defaultConfig :: DebugConfig
defaultConfig = DebugConfig Nothing False  -- Don't show internal variables by default

-- | Default context with empty user variables
defaultContext :: DebugContext
defaultContext = DebugContext defaultConfig Set.empty

-- | Create debug context from config and query
createDebugContext :: DebugConfig -> PureTerm -> DebugContext
createDebugContext config query = DebugContext config (atomNames query)

-------------------------------------------------------------
-- Tracing
-------------------------------------------------------------

-- | A single step trace entry
data TraceEntry = TraceEntry
  { traceStep :: Int
  , traceAction :: String
  , traceStateId :: Maybe Int  -- ^ Current search state ID being processed
  , traceGoals :: [String]
  , traceQueueSize :: Int
  , traceSolution :: Maybe (Map String PureTerm)
  , tracePartialMapping :: Map String PureTerm  -- ^ Current partial unification results
  , traceCacheSize :: Int  -- ^ Number of entries in the out-cache
  , traceOutCache :: Map String [String]  -- ^ Out-cache contents: key -> [results]
  , traceInCache :: [String]  -- ^ In-cache (whenSucceeds) contents
  } deriving (Show)

-- | The complete trace of a solving session
type SolverTrace = [TraceEntry]

-- | Pretty print a trace entry
prettyTraceEntry :: TraceEntry -> String
prettyTraceEntry entry = unlines $
  [ "Step " ++ show (traceStep entry) ++ 
    (case traceStateId entry of 
       Just stateId -> " [State #" ++ show stateId ++ "]"
       Nothing -> "") ++ 
    ": " ++ traceAction entry
  , "  Goals: [" ++ intercalate ", " (traceGoals entry) ++ "]"
  , "  Queue size: " ++ show (traceQueueSize entry)
  , "  Cache size: " ++ show (traceCacheSize entry)
  ] ++ 
  (if null (traceInCache entry)
    then ["  In-cache (whenSucceeds): []"]
    else ["  In-cache (whenSucceeds):"] ++ map ("    " ++) (traceInCache entry)) ++
  (if Map.null (traceOutCache entry)
    then ["  Out-cache: {}"]
    else ["  Out-cache:"] ++ formatCacheTable (traceOutCache entry)) ++
  [ if Map.null (tracePartialMapping entry)
      then "  Partial mapping: {}"
      else "  Partial mapping: " ++ show (tracePartialMapping entry)
  , case traceSolution entry of
      Just sol -> "  Solution found: " ++ show sol
      Nothing -> "  No solution yet"
  ]

-- | Format cache contents as a nice table
formatCacheTable :: Map String [String] -> [String]
formatCacheTable cache
  | Map.null cache = ["    (empty)"]
  | otherwise = 
      let entries = Map.toList cache
          maxKeyWidth = maximum (map (length . fst) entries)
          formatEntry (key, values) = 
            let paddedKey = key ++ replicate (maxKeyWidth - length key) ' '
            in if null values
                then "    " ++ paddedKey ++ " -> []"
                else "    " ++ paddedKey ++ " -> [" ++ intercalate ", " values ++ "]"
      in map formatEntry entries

-- | Pretty print the entire trace
prettyTrace :: SolverTrace -> String
prettyTrace = foldMap prettyTraceEntry

-------------------------------------------------------------
-- Monad
-------------------------------------------------------------

-- | Traced solver monad - wraps the solver with tracing capability and context
newtype TracedSolver q s a = TracedSolver
  { runTracedSolver :: ReaderT DebugContext (WriterT SolverTrace (Solver ParsePhase q s)) a }
  deriving (Functor, Applicative, Monad, MonadReader DebugContext, MonadWriter SolverTrace)

-- | Lift a solver action into the traced solver
liftSolver :: Solver ParsePhase q s a -> TracedSolver q s a
liftSolver = TracedSolver . lift . lift

-- | Add a trace entry
trace :: TraceEntry -> TracedSolver q s ()
trace = tell . List.singleton

-------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------

-- | Check step limit and either continue with action or stop with limit message
withStepLimit :: (Queue q) => Int -> TracedSolver q s a -> TracedSolver q s a -> TracedSolver q s a
withStepLimit stepNum continueAction stopAction = do
  context <- ask
  case context ^. debugConfig . configStepLimit of
    Just limit | stepNum > limit -> do
      partialMapping <- getCurrentPartialMapping
      cacheSize <- getCurrentCacheSize
      outCache <- getCurrentOutCache
      inCache <- getInCacheContents []  -- No specific whenSucceeds context here
      trace $ TraceEntry stepNum ("Step limit reached: " ++ show limit ++ " steps") Nothing [] 0 Nothing partialMapping cacheSize outCache inCache
      stopAction
    _ -> continueAction

-- | Get current partial mapping for tracing, filtered by configuration
getCurrentPartialMapping :: TracedSolver q s (Map String PureTerm)
getCurrentPartialMapping = return Map.empty
  -- context <- ask
  -- mapping <- liftSolver $ gets (^. searchCtx . currentMapping)
  -- fullMapping <- liftSolver $ liftST $ mapM (\cell -> Unification.pureTerm (Atom cell () dummyRange) mapping) mapping
  -- return $ if context ^. debugConfig . configShowInternalVars
  --   then fullMapping
  --   else Map.restrictKeys fullMapping (context ^. userVariables)

-- | Get current cache size for tracing
getCurrentCacheSize :: TracedSolver q s Int
getCurrentCacheSize = liftSolver $ gets (Map.size . (^. Language.Solver.outCache))

-- | Get current out-cache contents for tracing
getCurrentOutCache :: TracedSolver q s (Map String [String])
getCurrentOutCache = do
  cache <- liftSolver $ gets (^. Language.Solver.outCache)
  return $ Map.fromList $ map (\(key, values) -> (show key, map show (Set.toList values))) (Map.toList cache)

-- | Get current in-cache (whenSucceeds) contents for a search state
getInCacheContents :: [InOut ParsePhase s] -> TracedSolver q s [String]
getInCacheContents whenSucceeds = return $ map show whenSucceeds

-- | Convert goal terms to string representation for tracing
goalToString :: Unification.VariableMapping ParsePhase s -> SearchGoal ParsePhase s -> TracedSolver q s String
goalToString mapping (SearchGoal ruleName goal) =
   flip (Printf.printf "%s via %s") ruleName . show <$> liftSolver (liftST $ Unification.pureTerm goal mapping)

-- | Traced version of solveSingle with debugging information
tracedSolveSingle :: (Queue q) => Int -> TracedSolver q s (Maybe (Either (Map String PureTerm) ()))
tracedSolveSingle stepNum = do
  queueSize <- liftSolver getQueueSize

  -- Get the current state's info before processing
  (currentStateId, currentGoals, currentWhenSucceeds) <- liftSolver $ do
    gets (^. searchCtx . searchQueue) >>=
      (\case Nothing -> return (Nothing, [], [])
             Just (state, _) -> return (Just (state ^. searchStateId), state ^. searchGoals, state ^. searchWhenSucceeds)) . dequeue

  -- Get current mapping for goal conversion
  mapping <- liftSolver $ gets (^. searchCtx . currentMapping)
  goalStrings <- mapM (goalToString mapping) currentGoals

  result <- liftSolver solveSingle

  -- Get partial mapping, cache info for trace
  partialMapping <- getCurrentPartialMapping
  cacheSize <- getCurrentCacheSize
  outCache <- getCurrentOutCache
  inCache <- getInCacheContents currentWhenSucceeds

  case result of
    Nothing -> do
      trace $ TraceEntry stepNum "Queue empty - search finished" currentStateId [] 0 Nothing partialMapping cacheSize outCache inCache
      return Nothing
    Just (Left solution) -> do
      trace $ TraceEntry stepNum "Solution found!" currentStateId goalStrings queueSize (Just solution) partialMapping cacheSize outCache inCache
      return $ Just (Left solution)
    Just (Right ()) -> do
      newQueueSize <- liftSolver getQueueSize
      trace $ TraceEntry stepNum "Processed search state" currentStateId goalStrings newQueueSize Nothing partialMapping cacheSize outCache inCache
      return $ Just (Right ())

-- | Traced version of solveAll with step limit from config
tracedSolveAll :: (Queue q) => TracedSolver q s [Map String PureTerm]
tracedSolveAll = go 1 []
  where
    go stepNum solutions = withStepLimit stepNum continueStep (return $ reverse solutions)
      where
        continueStep = do
          result <- tracedSolveSingle stepNum
          case result of
            Nothing -> return $ reverse solutions
            Just (Left solution) -> go (stepNum + 1) (solution : solutions)
            Just (Right ()) -> go (stepNum + 1) solutions

-- | Main traced solve function with step limit from config and caching loop
tracedSolve :: (Queue q) => PureTerm -> TracedSolver q s [Map String PureTerm]
tracedSolve query = do
  partialMapping <- getCurrentPartialMapping
  cacheSize <- getCurrentCacheSize
  outCache <- getCurrentOutCache
  inCache <- getInCacheContents []
  trace $ TraceEntry 0 ("Starting to solve: " ++ show query) Nothing [] 0 Nothing partialMapping cacheSize outCache inCache
  tracedSolveUntilStable query 1
  liftSolver (cachedSolutions query)
  

-- | Traced version of solveUntilStable with cache iteration tracking
tracedSolveUntilStable :: (Queue q) => PureTerm -> Int -> TracedSolver q s [Map String PureTerm]
tracedSolveUntilStable query iteration = do
  partialMapping <- getCurrentPartialMapping
  cacheSize <- getCurrentCacheSize
  outCache <- getCurrentOutCache
  inCache <- getInCacheContents []
  trace $ TraceEntry 0 ("Cache iteration " ++ show iteration ++ ": solving " ++ show query) Nothing [] 0 Nothing partialMapping cacheSize outCache inCache
  
  initialCache <- liftSolver $ gets (^. Language.Solver.outCache)
  liftSolver $ initialWL query
  solutions <- tracedSolveAll
  finalCache <- liftSolver $ gets (^. Language.Solver.outCache)
  
  if initialCache == finalCache
    then do
      finalCacheSize <- getCurrentCacheSize
      finalOutCache <- getCurrentOutCache
      finalInCache <- getInCacheContents []
      trace $ TraceEntry 0 ("Cache stabilized after " ++ show iteration ++ " iteration(s)") Nothing [] 0 Nothing partialMapping finalCacheSize finalOutCache finalInCache
      return solutions
    else do
      newCacheSize <- getCurrentCacheSize
      newOutCache <- getCurrentOutCache
      newInCache <- getInCacheContents []
      trace $ TraceEntry 0 ("Cache changed, restarting (iteration " ++ show (iteration + 1) ++ ")") Nothing [] 0 Nothing partialMapping newCacheSize newOutCache newInCache
      tracedSolveUntilStable query (iteration + 1)

-- | Run a traced solver computation with context and return results with trace
runSolverWithTrace :: (Queue q) => DebugContext -> EngineCtx ParsePhase q s -> TracedSolver q s a -> ST.ST s (a, SolverTrace)
runSolverWithTrace context ctx tracedComp = do
  runSolver ctx $ runWriterT $ runReaderT (runTracedSolver tracedComp) context

-- | Debug solve a query with config and return solutions with trace  
debugSolve :: DebugConfig -> [RuleDecl] -> PureTerm -> IO ([Map String PureTerm], SolverTrace)
debugSolve config rules query = do
  return $ ST.runST $ do
    let ctx = fromRules rules :: EngineCtx ParsePhase [] s
    let debugCtx = createDebugContext config query
    runSolverWithTrace debugCtx ctx (tracedSolve query)

-------------------------------------------------------------
-- Interaction
-------------------------------------------------------------

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
      putStrLn "Use ':set steps N' to limit solver steps, ':show config' to view settings."
      mapM_ print rules

      debugLoop defaultConfig rules
  where
    debugLoop config rules = do
      putStr "debug> " >> hFlushAll stdout
      input <- getLine
      if | input == "quit" -> putStrLn "Goodbye!"
         | not (null input) && head input == ':' -> handleCommand input config rules
         | otherwise -> handleQuery input config rules

    handleCommand input config rules =
      case parseCommand' input config of
        Left err -> putStrLn err >> debugLoop config rules
        Right newConfig -> do
          putStrLn $ "Configuration updated: " ++ show newConfig
          debugLoop newConfig rules

    handleQuery input config rules = do
      case parseTerm input of
        Left err -> putStrLn $ "Parse error: " ++ show err
        Right term -> do
          (solutions, trace) <- debugSolve config rules term
          putStrLn "\n=== TRACE ==="
          putStrLn $ prettyTrace trace
          putStrLn "=== RESULTS ==="
          if null solutions
            then putStrLn "No solutions found."
            else do
              putStrLn $ "Found " ++ show (length solutions) ++ " solution(s):"
              mapM_ (putStrLn . ("  " ++) . show) (if config ^. configShowInternalVars
                                                      then solutions
                                                      else map (`Map.restrictKeys` atomNames term) solutions)
          
      debugLoop config rules

-- | Parse a command and update the given config
parseCommand' :: String -> DebugConfig -> Either String DebugConfig
parseCommand' (':':rest) currentConfig =
  case words rest of
    ["set", "steps", "off"] -> Right $ currentConfig & configStepLimit .~ Nothing
    ["set", "steps", stepStr] ->
      case readMaybe stepStr of
        Just steps | steps > 0 -> Right $ currentConfig & configStepLimit ?~ steps
        Just _ -> Left "Error: steps must be positive"
        Nothing -> Left "Error: invalid number for steps"
    ["set", "internal", flag] -> Right $ currentConfig & configShowInternalVars .~ (flag == "on")
    ["show", "config"] -> Left $ show currentConfig
    _ -> Left "Error: unknown command. Try ':set steps N', ':set steps off', ':set internal on/off'"
parseCommand' _ _ = Left "Error: commands must start with ':'"
