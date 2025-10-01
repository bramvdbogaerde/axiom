{-# LANGUAGE RecordWildCards, TypeApplications #-}
module Main where

import Prelude hiding (lex)
import Language.AST
import Language.Parser
import Language.TypeCheck (runChecker', CheckingContext(..))
import Language.CodeGen
import Language.ImportResolver (resolveImportsFromFile, ImportError(..))
import Control.Monad
import Data.Maybe (catMaybes)
import Data.List (stripPrefix)
import System.Console.ANSI
import System.Exit
import System.Process
import System.FilePath
import Reporting (printError, printImportError)
import Options.Applicative
import qualified LanguageServer
import qualified Language.SolverDebugger as SolverDebugger
import qualified Latex.Entrypoint as Latex
import Language.Solver
import qualified Language.Solver.BacktrackingST as ST
import System.Timeout (timeout)
import Text.Pretty.Simple (pPrint)

-------------------------------------------------------------
-- Data types
-------------------------------------------------------------

-- | Options for commands that require an input file
newtype InputOptions = InputOptions
  { inputFile :: String  -- ^ Path to the input file to process
  } deriving Show

-- | Global options containing the action to execute
newtype GlobalOptions = GlobalOptions
  { runAction :: IO ()   -- ^ The IO action to run based on the parsed command
  }

-- | Options for the "codegen" command
data CodeGenOptions = CodeGenOptions {
                        verbose :: Bool, -- ^ whether the code generation should output the typing context
                        enableDebugger :: Bool -- ^ whether to include debugger support in generated code
                      }
                    deriving (Ord, Eq, Show)

-------------------------------------------------------------
-- Command line parsers
-------------------------------------------------------------

-- | Parser for input file options
inputOptionsParser :: Parser InputOptions
inputOptionsParser = InputOptions
  <$> strArgument
      ( metavar "FILE"
     <> help "Input file to process" )

-- | Verbosity flag
verbosityFlag :: Parser Bool
verbosityFlag =
    switch ( short 'v' <> help "Enable verbose output (i.e., typing context is printed to stderr)" )

-- | Parser for the code generation options
codegenOptionsParser :: Parser CodeGenOptions
codegenOptionsParser = CodeGenOptions 
  <$> verbosityFlag
  <*> switch ( short 'd' <> long "debug" <> help "Include debugger support in generated code" )

-- | Parser for the 'check' subcommand
checkCommand :: Parser (IO ())
checkCommand = runCheckCommand <$> inputOptionsParser <*> verbosityFlag

-- | Parser for the 'debug' subcommand
debugCommand :: Parser (IO ())
debugCommand = runDebugCommand <$> inputOptionsParser

-- | Parser for the 'lsp' subcommand  
lspCommand :: Parser (IO ())
lspCommand = pure $ do
  exitCode <- LanguageServer.runLanguageServer
  exitWith $ if exitCode == 0 then ExitSuccess else ExitFailure exitCode

-- | Parser for the 'codegen' subcommand
codegenCommand :: Parser (IO ())
codegenCommand = runCodegenCommand <$> inputOptionsParser <*> codegenOptionsParser

-- | Parser for the 'runcodegen' subcommand
runcodegenCommand :: Parser (IO ())
runcodegenCommand = runRuncodegenCommand <$> inputOptionsParser

-- | Parser for the 'runsolver' subcommand
runsolverCommand :: Parser (IO ())
runsolverCommand = runRunsolverCommand <$> inputOptionsParser

-- | Parser for the 'latex' subcommand
latexCommand :: Parser (IO ())
latexCommand = Latex.runLatexCommand <$> Latex.latexOptionsParser

-- | Parser for all available subcommands
commandParser :: Parser (IO ())
commandParser = subparser  ( command "check"
    (info checkCommand (progDesc "Type check a program"))
 <> command "debug"
    (info debugCommand (progDesc "Start interactive solver debugger"))
 <> command "lsp"
    (info lspCommand (progDesc "Start Language Server Protocol server"))
 <> command "codegen"
    (info codegenCommand (progDesc "Generate code from a typechecked program"))
 <> command "runcodegen"
    (info runcodegenCommand (progDesc "Generate and execute code from a typechecked program"))
 <> command "runsolver"
    (info runsolverCommand (progDesc "Run solver on test queries from a program"))
 <> command "latex"
    (info latexCommand (progDesc "Generate LaTeX files from a program"))
  )

-- | Top-level parser for global options
globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions <$> commandParser

-- | Parser info for the entire program
opts :: ParserInfo GlobalOptions
opts = info (globalOptions <**> helper)
  ( fullDesc
 <> progDesc "Analysis language tools"
 <> header "analysislang - tools for analysis language programs" )

-------------------------------------------------------------
-- Utility functions
-------------------------------------------------------------

-- | Load a file and parse it into an AST, returning both the source and parsed program
-- Returns Left with error info if parsing fails
loadAndParseFile :: String -> IO (Either (String, ImportError) (String, Program))
loadAndParseFile filename = do
  -- TODO: the imports should also be considered, thus we should return a Map, mapping filenames to their programs, this is already constructed in the ImportResolver but never exposed.
  contents <- readFile filename
  program <- resolveImportsFromFile filename
  return $ case program of
    Left err -> Left (contents, err)
    Right ast -> Right (contents, ast)

-- | Print text in green color for success messages
printGreen :: String -> IO ()
printGreen txt = setSGR [SetColor Foreground Vivid Green]
              >> putStrLn txt
              >> setSGR [Reset]

-- | Print a success message when type checking completes
printSuccess :: IO ()
printSuccess = printGreen "Program typechecked successfully"

-- | Extract test queries from comments that start with "test:"
extractTestQueries :: [Comment' p] -> [String]
extractTestQueries comments = catMaybes $ map extractQuery comments
  where
    extractQuery (Comment content _) = stripPrefix "test: " content

-- | Parse and run a single test query against a program with timeout protection
runTestQuery :: Program -> String -> IO Bool
runTestQuery (Program decls _) queryStr = do
  putStr $ "Testing query: " ++ queryStr ++ " ... "
  case parseTerm queryStr of
    Left parseError -> do
      putStrLn $ "PARSE ERROR: " ++ show parseError
      return False
    Right query -> do
      -- First type check the program to get the subtyping graph
      case runChecker' (Program decls []) of
        Left typeError -> do
          putStrLn $ "TYPE ERROR: " ++ show typeError
          return False
        Right (checkingCtx, typedProgram) -> do
          let rewrites = [rewrite | Rewrite rewrite _ <- getDecls typedProgram]
          let rules = [rule | RulesDecl _ rules _ <- getDecls typedProgram, rule <- rules]
          let subtyping = _subtypingGraph checkingCtx
          let engineCtx = fromRules subtyping rules rewrites :: EngineCtx TypingPhase [] s
          let typedQuery = anyTyped query
          let solverComputation = ST.runST $ runSolver engineCtx (solve @TypingPhase typedQuery)
          
          -- Run with 5 second timeout to catch non-termination
          timeoutResult <- timeout 5000000 (return $! solverComputation) -- 5 seconds in microseconds
          case timeoutResult of
            Nothing -> do
              putStrLn "TIMEOUT (non-termination detected)"
              return False
            Just solutions -> do
              let hasSolution = not $ null solutions
              putStrLn $ if hasSolution then "PASS" else "FAIL"
              return hasSolution

-------------------------------------------------------------
-- Command implementations
-------------------------------------------------------------

-- | Execute the type checking command
runCheckCommand :: InputOptions -> Bool -> IO ()
runCheckCommand (InputOptions filename) verbose = do
  putStrLn $ "Checking " ++ filename
  loadResult <- loadAndParseFile filename
  case loadResult of
    Left (contents, err) -> printImportError contents err
    Right (contents, ast) -> do
      result <- traverse (\(ctx, ast') -> when verbose (pPrint ctx >> pPrint ast') >> return (ctx, ast')) $ runChecker' ast
      either (printError contents) (const printSuccess) result

-- | Execute the solver debugging command
runDebugCommand :: InputOptions -> IO ()
runDebugCommand (InputOptions filename) = SolverDebugger.debugSession filename

-- | Execute the code generation command
runCodegenCommand :: InputOptions -> CodeGenOptions -> IO ()
runCodegenCommand (InputOptions filename) (CodeGenOptions verbose enableDebug) = do
  loadResult <- loadAndParseFile filename
  case loadResult of
    Left (contents, err) -> printImportError contents err >> exitFailure
    Right (contents, ast) -> do
      r@(ctx, _) <- either (printError contents >=> const exitFailure) return $ runChecker' ast
      when verbose $ do
        pPrint ctx
      (uncurry (codegen enableDebug) >=> putStrLn) r

-- | Execute the code generation and run command
runRuncodegenCommand :: InputOptions -> IO ()
runRuncodegenCommand (InputOptions filename) = do
  loadResult <- loadAndParseFile filename
  case loadResult of
    Left (contents, err) -> printImportError contents err >> exitFailure
    Right (contents, ast) ->
      either (printError contents) runGenerated $ runChecker' ast
  where
    runGenerated (context, typedProgram) = do
      generatedCode <- codegen False context typedProgram
      let outName = replaceExtension filename "out.hs"
      writeFile outName generatedCode
      putStrLn $ "Generated code written to: " ++ outName
      -- TODO: ensure that this command can be executed without using cabal (dependending on the environment: prod/dev?).
      exitCode <- system $ "cabal exec -- runghc --ghc-arg=\"-package analysislang\" --ghc-arg=\"-package maf2-domains\" " ++ outName
      exitWith exitCode

-- | Execute the solver test command
runRunsolverCommand :: InputOptions -> IO ()
runRunsolverCommand (InputOptions filename) = do
  loadResult <- loadAndParseFile filename
  case loadResult of
    Left (contents, err) -> printImportError contents err >> exitFailure
    Right (_contents, ast@(Program _ comments)) -> do
      let queries = extractTestQueries comments
      if null queries
        then putStrLn "No test queries found in file (looking for %test: comments)"
        else do
          putStrLn $ "Running " ++ show (length queries) ++ " test queries..."
          results <- mapM (runTestQuery ast) queries
          let passed = length $ filter id results
              total = length results
              failed = total - passed

          putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"

          if failed == 0
            then do
              printGreen "All tests passed!"
              exitSuccess
            else do
              putStrLn $ show failed ++ " tests failed"
              exitWith (ExitFailure 1)

-------------------------------------------------------------
-- Main entry point
-------------------------------------------------------------

-- | Main entry point - parse command line arguments and execute the requested action
main :: IO ()
main = do
  GlobalOptions{..} <- execParser opts
  runAction


