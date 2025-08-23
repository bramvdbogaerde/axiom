{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Prelude hiding (lex)
import Language.AST
import Language.Parser
import Language.TypeCheck
import Language.CodeGen
import Text.Pretty.Simple hiding (Vivid, Green)
import Data.Either
import Data.Bifunctor
import Control.Monad
import System.Console.ANSI
import System.Exit
import System.Process
import System.FilePath
import Reporting
import Options.Applicative
import qualified LanguageServer
import qualified SolverDebugger

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

-------------------------------------------------------------
-- Command line parsers
-------------------------------------------------------------

-- | Parser for input file options (reusable across commands)
inputOptionsParser :: Parser InputOptions
inputOptionsParser = InputOptions
  <$> strArgument
      ( metavar "FILE"
     <> help "Input file to process" )

-- | Parser for the 'check' subcommand
checkCommand :: Parser (IO ())
checkCommand = runCheckCommand <$> inputOptionsParser

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
codegenCommand = runCodegenCommand <$> inputOptionsParser

-- | Parser for the 'runcodegen' subcommand
runcodegenCommand :: Parser (IO ())
runcodegenCommand = runRuncodegenCommand <$> inputOptionsParser

-- | Parser for all available subcommands
commandParser :: Parser (IO ())
commandParser = subparser
  ( command "check"
    (info checkCommand (progDesc "Type check a program"))
 <> command "debug"
    (info debugCommand (progDesc "Start interactive solver debugger"))
 <> command "lsp"
    (info lspCommand (progDesc "Start Language Server Protocol server"))
 <> command "codegen"
    (info codegenCommand (progDesc "Generate code from a typechecked program"))
 <> command "runcodegen"
    (info runcodegenCommand (progDesc "Generate and execute code from a typechecked program"))
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
loadAndParseFile :: String -> IO (String, Program)
loadAndParseFile filename = do
  contents <- readFile filename
  let ast = either (error . ("could not parse program" ++) . show) id $ parseProgram contents
  return (contents, ast)

-- | Print text in green color for success messages
printGreen :: String -> IO ()
printGreen txt = setSGR [SetColor Foreground Vivid Green]
              >> putStrLn txt
              >> setSGR [Reset]

-- | Print a success message when type checking completes
printSuccess :: IO ()
printSuccess = printGreen "Program typechecked successfully"

-------------------------------------------------------------
-- Command implementations
-------------------------------------------------------------

-- | Execute the type checking command
runCheckCommand :: InputOptions -> IO ()
runCheckCommand (InputOptions filename) = do
  (contents, ast) <- loadAndParseFile filename
  either (printError contents) (const printSuccess) $ runChecker ast

-- | Execute the solver debugging command
runDebugCommand :: InputOptions -> IO ()
runDebugCommand (InputOptions filename) = SolverDebugger.debugSession filename

-- | Execute the code generation command
runCodegenCommand :: InputOptions -> IO ()
runCodegenCommand (InputOptions filename) = do
  (contents, ast) <- loadAndParseFile filename
  either (printError contents) (uncurry codegen >=> putStrLn) $ runChecker' ast

-- | Execute the code generation and run command
runRuncodegenCommand :: InputOptions -> IO ()
runRuncodegenCommand (InputOptions filename) = do
  (contents, ast) <- loadAndParseFile filename
  either (printError contents) runGenerated $ runChecker' ast
  where
    runGenerated (context, typedProgram) = do
      generatedCode <- codegen context typedProgram
      let outName = replaceExtension filename "out.hs"
      writeFile outName generatedCode
      putStrLn $ "Generated code written to: " ++ outName
      exitCode <- system $ "cabal exec -- runghc --ghc-arg=\"-package analysislang\" " ++ outName
      exitWith exitCode

-------------------------------------------------------------
-- Main entry point
-------------------------------------------------------------

-- | Main entry point - parse command line arguments and execute the requested action
main :: IO ()
main = do
  GlobalOptions{..} <- execParser opts
  runAction
  

