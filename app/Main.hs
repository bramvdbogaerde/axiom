{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Main where

import Prelude hiding (lex)
import Language.AST
import Language.Parser
import Language.TypeCheck
import Text.Pretty.Simple hiding (Vivid, Green)
import Data.Either
import Data.Bifunctor
import System.Console.ANSI
import Reporting
import Options.Applicative

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

-- | Parser for all available subcommands
commandParser :: Parser (IO ())
commandParser = subparser
  ( command "check"
    (info checkCommand (progDesc "Type check a program"))
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

-------------------------------------------------------------
-- Main entry point
-------------------------------------------------------------

-- | Main entry point - parse command line arguments and execute the requested action
main :: IO ()
main = do
  GlobalOptions{..} <- execParser opts
  runAction
  

