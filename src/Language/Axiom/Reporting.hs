module Language.Axiom.Reporting where

import Language.TypeCheck (ModelError(..))
import qualified Language.TypeCheck as TypeCheck
import qualified Language.Parser as Parser
import Language.Types (Typ(..), toSortName)
import Language.AST (Range(..), Position(..), filenameStart)
import Language.ImportResolver (ImportError(..), ModuleMap)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Console.ANSI
import Data.List (intercalate)
import Control.Monad

-------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------

printError :: ModuleMap -> TypeCheck.Error -> IO ()
printError modules (TypeCheck.Error modelErr maybeRange _ctx) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Error:"
  putStrLn $ "  " ++ formatModelError modelErr
  maybe (return ()) ((putStrLn "" >>) . printLocationInfo (getSourceFromRange modules maybeRange)) maybeRange
  setSGR [Reset]

printTypeError :: String -> TypeCheck.Error -> IO ()
printTypeError body (TypeCheck.Error modelErr maybeRange _ctx) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Error:"
  putStrLn $ "  " ++ formatModelError modelErr
  maybe (return ()) ((putStrLn "" >>) . printLocationInfo body) maybeRange
  setSGR [Reset]


getSourceFromRange :: ModuleMap -> Maybe Range -> String
getSourceFromRange modules r = do
  fromMaybe "no source information"
            (foldMap (flip Map.lookup modules <=< filenameStart) r)

getSource :: ModuleMap -> FilePath -> String
getSource modules name =
  fromMaybe "no source information" $ Map.lookup name modules

printParseError :: String -> Parser.Error -> IO ()
printParseError sourceCode (Parser.ParsingError pos msg) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Parse Error:"
  putStrLn $ "  " ++ msg
  putStrLn ""
  let range = Range pos pos
  printLocationInfo sourceCode range
  setSGR [Reset]

printImportError :: ModuleMap -> ImportError -> IO ()
printImportError modules (ParseError filepath err) =
  printParseError (getSource modules filepath) err
printImportError _contents (FileNotFound filepath) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "File Not Found:"
  putStrLn $ "  " ++ filepath
  setSGR [Reset]
printImportError _contents (CyclicImport files) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Cyclic Import:"
  putStrLn $ "  " ++ show files
  setSGR [Reset]
printImportError _contents (InvalidImportPath path) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Invalid Import Path:"
  putStrLn $ "  " ++ path
  setSGR [Reset]

printColoredLn :: [SGR] -> String -> IO ()
printColoredLn sgr text =
  setSGR sgr >> putStrLn text >> setSGR [Reset]

printColored :: [SGR] -> String -> IO ()
printColored sgr text =
  setSGR sgr >> putStr text >> setSGR [Reset]

-------------------------------------------------------------
-- Error formatting
-------------------------------------------------------------

formatModelError :: ModelError -> String
formatModelError (DuplicateVariable var sortName) =
  "Variable '" ++ var ++ "' is already defined for sort '" ++ toSortName sortName ++ "'"
formatModelError (DuplicateSort sortName) =
  "Sort '" ++ toSortName sortName ++ "' is defined multiple times"
formatModelError (NoNestingAt sortName) =
  "Nested terms are not allowed in sort '" ++ toSortName sortName ++ "' - only atoms are permitted"
formatModelError (NameNotDefined var) =
  "Name '" ++ var ++ "' is used as a functor but not defined."
formatModelError (IncompatibleTypes expected actual) =
  "Type mismatch: " ++ formatTypeDifference expected actual
formatModelError (SortNotDefined sortName) =
  "Sort " ++ sortName ++ " is not defined"
formatModelError (ArityMismatch functorName expected actual) =
  "Arity mismatch for '" ++ functorName ++ "': expected " ++ show expected ++ " arguments but got " ++ show actual
formatModelError HaskellExprTypeInferenceError =
  "Cannot infer type for Haskell expression - no type context available"
formatModelError (InvalidConstructor msg) =
  "Invalid constructor: " ++ msg

formatTypeDifference :: [Typ] -> [Typ] -> String
formatTypeDifference expected actual
  | length expected /= length actual = 
      "arity mismatch: expected " ++ show (length expected) ++ " arguments but got " ++ show (length actual)
  | otherwise = 
      case differences of
           [] -> "all types match (this shouldn't happen)"
           [(e, a)] -> "expected '" ++ toSortName e ++ "' but got '" ++ toSortName a ++ "'"
           diffs -> "differences at " ++ intercalate ", "
                      (map (\(e, a) -> "'" ++ toSortName e ++ "' vs '" ++ toSortName a ++ "'") diffs)
  where differences = filter (uncurry (/=)) (zip expected actual)

-------------------------------------------------------------
-- Source context printing
-------------------------------------------------------------

-- | Print line numbers next to the relevant portions of the source file
printLocationInfo :: String -> Range -> IO ()
printLocationInfo sourceCode range = do
  let startPos = rangeStart range

  printColored [SetColor Foreground Vivid Blue] "  --> "
  case filename startPos of
    Just fname -> putStrLn $ fname ++ ":" ++ show (positionLine startPos) ++ ":" ++ show (positionColumn startPos)
    Nothing -> putStrLn $ "line " ++ show (positionLine startPos) ++ ", column " ++ show (positionColumn startPos)

  printSourceContext sourceCode range

printSourceContext :: String -> Range -> IO ()
printSourceContext sourceCode range = do
  let sourceLines = lines sourceCode
  let startLine = positionLine (rangeStart range)
  let endLine = positionLine (rangeEnd range)
  let contextStart = max 1 (startLine - 1)
  let contextEnd = min (length sourceLines) (endLine + 1)

  let relevantLines = take (contextEnd - contextStart + 1) $ drop (contextStart - 1) sourceLines
  let lineNumbers = [contextStart..contextEnd]

  mapM_ (printSourceLine range startLine endLine) (zip lineNumbers relevantLines)

printSourceLine :: Range -> Int -> Int -> (Int, String) -> IO ()
printSourceLine range startLine endLine (lineNum, content) = do
  let isErrorLine = lineNum >= startLine && lineNum <= endLine
  let lineNumStr = show lineNum
  let padding = replicate (4 - length lineNumStr) ' '

  printColored [SetColor Foreground Vivid Blue] (padding ++ lineNumStr ++ " | ")

  if isErrorLine
    then printHighlightedLine range lineNum content
    else putStrLn content

printHighlightedLine :: Range -> Int -> String -> IO ()
printHighlightedLine range lineNum content = do
  let startPos = rangeStart range
  let endPos = rangeEnd range
  let startCol = if positionLine startPos == lineNum then positionColumn startPos else 1
  let endCol = if positionLine endPos == lineNum then positionColumn endPos else length content

  let (before, rest) = splitAt (startCol - 1) content
  let (highlighted, after) = splitAt (endCol - startCol + 1) rest

  putStr before
  printColored [SetColor Foreground Vivid Red] highlighted
  putStrLn after
