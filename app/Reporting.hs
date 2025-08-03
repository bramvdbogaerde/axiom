module Reporting where

import Language.TypeCheck (ModelError(..), Error(..), Context(..), SortName(..))
import Language.AST (Range(..), Position(..))
import System.Console.ANSI
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Monad (when)

-------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------

printError :: String -> Error -> IO ()
printError sourceCode (Error modelErr maybeRange ctx) = do
  printColoredLn [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "Error:"
  putStrLn $ "  " ++ formatModelError modelErr
  maybe (return ()) ((putStrLn "" >>) . printLocationInfo sourceCode) maybeRange
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
  "Variable '" ++ var ++ "' is already defined for sort '" ++ getSortName sortName ++ "'"
formatModelError (DuplicateSort sortName) =
  "Sort '" ++ getSortName sortName ++ "' is defined multiple times"
formatModelError (NoNestingAt sortName) =
  "Nested terms are not allowed in sort '" ++ getSortName sortName ++ "' - only atoms are permitted"
formatModelError (NoSuchSort var) =
  "Variable '" ++ var ++ "' is used but not defined in any sort"
formatModelError (IncompatibleTypes expected actual) =
  "Type mismatch: " ++ formatTypeDifference expected actual
formatModelError (SortNotDefined sortName) =
  "Sort " ++ sortName ++ " is not defined"

formatTypeDifference :: [SortName] -> [SortName] -> String
formatTypeDifference expected actual =
    case differences of
         [] -> "all types match (this shouldn't happen)"
         [(e, a)] -> "expected '" ++ getSortName e ++ "' but got '" ++ getSortName a ++ "'"
         diffs -> "differences at " ++ intercalate ", "
                    (map (\(e, a) -> "'" ++ getSortName e ++ "' vs '" ++ getSortName a ++ "'") diffs)
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
