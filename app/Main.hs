{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (lex)
import Language.Parser
import Language.TypeCheck
import Text.Pretty.Simple hiding (Vivid, Green)
import Data.Either
import Data.Bifunctor
import System.Console.ANSI

printGreen :: String -> IO ()
printGreen txt = setSGR [SetColor Foreground Vivid Green]
              >> putStrLn txt
              >> setSGR [Reset]

printError :: Error -> IO ()
printError = undefined

printSuccess :: IO ()
printSuccess = printGreen "Program typechecked successfully"

main :: IO ()
main = do
  contents <- readFile "example.sem"
  let ast = either (error . ("could not parse program" ++) . show) id $  parseProgram contents
  either printError (const printSuccess) $ runChecker ast
  

