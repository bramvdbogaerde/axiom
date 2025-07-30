module Main where

import Prelude hiding (lex)
import Language.Lexer
import Language.Parser
import Language.Model
import Text.Pretty.Simple
import Data.Either

main :: IO ()
main = do
  contents <- readFile "example.sem"
  let tokens = lex emptyCtx contents
  -- pPrint tokens
  let ast = fromRight (error "could not parse program") $  runParser tokens
  pPrint $ testContext ast

