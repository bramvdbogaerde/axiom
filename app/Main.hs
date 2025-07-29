module Main where

import Prelude hiding (lex)
import Language.Lexer
import Language.Parser
import Text.Pretty.Simple

main :: IO ()
main = do
  contents <- readFile "example.sem"
  let tokens = lex emptyCtx contents
  pPrint tokens
  pPrint $ runParser tokens

