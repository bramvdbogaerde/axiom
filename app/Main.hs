{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (lex)
import Language.Lexer
import Language.Parser
import Language.TypeCheck
import Text.Pretty.Simple
import Data.Either
import Data.Bifunctor

main :: IO ()
main = do
  contents <- readFile "example.sem"
  let tokens = lex emptyCtx contents
  -- pPrint tokens
  let ast = fromRight (error "could not parse program") $  runParser tokens
  pPrint $ first (\case Error e _ -> e) $ runChecker ast

