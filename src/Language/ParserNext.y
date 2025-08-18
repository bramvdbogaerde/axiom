{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.ParserNext(parseProgram, parseTerm, parseRule, Error(..)) where

import Prelude hiding (lex)

import Language.Lexer
import Language.Lexer.Token
import qualified Language.Lexer.Token as Token
import Language.AST
import Language.Range
import Data.Functor.Identity
import Data.Maybe
import Data.Bifunctor
import Data.Either (partitionEithers)

}

%name parseProgram Program
%name parseTerm Term  
%name parseRule Rule
%tokentype { TokenWithRange }
%error { parseError }

%token
  '::='     { TokenWithRange IsDefinedAs _ }
  '~>'      { TokenWithRange LeadsTo _ }
  '=>'      { TokenWithRange Implies _ }
  '='       { TokenWithRange Equal _ }
  '/='      { TokenWithRange NotEqual _ }
  'rules'   { TokenWithRange Rules _ }
  'rule'    { TokenWithRange Rule _ }
  '('       { TokenWithRange Lpar _ }
  ')'       { TokenWithRange Rpar _ }
  '{'       { TokenWithRange LCBa _ }
  '}'       { TokenWithRange RCBa _ }
  '['       { TokenWithRange LBra _ }
  ']'       { TokenWithRange RBra _ }
  ','       { TokenWithRange Comma _ }
  '.'       { TokenWithRange Dot _ }
  ';'       { TokenWithRange Sem _ }
  '|'       { TokenWithRange Bar _ }
  IDENT     { TokenWithRange (Ident _) _ }
  STRING    { TokenWithRange (Quo _) _ }
  COMMENT   { TokenWithRange (Token.Comment _) _ }

%right '~>'
%left '=' '/='

%%

Program :: { Program }
Program : { undefined }

Term :: { PureTerm }
Term : { undefined }

Rule :: { RuleDecl }
Rule : { undefined }


{

-------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------

mkRange :: (RangeOf a, RangeOf b) => a -> b -> Range
mkRange start end  = Range (rangeStart (rangeOf start)) (rangeEnd (rangeOf end))

-------------------------------------------------------------
-- Error handling
-------------------------------------------------------------

data Error = ParsingError { parseErrorPosition :: Position, parseErrorMessage :: String }
          deriving (Ord, Show, Eq)

parseError :: [TokenWithRange] -> a
parseError [] = error "Parse error at end of input"
parseError (t:_) = error $ "Parse error at " ++ show (tokenRange t) ++ ": unexpected " ++ show (tokenToken t)

-------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------


-- parseProgram :: String -> Either Error Program
-- parseProgram = undefined
-- 
-- parseTerm :: String -> Either Error PureTerm
-- parseTerm = undefined
-- 
-- parseRule :: String -> Either Error RuleDecl  
-- parseRule = undefined

}
