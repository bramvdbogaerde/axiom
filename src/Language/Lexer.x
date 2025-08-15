{
{-# LANGUAGE RecordWildCards #-}

module Language.Lexer (
  lex,
  Token(..),
  TokenWithRange(..)
) where

import Prelude hiding (lex)
import Language.Lexer.Token
import Language.Range

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-

  $white+                       ;
  "%".*                         { mkTokenWith (\s -> Comment (drop 1 s)) } -- comments
  "::="                         { mkToken IsDefinedAs }
  "~>"                          { mkToken LeadsTo }
  "=>"                          { mkToken Implies }
  "/="                          { mkToken NotEqual }
  "="                           { mkToken Equal }
  "rules"                       { mkToken Rules }
  "rule"                        { mkToken Rule }
  "("                           { mkToken Lpar }
  ")"                           { mkToken Rpar }
  "{"                           { mkToken LCBa }
  "}"                           { mkToken RCBa }
  "["                           { mkToken LBra }
  "]"                           { mkToken RBra }
  ","                           { mkToken Comma }
  "."                           { mkToken Dot }
  ";"                           { mkToken Sem }
  "|"                           { mkToken Bar }
  $alpha $alphanum*             { mkTokenWith Ident }
  \"[^\"]*\"                    { mkTokenWith (\s -> Quo (init (tail s))) }

{

-- | Helper function to create tokens without needing string content
mkToken :: Token -> AlexPosn -> String -> TokenWithRange
mkToken token pos str = TokenWithRange token (alexPosToRange pos str)

-- | Helper function to create tokens that need the string content
mkTokenWith :: (String -> Token) -> AlexPosn -> String -> TokenWithRange
mkTokenWith tokenFunc pos str = TokenWithRange (tokenFunc str) (alexPosToRange pos str)

-- | Convert Alex position to Range
alexPosToRange :: AlexPosn -> String -> Range
alexPosToRange (AlexPn _ line col) str = 
  Range (Position line col Nothing) (Position line (col + length str - 1) Nothing)

-- | Main lexing function
lex :: String -> [TokenWithRange]
lex input = alexScanTokens input

}
