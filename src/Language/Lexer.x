{
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-x-partial #-}


module Language.Lexer (
  lex,
  lexWithFilename,
  Token(..),
  TokenWithRange(..)
) where

import Prelude hiding (lex)
import Language.Lexer.Token
import Language.Range
import Data.Either
import qualified Debug.Trace as Debug
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Monad

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z\x80-\x10ffff] -- ASCII letters + Unicode symbols
$alphanum = [a-zA-Z0-9\x80-\x10ffff] -- ASCII letters/digits + Unicode symbols

tokens :-

  <0> $white+                       ;
  <0> "%".*                         { mkTokenWith (\s -> Comment (drop 1 s)) } -- comments
  <0> \$ \{ [^\}]* \}               { mkTokenWith (\s -> HaskellExpr (init (tail (tail s)))) } -- Haskell expressions
  <0> "::="                         { mkToken IsDefinedAs }
  <0> "~>"                          { mkToken LeadsTo }
  <0> "=>"                          { mkToken Implies }
  <0> "/="                          { mkToken NotEqual }
  <0> "="                           { mkToken Equal }
  <0> "⇓"                           { mkToken BigStep }
  <0> "rules"                       { mkToken Rules }
  <0> "rule"                        { mkToken Rule }
  <0> "true"                        { mkToken (Boo True)  }
  <0> "false"                       { mkToken (Boo False) }
  <0> "|->"                         { mkToken MapsTo }
  <0> "("                           { mkToken Lpar }
  <0> ")"                           { mkToken Rpar }
  <0> "{{{"                         { begin h }
  <h> "}}}"                         { collectInto (\s r -> TokenWithRange (Hask s) r) `andBegin` 0 }
  <h> [$white $printable]           { save }

  <0> "[[["                         { begin p }
  <p> "]]]"                         { collectInto (\s r -> TokenWithRange (HaskPost s) r) `andBegin` 0 }
  <p> [$white $printable]           { save }

  <0> "{"                           { mkToken LCBa }
  <0> "}"                           { mkToken RCBa }
  <0> "["                           { mkToken LBra }
  <0> "]"                           { mkToken RBra }
  <0> ","                           { mkToken Comma }
  <0> "."                           { mkToken Dot }
  <0> ";"                           { mkToken Sem }
  <0> "|"                           { mkToken Bar }
  <0> "_"                           { mkToken Wildcard }
  <0> $alpha $alphanum*             { mkTokenWith Ident }
  <0> \"[^\"]*\"                    { mkTokenWith (\s -> Quo (init (tail s))) }
  <0> $digit+                       { mkTokenWith (\s -> IntLit (read s)) }

{

data AlexUserState = AlexUserState {
  accumulated :: String,
  currentFilename :: String
}

alexInitUserState = AlexUserState "" "<unknown>"

-- | Appends the collected input into the current user state, skips to the next token
save :: AlexInput -> Int -> Alex TokenWithRange
save (_, _, _, str) len = do
  st <- alexGetUserState
  alexSetUserState $ st { accumulated = accumulated st ++ take len str }
  alexMonadScan

-- | Collects the collected input into the given token
collectInto :: (String -> Range -> a) -> AlexInput -> Int -> Alex a
collectInto f (pos, _, _, str) len = do
  st <- alexGetUserState
  let tok = f (accumulated st) (alexPosToRange (currentFilename st) pos (take len str))
  alexSetUserState $ st { accumulated = "" }
  return tok

-- | Designates a special token for the end of the input
alexEOF :: Alex TokenWithRange
alexEOF = return $ TokenWithRange EOF dummyRange 

byteToString :: Int -> [Word8] -> String
byteToString len = U.toString . B.pack . take len

-- | Helper function to create tokens without needing string content
mkToken :: Token -> AlexInput -> Int -> Alex TokenWithRange
mkToken token (pos, _, _, str) len = do
  st <- alexGetUserState
  return $ TokenWithRange token (alexPosToRange (currentFilename st) pos (take len str))

-- | Helper function to create tokens that need the string content
mkTokenWith :: (String -> Token) -> AlexInput -> Int -> Alex TokenWithRange
mkTokenWith tokenFunc (pos, _, _, str) len = do
  st <- alexGetUserState
  return $ TokenWithRange (tokenFunc $ (take len str)) (alexPosToRange (currentFilename st) pos (take len str))

-- | Convert Alex position to Range
alexPosToRange :: String -> AlexPosn -> String -> Range
alexPosToRange fname (AlexPn _ line col) str =
  Range (Position line col (Just fname)) (Position line (col + length str - 1) (Just fname))


-- | Main lexing function with filename
lexWithFilename :: String -> String -> [TokenWithRange]
lexWithFilename fname input = either (error . ("lexical error" ++)  . show) id $ runAlex input go
  where go = do
          alexSetUserState $ AlexUserState "" fname
          collectTokens
        collectTokens = do
          tok <- alexMonadScan
          case tok of
            TokenWithRange EOF _ -> return []
            _ ->  (tok:) <$> collectTokens

-- | Main lexing function (uses "<unknown>" as filename)
lex :: String -> [TokenWithRange]
lex = lexWithFilename "<unknown>"
}
