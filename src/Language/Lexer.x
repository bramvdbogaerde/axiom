{
{-# LANGUAGE RecordWildCards #-}

module Language.Lexer (
  lex,
  Token(..),
  TokenWithRange(..)
) where

import Prelude hiding (lex)
import Language.Lexer.Token
import qualified Debug.Trace as Debug
import Language.Range
import Data.Either
import qualified Debug.Trace as Debug
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Control.Monad

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [a-zA-Z0-9]

tokens :-

  <0> $white+                       ;
  <0> "%".*                         { mkTokenWith (\s -> Comment (drop 1 s)) } -- comments
  <0> \$ \{ [^\}]* \}               { mkTokenWith (\s -> HaskellExpr (init (tail (tail s)))) } -- Haskell expressions
  <0> "::="                         { mkToken IsDefinedAs }
  <0> "~>"                          { mkToken LeadsTo }
  <0> "=>"                          { mkToken Implies }
  <0> "/="                          { mkToken NotEqual }
  <0> "="                           { mkToken Equal }
  <0> "rules"                       { mkToken Rules }
  <0> "rule"                        { mkToken Rule }
  <0> "("                           { mkToken Lpar }
  <0> ")"                           { mkToken Rpar }
  <0> "{{{"                         { begin h }
  <h> "}}}"                         { collectInto (\s r -> TokenWithRange (Hask s) r) `andBegin` 0 }
  <h> [$white $printable]           { save }
  <0> "{"                           { mkToken LCBa }
  <0> "}"                           { mkToken RCBa }
  <0> "["                           { mkToken LBra }
  <0> "]"                           { mkToken RBra }
  <0> ","                           { mkToken Comma }
  <0> "."                           { mkToken Dot }
  <0> ";"                           { mkToken Sem }
  <0> "|"                           { mkToken Bar }
  <0> $alpha $alphanum*             { mkTokenWith Ident }
  <0> \"[^\"]*\"                    { mkTokenWith (\s -> Quo (init (tail s))) }
  <0> $digit+                       { mkTokenWith (\s -> IntLit (read s)) }

{

type AlexUserState = String
alexInitUserState = ""

-- | Appends the collected input into the current user state, skips to the next token
save :: AlexInput -> Int -> Alex TokenWithRange
save (_, _, _, str) len = (++ take len str) <$> alexGetUserState >>= alexSetUserState >> alexMonadScan

-- | Collects the collected input into the given token
collectInto :: (String -> Range -> a) -> AlexInput -> Int -> Alex a
collectInto f (pos, _, _, str) len = do
  s <- alexGetUserState
  let token = f s (alexPosToRange pos (take len str))
  alexSetUserState ""
  return token

-- | Designates a special token for the end of the input
alexEOF :: Alex TokenWithRange
alexEOF = return $ TokenWithRange EOF dummyRange 

byteToString :: Int -> [Word8] -> String
byteToString len = U.toString . B.pack . take len

-- | Helper function to create tokens without needing string content
mkToken :: Token -> AlexInput -> Int -> Alex TokenWithRange
mkToken token (pos, _, _, str) len = return $ TokenWithRange token (alexPosToRange pos (take len str))

-- | Helper function to create tokens that need the string content
mkTokenWith :: (String -> Token) -> AlexInput -> Int -> Alex TokenWithRange
mkTokenWith tokenFunc (pos, _, _, str) len = return $ TokenWithRange (tokenFunc $ (take len str)) (alexPosToRange pos (take len str))

-- | Convert Alex position to Range
alexPosToRange :: AlexPosn -> String -> Range
alexPosToRange (AlexPn _ line col) str = 
  Range (Position line col Nothing) (Position line (col + length str - 1) Nothing)

 
-- | Main lexing function
lex :: String -> [TokenWithRange]
lex = either (error . ("lexical error" ++)  . show) id . flip runAlex go
  where go = do
          tok <- alexMonadScan
          case (Debug.traceShowId tok) of
            TokenWithRange EOF _ -> return []
            _ ->  (tok:) <$> go
}
