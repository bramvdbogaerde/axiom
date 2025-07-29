{-# LANGUAGE LambdaCase #-}
module Language.Parser where

import Language.Lexer
import Language.AST

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Monad.Except
import Data.Maybe
import Data.Functor.Identity
import Text.Parsec.Error (ParseError)

type Parser a = ParsecT [TokenWithCtx] () Identity a

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

matchToken :: (Token -> Maybe a) -> Parser a
matchToken f = tokenPrim show (\pos _ _ -> pos) (f . tokenToken)

parens :: Parser a -> Parser a
parens = between lpar rpar

------------------------------------------------------------
--  Token matching parser
------------------------------------------------------------

-- | Matches with '='
equals :: Parser ()
equals = matchToken (\case Equal -> Just (); _ -> Nothing)

-- | Matches a semicolon
sem :: Parser ()
sem = matchToken (\case Sem -> Just (); _ -> Nothing)

-- | Matches "rules" tokens
rulesToken :: Parser ()
rulesToken = matchToken (\case Rules -> Just (); _ -> Nothing)

-- | Matches "syntax" tokens
syntaxToken :: Parser ()
syntaxToken = matchToken (\case Ident "syntax" -> Just (); _ -> Nothing)

-- | Matches a left parenthsis
lpar :: Parser ()
lpar = matchToken (\case Lpar -> Just (); _ -> Nothing)

-- | Matches a right parenthesis
rpar :: Parser ()
rpar = matchToken (\case Rpar -> Just (); _ -> Nothing)

-- | Matches a left curly brace
lcba :: Parser ()
lcba = matchToken (\case LCBa -> Just (); _ -> Nothing)

-- | Matches a right curly brace
rcba :: Parser ()
rcba = matchToken (\case RCBa -> Just (); _ -> Nothing)

-- | Matches a right comma
com :: Parser ()
com = matchToken (\case Comma -> Just (); _ -> Nothing)

-- | Matches with an identifier token
ident :: Parser String
ident = matchToken (\case Ident s -> Just s; _ -> Nothing)

-- | Matches with a defined as token
definedAs :: Parser ()
definedAs = matchToken (\case IsDefinedAs -> Just (); _ -> Nothing)

-- | Matches with a defined as token
mid :: Parser ()
mid = matchToken (\case Bar -> Just (); _ -> Nothing)

-- | Matches an "in" ident
inn :: Parser ()
inn = matchToken (\case Ident "in" -> Just () ; _ -> Nothing)


------------------------------------------------------------
-- Parsers
------------------------------------------------------------

-- | Parses a term or pattern
term :: Parser Production
term =  do
    typeOrFunctor <- ident
    parens (Functor typeOrFunctor <$> sepBy term com)  <|> return (Type typeOrFunctor)


-- | Parse a single syntax declaration
syntaxDecl :: Parser SyntaxDecl
syntaxDecl = do
      SyntaxDecl <$> sepBy ident com
                 <*> (inn >> ident)
                 <*> (fromMaybe [] <$> parseProductions)
  where parseProductions = optionMaybe (definedAs >> sepBy parseProduction mid)
        parseProduction :: Parser Production
        parseProduction  = term
-- | Parse a syntax block
syntax :: Parser Decl
syntax =  Syntax <$>
       (   syntaxToken
        >> lcba
        >> endBy syntaxDecl sem
        <* rcba )

-- | Parse a rewrite rule
rewriteRule :: Parser Decl
rewriteRule =  Rewrite 
           <$> ( RewriteDecl
              <$> ident
              <*> parens (sepBy term com)
              <*> (equals >> term) )
    where termParser = undefined

-- | Parse a rules section
rules :: Parser Decl
rules = RulesDecl <$> undefined

parser :: Parser Program
parser = Program <$> endBy programElement sem
  where programElement =  syntax
                       -- <|> rules
                       <|> rewriteRule


runParser :: [TokenWithCtx] -> Either ParseError Program
runParser =  parse parser "<test>"

