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

brackets :: Parser a -> Parser a
brackets = between lbra rbra


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

-- | Matches with a left bracket
lbra :: Parser ()
lbra = matchToken (\case LBra -> Just () ; _ -> Nothing)

-- | Matches with a left bracket
rbra :: Parser ()
rbra = matchToken (\case RBra -> Just () ; _ -> Nothing)

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

-- | Match a "rule" token
ruleToken :: Parser ()
ruleToken = matchToken (\case Rule -> Just () ; _ -> Nothing)

-- | Match a string token and returns its value
str :: Parser String
str = matchToken (\case Quo s -> Just s; _ -> Nothing)

-- | Matches with a "~>" token
leadsto :: Parser ()
leadsto = matchToken (\case LeadsTo -> Just () ; _ -> Nothing)

-- | Matches with a "transition" token
transitionToken :: Parser ()
transitionToken = matchToken (\case Ident "transition" -> Just (); _ -> Nothing)

-- | Matches with a "=>" token
implies :: Parser ()
implies = matchToken (\case Implies -> Just (); _ -> Nothing)

------------------------------------------------------------
-- Parsers
------------------------------------------------------------

-- | Parses a term or pattern
term :: Parser Term
term =  do
    typeOrFunctor <- ident
    term0 <- parens (Functor typeOrFunctor <$> sepBy term com)  <|> return (Atom typeOrFunctor)
    -- possibly still a transition rule or "equals" rule
    (equals >> Eqq term0 <$> term) <|> (leadsto >> Transition "~>" term0 <$> term) <|> return term0

-- | Parses a sequence of terms seperated (and ended) by the given parser
terms :: Parser a -> Parser [Term]
terms = sepBy term

-- | Parse a single syntax declaration
syntaxDecl :: Parser SyntaxDecl
syntaxDecl = do
      SyntaxDecl <$> sepBy ident com
                 <*> (inn >> ident)
                 <*> (fromMaybe [] <$> parseProductions)
  where parseProductions = optionMaybe (definedAs >> sepBy parseProduction mid)
        parseProduction :: Parser Term
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

-- | Parse a single rule declaration
rule :: Parser RuleDecl
rule = ruleToken >> (RuleDecl <$> str <*> brackets (terms sem) <*> (implies >> brackets (terms sem)))

-- | Parse a rules section
rules :: Parser Decl
rules = rulesToken >> RulesDecl <$> between lcba rcba (endBy rule sem)

-- | Parses a declarartion for a transition
transition :: Parser Decl
transition = transitionToken >> flip TransitionDecl <$> ident <*> (leadsto >> return "~>") <*> ident

parser :: Parser Program
parser = Program <$> endBy programElement sem
  where programElement =  syntax
                       <|> rules
                       <|> transition
                       <|> rewriteRule


runParser :: [TokenWithCtx] -> Either ParseError Program
runParser =  parse parser "<test>"

