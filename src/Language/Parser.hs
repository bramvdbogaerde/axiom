{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Parser(parseProgram, Error(..)) where

import Prelude hiding (lex)

import Language.Lexer
import Language.AST

import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Combinator
import Control.Monad.Except
import Data.Maybe
import Data.Functor.Identity
import Text.Parsec.Error (ParseError)
import Text.Parsec (SourcePos(..), sourceLine, sourceColumn, sourceName, errorPos)
import Data.Bifunctor

type Parser a = ParsecT [TokenWithCtx] () Identity a

data Error = ParsingError { parseErrorPosition :: Position, parseErrorMessage :: String }
          deriving (Ord, Show, Eq)

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition pos = Position (sourceLine pos) (sourceColumn pos) (Just $ sourceName pos)

parseErrorToError :: ParseError -> Error
parseErrorToError err = ParsingError (sourcePosToPosition $ errorPos err) (show err)

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

matchToken :: (Token -> Maybe a) -> Parser a
matchToken f = tokenPrim show (\pos _ _ -> pos) (f . tokenToken)

parens :: Parser a -> Parser a
parens = between lpar rpar

brackets :: Parser a -> Parser a
brackets = between lbra rbra

-- | Returns the current token with its context without consuming any input,
-- never fails.
currentToken :: Parser TokenWithCtx
currentToken = lookAhead $ tokenPrim show (\pos _ _ -> pos) Just

-- | Turns a 'Ctx' into a valid position
ctxToPosition :: Ctx -> Position
ctxToPosition Ctx { .. } =
  -- TODO: add filename information to the lexer so that it can
  -- be used here.
  Position line col Nothing

-- | Returns the position (according to the lexer) of the current token
position :: Parser Position
position = ctxToPosition . tokenCtx <$> currentToken

-- | End the range at the current position
endRange :: Position -- ^ starting position
        -> Parser Range
endRange = (fmap (flip Range) position <*>) . pure

-- TODO: we should actually do the range construction in the lexer as it can more
-- reliably deduce the range of its tokens
-- | Attaches a range object to the output of the given applicative functor
withRange :: Parser (Range -> a) -> Parser a
withRange f = do
  pos0 <- position
  f <*> endRange pos0

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

identWithRange :: Parser (String, Range)
identWithRange = do
  start <- position
  x <- ident
  end <- position
  return (x, Range start end) 

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
    pos0 <- position
    typeOrFunctor <- ident
    -- Terms are functors or atoms
    term0 <- (parens (Functor typeOrFunctor <$> sepBy term com)
          <|> return (Atom typeOrFunctor)) <*> endRange pos0

    -- Terms can still be infix operators (which are predefined)
    (equals >> Eqq term0 <$> term <*> endRange pos0)
      <|> (leadsto >> Transition "~>" term0 <$> term <*> endRange pos0)
      <|> return term0

-- | Parses a sequence of terms seperated (and ended) by the given parser
terms :: Parser a -> Parser [Term]
terms = sepBy term

-- | Parse a single syntax declaration
syntaxDecl :: Parser SyntaxDecl
syntaxDecl = withRange $ do
      SyntaxDecl <$> sepBy ident com
                 <*> (inn >> ident)
                 <*> (fromMaybe [] <$> parseProductions)
  where parseProductions = optionMaybe (definedAs >> sepBy parseProduction mid)
        parseProduction :: Parser Term
        parseProduction  = term
-- | Parse a syntax block
syntax :: Parser Decl
syntax =  withRange $ Syntax <$>
       (   syntaxToken
        >> lcba
        >> endBy syntaxDecl sem
        <* rcba )

-- | Parse a rewrite rule
rewriteRule :: Parser Decl
rewriteRule =  withRange $ Rewrite
           <$> withRange (RewriteDecl
              <$> ident
              <*> parens (sepBy term com)
              <*> (equals >> term))

-- | Parse a single rule declaration
rule :: Parser RuleDecl
rule = ruleToken >> withRange (RuleDecl <$> str <*> brackets (terms sem) <*> (implies >> brackets (terms sem)))

-- | Parse a rules section
rules :: Parser Decl
rules = rulesToken >> withRange (RulesDecl <$> between lcba rcba (endBy rule sem))

-- | Parses a declarartion for a transition
transition :: Parser Decl
transition = transitionToken >> withRange (flip TransitionDecl <$> identWithRange <*> (leadsto >> return "~>") <*> identWithRange)

parser :: Parser Program
parser = Program <$> endBy programElement sem
  where programElement =  syntax
                       <|> rules
                       <|> transition
                       <|> rewriteRule


runParser :: [TokenWithCtx] -> Either ParseError Program
runParser =  parse parser "<test>"

-- | Parse a program from string
parseProgram :: String -> Either Error Program
parseProgram = first parseErrorToError . runParser . lex emptyCtx
