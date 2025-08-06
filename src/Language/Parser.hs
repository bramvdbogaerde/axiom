{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Parser(parseProgram, parseTerm, parseRule, Error(..)) where

import Prelude hiding (lex)

import Language.Lexer
import Language.Lexer.Token
import Language.AST
import Language.Range
import Data.Functor.Identity

import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Combinator
import Text.Parsec
    ( sepEndBy,
      skipMany,
      eof,
      (<?>),
      SourcePos(..),
      sourceLine,
      sourceColumn,
      sourceName,
      errorPos )
import Control.Monad.Except
import Data.Maybe
import Data.Functor.Identity
import Text.Parsec.Error (ParseError)
import Text.Parsec.Pos (newPos)
import Data.Bifunctor
import Data.Either (partitionEithers)
import qualified Debug.Trace as Debug

type Parser a = ParsecT [TokenWithRange] () Identity a

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
matchToken f = tokenPrim show updatePos (f . tokenToken)
  where
    updatePos pos (TokenWithRange _ range) _ =
      let start = rangeStart range
      in newPos (sourceName pos) (positionLine start) (positionColumn start)

parens :: Parser a -> Parser a
parens = between lpar rpar

brackets :: Parser a -> Parser a
brackets = between lbra rbra

-- | Returns the current token with its range without consuming any input,
-- never fails.
currentToken :: Parser TokenWithRange
currentToken = lookAhead $ tokenPrim show (\pos _ _ -> pos) Just

-- | Gets the start position from a range
rangeToPosition :: Range -> Position
rangeToPosition = rangeStart

-- | Returns the position (according to the lexer) of the current token
position :: Parser Position
position = rangeToPosition . tokenRange <$> currentToken

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

-- | Match a comment token and return its content with range
comment :: Parser Comment
comment = do
  pos0 <- position
  content <- matchToken (\case Language.Lexer.Token.Comment s -> Just s; _ -> Nothing)
  return $ Language.AST.Comment content (Range pos0 pos0)

-- | Skip any comments that appear at the current position
skipComments :: Parser ()
skipComments = skipMany (matchToken (\case Language.Lexer.Token.Comment _ -> Just (); _ -> Nothing))

-- | Allows comments before a parser
withComments :: Parser a -> Parser a
withComments p = skipComments >> p

-- | Allows comments before and after a parser
surroundedByComments :: Parser a -> Parser a
surroundedByComments p = skipComments >> p <* skipComments

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
term :: Parser PureTerm
term =  do
    pos0 <- position
    typeOrFunctor <- ident <?> "identifier"
    -- Terms are functors or atoms
    term0 <- (((parens (Functor typeOrFunctor <$> (withComments ((sepBy (surroundedByComments term) (withComments com)) <?> "comma-separated terms")) <?> "functor arguments") <?> "functor application")
          <|> return (Atom (Identity typeOrFunctor))) <*> endRange pos0) <?> "term"

    -- Terms can still be infix operators (which are predefined)
    ((equals >> (Eqq term0 <$> term <*> endRange pos0)) <?> "equality")
      <|> ((leadsto >> (Transition "~>" term0 <$> term <*> endRange pos0)) <?> "transition")
      <|> return term0

-- | Parses a sequence of terms separated (and ended) by the given parser, allowing comments
terms :: Parser a -> Parser [PureTerm]
terms sep = (withComments $ sepBy (surroundedByComments term) (withComments sep)) <?> "terms separated by separator"

-- | Parse a single syntax declaration
syntaxDecl :: Parser SyntaxDecl
syntaxDecl = withRange $ do
      SyntaxDecl <$> sepBy ident com
                 <*> (inn >> ident)
                 <*> (fromMaybe [] <$> parseProductions)
  where parseProductions = optionMaybe (definedAs >> withComments (sepBy (surroundedByComments parseProduction) mid))
        parseProduction :: Parser PureTerm
        parseProduction  = term
-- | Parse a syntax block
syntax :: Parser Decl
syntax =  withRange (Syntax <$>
       (   syntaxToken
        >> (lcba <?> "opening brace after syntax")
        >> (withComments (sepEndBy syntaxDecl (sem >> skipComments)) <?> "syntax declarations")
        <* (rcba <?> "closing brace"))) <?> "syntax block"

-- | Parse a rewrite rule
rewriteRule :: Parser Decl
rewriteRule =  withRange $ Rewrite
           <$> withRange (RewriteDecl
              <$> ident
              <*> parens (sepBy term com)
              <*> (equals >> term))

-- | Parse a single rule declaration
rule :: Parser RuleDecl
rule = ruleToken >> withRange (RuleDecl <$> (str <?> "rule name") <*> (brackets (terms sem) <?> "rule preconditions") <*> (implies >> brackets (terms sem <?> "rule postconditions")) <?> "rule declaration")

-- | Parse a rules section
rules :: Parser Decl
rules = rulesToken >> withRange ((RulesDecl <$> between (lcba >> skipComments) rcba (sepEndBy rule (sem >> skipComments) <?> "rule declarations")) <?> "rules block")

-- | Parses a declarartion for a transition
transition :: Parser Decl
transition = transitionToken >> withRange (flip TransitionDecl <$> identWithRange <*> (leadsto >> return "~>") <*> identWithRange)

parser :: Parser Program
parser = do
  uncurry Program . partitionEithers <$> many programElement <?> "program elements"
   where
    programElement = (Left <$> ((syntax <|> rules <|> transition <|> rewriteRule) <* sem) <?> "declaration")
                 <|> (Right <$> comment <?> "comment")


runParser :: [TokenWithRange] -> Either ParseError Program
runParser =  parse parser "<test>"

-- | Parse a program from string
parseProgram :: String -> Either Error Program
parseProgram = first parseErrorToError . runParser . lex

-- | Add a synthetic semicolon token at the end to help with range calculation
addSyntheticSem :: [TokenWithRange] -> [TokenWithRange]
addSyntheticSem tokens = tokens ++ [TokenWithRange Sem syntheticRange]
  where
    syntheticRange = case tokens of
      [] -> dummyRange
      _ -> let lastRange = tokenRange (last tokens)
               endPos = rangeEnd lastRange
           in Range endPos endPos

-- | Parse a single term from string
parseTerm :: String -> Either Error PureTerm
parseTerm = first parseErrorToError . runTermParser . addSyntheticSem . lex
  where
    runTermParser :: [TokenWithRange] -> Either ParseError PureTerm
    runTermParser = parse (term <* sem <* eof) "<term>"

-- | Parse a single rule from string
parseRule :: String -> Either Error RuleDecl
parseRule = first parseErrorToError . runRuleParser . addSyntheticSem . lex
  where
    runRuleParser :: [TokenWithRange] -> Either ParseError RuleDecl
    runRuleParser = parse (rule <* sem <* eof) "<rule>"
