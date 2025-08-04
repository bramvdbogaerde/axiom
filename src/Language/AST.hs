{-# LANGUAGE LambdaCase #-}
module Language.AST(
    Tpy,
    Program(..),
    Decl(..),
    SyntaxDecl(..),
    RewriteDecl(..),
    RuleDecl(..),
    Term(..),
    variableName,
    Position(..),
    Range(..),
    RangeOf(..),
    module Language.Range
  ) where

import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromJust, fromMaybe)
import Language.Range

-- | A program is a sequence of declarations
newtype Program = Program [Decl] deriving (Ord, Eq, Show)

-- | Names of types
type Tpy = String

-- | A declaration is either a syntax section, rules section, transition
-- declaration or or a rewrite rule.
data Decl = Syntax [SyntaxDecl] Range
          | Rewrite RewriteDecl Range 
          | RulesDecl [RuleDecl] Range
          | TransitionDecl String (Tpy, Range) (Tpy, Range) Range
          deriving (Ord, Eq, Show)

-- | var in Tpy ::= term0 | term1 | ...
data SyntaxDecl = SyntaxDecl {
    syntaxVars :: [String],
    syntaxType :: String,
    syntaxProductions :: [Term],
    syntaxRange :: Range
  } deriving (Ord, Eq, Show)

-- | head(term0, term1, ...) = term;
data RewriteDecl = RewriteDecl String -- ^ name
                               [Term] -- ^ argument
                               Term -- ^ body
                               Range
                              deriving (Ord, Eq, Show)

-- | rule NAME [ PRECEDENT ] => [ CONSEQUENT ];
data RuleDecl = RuleDecl String -- ^ rule name
                         [Term] -- ^ the precedent
                         [Term] -- ^ the consequent
                         Range
                        deriving (Ord, Eq, Show)

-- | term \in Term ::= atom
--                  | atom(term0, term1, ...)
--                  | term0 = term1
--                  | term0 ~> term1
data Term = Atom String Range
          | Functor String [Term] Range
          | Eqq Term Term Range
          | Transition String Term Term Range
          deriving (Ord, Eq, Show)

instance RangeOf Term where
  rangeOf = \case Atom _ r -> r
                  Functor _ _ r -> r
                  Eqq _ _ r -> r
                  Transition _ _ _  r -> r

-- | Extract the name of the variable from variables suffixed with numbers
variableName :: String -> String
variableName s = head $ fromMaybe (error $ "could not get variable name of " ++ s) $ matchRegex r s
  where r = mkRegex "([a-zA-Z]+)\\d*"

-- | Allowed infix names that can be used in a term
infixNames :: [String]
infixNames = [ "=", "~>"]

