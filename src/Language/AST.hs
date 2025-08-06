{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.AST(
    Tpy,
    Program(..),
    Comment(..),
    Decl(..),
    SyntaxDecl(..),
    RewriteDecl(..),
    RuleDecl(..),
    Term(..),
    PureTerm,
    variableName,
    Position(..),
    Range(..),
    RangeOf(..),
    functorNames,
    termEqIgnoreRange,
    infixNames,
    module Language.Range
  ) where

import Data.Set
import qualified Data.Set as Set
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromJust, fromMaybe)
import Language.Range
import Data.Kind
import Data.Functor.Identity

-- | A program is a sequence of declarations with optional top-level comments
data Program = Program [Decl] [Comment] deriving (Ord, Eq, Show)

-- | A comment with its position
data Comment = Comment String Range deriving (Ord, Eq, Show)

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
    syntaxProductions :: [PureTerm],
    syntaxRange :: Range
  } deriving (Ord, Eq, Show)

-- | head(term0, term1, ...) = term;
data RewriteDecl = RewriteDecl String -- ^ name
                               [PureTerm] -- ^ argument
                               PureTerm -- ^ body
                               Range
                              deriving (Ord, Eq, Show)

-- | rule NAME [ PRECEDENT ] => [ CONSEQUENT ];
data RuleDecl = RuleDecl String -- ^ rule name
                         [PureTerm] -- ^ the precedent
                         [PureTerm] -- ^ the consequent
                         Range
                        deriving (Ord, Eq, Show)

-- | term \in Term ::= atom
--                  | atom(term0, term1, ...)
--                  | term0 = term1
--                  | term0 ~> term1
data Term f  = Atom (f String) Range
             | Functor String [Term f] Range
             | Eqq (Term f) (Term f) Range
             | Transition String (Term f) (Term f) Range

deriving instance (Show (f String)) => Show (Term f)
deriving instance (Ord (f String)) => Ord (Term f)
deriving instance (Eq (f String)) => Eq (Term f)

type PureTerm = Term Identity

functorNames :: PureTerm -> Set String
functorNames = \case Atom a _ -> Set.singleton $ runIdentity a
                     Functor _ ts _ -> foldMap functorNames ts
                     Eqq t1 t2 _ -> functorNames t1 `Set.union` functorNames t2
                     Transition _ t1 t2 _ -> functorNames t1 `Set.union` functorNames t2

instance RangeOf (Term f) where
  rangeOf = \case Atom _ r -> r
                  Functor _ _ r -> r
                  Eqq _ _ r -> r
                  Transition _ _ _  r -> r

-- | Extract the name of the variable from variables suffixed with numbers
variableName :: String -> String
variableName s = head $ fromMaybe (error $ "could not get variable name of " ++ s) $ matchRegex r s
  where r = mkRegex "([a-zA-Z]+)\\d*"

-- | Compare two terms for equality ignoring range information
termEqIgnoreRange :: (Eq (f String)) => Term f -> Term f -> Bool
termEqIgnoreRange (Atom a1 _) (Atom a2 _) = a1 == a2
termEqIgnoreRange (Functor name1 args1 _) (Functor name2 args2 _) = 
  name1 == name2 && length args1 == length args2 && all (uncurry termEqIgnoreRange) (zip args1 args2)
termEqIgnoreRange (Eqq l1 r1 _) (Eqq l2 r2 _) = 
  termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (Transition n1 l1 r1 _) (Transition n2 l2 r2 _) = 
  n1 == n2 && termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange _ _ = False

-- | Allowed infix names that can be used in a term
infixNames :: [String]
infixNames = [ "=", "~>"]

