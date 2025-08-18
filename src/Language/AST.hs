{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
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
    atomNames,
    functorName,
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
import Data.List (intercalate)

-------------------------------------------------------------
-- Phase seperation
-------------------------------------------------------------

type XHaskellExpr :: Type -> Type
type family XHaskellExpr p  -- determines which type to use for haskell expression nodes in each phase

data ParsePhase
type instance XHaskellExpr ParsePhase = String

data CodeGenPhase
type instance XHaskellExpr CodeGenPhase  = ([PureTerm] -> PureTerm)

-------------------------------------------------------------
-- The AST
-------------------------------------------------------------

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
                        deriving (Ord, Eq)

-- | Show instance that displays rules as they appear in source code
instance Show RuleDecl where
  show (RuleDecl name precedent consequent _) =
    "rule " ++ name ++
    " [" ++ intercalate ", " (Prelude.map show precedent) ++ "]" ++
    " => " ++
    "[" ++ intercalate ", " (Prelude.map show consequent) ++ "]" ++
    ";"

-- | term \in Term ::= atom
--                  | atom(term0, term1, ...)
--                  | term0 = term1
--                  | term0 /= term1
--                  | term0 ~> term1
--                  | ${ haskell_expr }
data Term f  = Atom (f String) Range
             | Functor String [Term f] Range
             | Eqq (Term f) (Term f) Range
             | Neq (Term f) (Term f) Range
             | Transition String (Term f) (Term f) Range
             | HaskellExpr String Range

deriving instance (Ord (f String)) => Ord (Term f)
deriving instance (Eq (f String)) => Eq (Term f)

-- | Show instance that displays terms as they appear in source code
instance (Show (f String)) => Show (Term f) where
  show (Atom name _) = show name
  show (Functor fname [] _) = fname
  show (Functor fname args _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right _) = show left ++ " = " ++ show right
  show (Neq left right _) = show left ++ " /= " ++ show right
  show (Transition tname left right _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr _) = "${" ++ expr ++ "}"

-- | Specialized Show instance for PureTerm that doesn't show Identity wrapper
instance {-# OVERLAPPING #-} Show PureTerm where
  show (Atom (Identity name) _) = name
  show (Functor fname [] _) = fname
  show (Functor fname args _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right _) = show left ++ " = " ++ show right
  show (Neq left right _) = show left ++ " /= " ++ show right
  show (Transition tname left right _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr _) = "${" ++ expr ++ "}"

type PureTerm = Term Identity

atomNames :: PureTerm -> Set String
atomNames = \case Atom a _ -> Set.singleton $ runIdentity a
                  Functor _ ts _ -> foldMap atomNames ts
                  Eqq t1 t2 _ -> atomNames t1 `Set.union` atomNames t2
                  Neq t1 t2 _ -> atomNames t1 `Set.union` atomNames t2
                  Transition _ t1 t2 _ -> atomNames t1 `Set.union` atomNames t2
                  HaskellExpr _ _ -> Set.empty


-- | Returns the name of the functor embedded in the term (if any),
-- also matches on equality and transition relations and returns the obvious functor names for them.
functorName :: PureTerm -> Maybe String
functorName = \case Functor nam _ _ -> Just nam
                    Eqq {} -> Just "="
                    Neq {} -> Just "/="
                    Transition {} -> Just "~>"
                    HaskellExpr {} -> Nothing
                    _ -> Nothing

instance RangeOf (Term f) where
  rangeOf = \case Atom _ r -> r
                  Functor _ _ r -> r
                  Eqq _ _ r -> r
                  Neq _ _ r -> r
                  Transition _ _ _  r -> r
                  HaskellExpr _ r -> r

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
termEqIgnoreRange (Neq l1 r1 _) (Neq l2 r2 _) =
  termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (Transition n1 l1 r1 _) (Transition n2 l2 r2 _) =
  n1 == n2 && termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (HaskellExpr expr1 _) (HaskellExpr expr2 _) = expr1 == expr2
termEqIgnoreRange _ _ = False

-- | Allowed infix names that can be used in a term
infixNames :: [String]
infixNames = [ "=", "/=", "~>"]

