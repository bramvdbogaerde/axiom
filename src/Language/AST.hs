{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Language.AST(
    Tpy,
    Program,
    Comment,
    Decl,
    SyntaxDecl,
    RewriteDecl,
    RuleDecl,
    Term,

    ForAllPhases,

    Program'(..),
    Comment'(..),
    Decl'(..),
    SyntaxDecl'(..),
    RewriteDecl'(..),
    RuleDecl'(..),
    Term'(..),

    PureTerm,
    PureTerm',

    TypedTerm,
    TypedDecl,
    TypedRewriteDecl,
    TypedSyntaxDecl,
    TypedRuleDecl,
    TypedProgram,

    typeComment,
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

import Language.Types

-------------------------------------------------------------
-- Phase seperation
-------------------------------------------------------------

type XHaskellExpr :: Type -> Type
type family XHaskellExpr p  -- ^ determines which type to use for haskell expression nodes in each phase
type family XTypeAnnot p    -- ^ determines whether this phase adds type information to the AST element.

-- | After parsing, the Haskell code is represented as a string since it still has to be compiled
-- and turned into a Template Haskell expression, we do so after type checking and during code generation
-- so that arguments can be unboxed wherever possible.
--
-- In terms of type information, the parser does not add any, this is up to the type checker to
-- update the AST into a typed annotated AST.
data ParsePhase
type instance XHaskellExpr ParsePhase = String
type instance XTypeAnnot ParsePhase = ()

data TypingPhase
type instance XHaskellExpr TypingPhase = String -- after typing the Haskell expression is still a string
type instance XTypeAnnot TypingPhase = Typ

-- | Phase after code generation. Haskell expressions are turned in pure functions
-- that have unified terms are their argument. The solver makes sure that all terms
-- are ground before passing them to the function and tries to unwrap the data
-- into pure Haskell types wherever possible.
data CodeGenPhase
type instance XHaskellExpr CodeGenPhase  = ([PureTerm] -> PureTerm)

-- | For all phase add the specified constraint
type ForAllPhases :: (Type -> Constraint) -> Type -> Constraint
type ForAllPhases c p = (c (XHaskellExpr p), c (XTypeAnnot p))


-------------------------------------------------------------
-- The AST
-------------------------------------------------------------

-- | A program is a sequence of declarations with optional top-level comments
data Program' p  = Program [Decl' p] [Comment' p]
deriving instance (ForAllPhases Ord p) => Ord (Program' p)
deriving instance (ForAllPhases Eq p) => Eq (Program' p)
deriving instance (ForAllPhases Show p) => Show (Program' p)
type Program = Program' ParsePhase
type TypedProgram = Program' TypingPhase

-- | A comment with its position
data Comment' p = Comment String Range deriving (Ord, Eq, Show)
type Comment = Comment' ParsePhase

typeComment :: Comment' p -> Comment' q
-- NOTE: id does not work since "id :: a -> a"
typeComment (Comment nam ran) = Comment nam ran

-- | Names of types
type Tpy = String

-- | A declaration is either a syntax section, rules section, transition
-- declaration or or a rewrite rule.
data Decl' p = Syntax [SyntaxDecl' ParsePhase] Range
             | Rewrite (RewriteDecl' ParsePhase) Range
             | RulesDecl [RuleDecl' p] Range
             | TransitionDecl String (Typ, Range) (Typ, Range) Range
deriving instance (ForAllPhases Ord p) => Ord (Decl' p)
deriving instance (ForAllPhases Eq p) => Eq (Decl' p)
deriving instance (ForAllPhases Show p) => Show (Decl' p)
type Decl = Decl' ParsePhase
type TypedDecl = Decl' TypingPhase

-- | var in Tpy ::= term0 | term1 | ...
data SyntaxDecl' p = SyntaxDecl {
    syntaxVars :: [String],
    syntaxType :: String,
    syntaxProductions :: [PureTerm' p],
    syntaxRange :: Range
  }

deriving instance (ForAllPhases Ord p) => Ord (SyntaxDecl' p)
deriving instance (ForAllPhases Eq p) => Eq (SyntaxDecl' p)
deriving instance (ForAllPhases Show p) => Show (SyntaxDecl' p)
type SyntaxDecl = SyntaxDecl' ParsePhase
type TypedSyntaxDecl = SyntaxDecl' TypingPhase


-- | head(term0, term1, ...) = term;
data RewriteDecl' p = RewriteDecl String -- ^ name
                                  [PureTerm' p] -- ^ argument
                                  (PureTerm' p) -- ^ body
                                  Range
deriving instance (ForAllPhases Ord p) => Ord (RewriteDecl' p)
deriving instance (ForAllPhases Eq p) => Eq (RewriteDecl' p)
deriving instance (ForAllPhases Show p) => Show (RewriteDecl' p)
type RewriteDecl = RewriteDecl' ParsePhase
type TypedRewriteDecl = RewriteDecl' TypingPhase


-- | rule NAME [ PRECEDENT ] => [ CONSEQUENT ];
data RuleDecl' p = RuleDecl String -- ^ rule name
                           [PureTerm' p] -- ^ the precedent
                           [PureTerm' p] -- ^ the consequent
                           Range
deriving instance (ForAllPhases Ord p) => Ord (RuleDecl' p)
deriving instance (ForAllPhases Eq p) => Eq (RuleDecl' p)
type RuleDecl = RuleDecl' ParsePhase
type TypedRuleDecl = RuleDecl' TypingPhase

-- | Show instance that displays rules as they appear in source code
instance (ForAllPhases Show p) => Show (RuleDecl' p) where
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
data Term' p f  = Atom (f String) (XTypeAnnot p)  Range
                | Functor String [Term' p f]  (XTypeAnnot p) Range
                | Eqq (Term' p f) (Term' p f) (XTypeAnnot p) Range
                | Neq (Term' p f) (Term' p f) (XTypeAnnot p) Range
                | Transition String (Term' p f) (Term' p f) (XTypeAnnot p)  Range
                | HaskellExpr (XHaskellExpr p) (XTypeAnnot p) Range
type Term = Term' ParsePhase

deriving instance (Ord (f String), ForAllPhases Ord p) => Ord (Term' p f)
deriving instance (Eq (f String), ForAllPhases  Eq p) => Eq (Term' p f)

-- | Show instance that displays terms as they appear in source code
instance (Show (f String), ForAllPhases Show p) => Show (Term' p f) where
  show (Atom name _ _) = show name
  show (Functor fname [] _ _) = fname
  show (Functor fname args _ _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right  _ _) = show left ++ " = " ++ show right
  show (Neq left right _ _) = show left ++ " /= " ++ show right
  show (Transition tname left right _ _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr _ _) = "${" ++ show expr ++ "}"

-- | Specialized Show instance for PureTerm that doesn't show Identity wrapper
instance {-# OVERLAPPING #-} Show PureTerm where
  show (Atom (Identity name)_ _) = name
  show (Functor fname [] _ _) = fname
  show (Functor fname args _ _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right _ _) = show left ++ " = " ++ show right
  show (Neq left right _ _) = show left ++ " /= " ++ show right
  show (Transition tname left right _ _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr _ _) = "${" ++ expr ++ "}"

type PureTerm = PureTerm' ParsePhase
type PureTerm' p = Term' p Identity
-- | A typed pure term
type TypedTerm = PureTerm' TypingPhase

atomNames :: PureTerm' p -> Set String
atomNames = \case Atom a _ _ -> Set.singleton $ runIdentity a
                  Functor _ ts _ _ -> foldMap atomNames ts
                  Eqq t1 t2 _ _ -> atomNames t1 `Set.union` atomNames t2
                  Neq t1 t2 _ _ -> atomNames t1 `Set.union` atomNames t2
                  Transition _ t1 t2 _ _ -> atomNames t1 `Set.union` atomNames t2
                  HaskellExpr {} -> Set.empty


-- | Returns the name of the functor embedded in the term (if any),
-- also matches on equality and transition relations and returns the obvious functor names for them.
functorName :: PureTerm' p -> Maybe String
functorName = \case Functor nam _ _ _ -> Just nam
                    Eqq {} -> Just "="
                    Neq {} -> Just "/="
                    Transition {} -> Just "~>"
                    HaskellExpr {} -> Nothing
                    _ -> Nothing

instance RangeOf (Term' p f) where
  rangeOf = \case Atom _ _ r -> r
                  Functor _ _ _ r -> r
                  Eqq _ _ _ r -> r
                  Neq _ _ _ r -> r
                  Transition _ _ _ _  r -> r
                  HaskellExpr _ _ r -> r

-- | Extract the name of the variable from variables suffixed with numbers
variableName :: String -> String
variableName s = head $ fromMaybe (error $ "could not get variable name of " ++ s) $ matchRegex r s
  where r = mkRegex "([a-zA-Z]+)\\d*"

-- | Compare two terms for equality ignoring range information
termEqIgnoreRange :: (Eq (f String), ForAllPhases Eq p) => Term' p f -> Term' p f -> Bool
termEqIgnoreRange (Atom a1 _ _) (Atom a2 _ _) = a1 == a2
termEqIgnoreRange (Functor name1 args1 _ _) (Functor name2 args2 _ _) =
  name1 == name2 && length args1 == length args2 && all (uncurry termEqIgnoreRange) (zip args1 args2)
termEqIgnoreRange (Eqq l1 r1 _ _) (Eqq l2 r2 _ _) =
  termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (Neq l1 r1 _ _) (Neq l2 r2  _ _) =
  termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (Transition n1 l1 r1 _ _) (Transition n2 l2 r2 _ _) =
  n1 == n2 && termEqIgnoreRange l1 l2 && termEqIgnoreRange r1 r2
termEqIgnoreRange (HaskellExpr expr1 _ _) (HaskellExpr expr2 _ _) = expr1 == expr2
termEqIgnoreRange _ _ = False

-- | Allowed infix names that can be used in a term
infixNames :: [String]
infixNames = [ "=", "/=", "~>"]

