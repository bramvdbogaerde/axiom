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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
    
    TypeCon(..),
    fromTypeCon,
    typeSubst,

    XHaskellExpr,
    XTypeAnnot,
    XEmbeddedValue,

    PureTerm,
    PureTerm',

    TypedTerm,
    TypedDecl,
    TypedRewriteDecl,
    TypedSyntaxDecl,
    TypedRuleDecl,
    TypedProgram,

    ParsePhase,
    TypingPhase,

    haskellBlocks,

    typeComment,
    variableName,
    safeVariableName,
    Position(..),
    Range(..),
    RangeOf(..),
    atomNames,
    functorName,
    termEqIgnoreRange,
    infixNames,
    isTermGround,
    HaskellExprExecutor(..),
    HaskellExprRename(..),
    AnnotateType(..),
    module Language.Range
  ) where

import Data.Set hiding (map)
import qualified Data.Set as Set hiding (map)
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromMaybe, mapMaybe)
import Language.Range
import Data.Kind
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Void (Void, absurd)

import Language.Types
import Data.Data
import Control.Monad.Error.Class

-------------------------------------------------------------
-- Phase seperation
-------------------------------------------------------------

type XHaskellExpr :: Type -> Type
type family XHaskellExpr p  -- ^ determines which type to use for haskell expression nodes in each phase
type family XTypeAnnot p    -- ^ determines whether this phase adds type information to the AST element.
type family XEmbeddedValue p -- ^ determines the type of embedded Haskell expression values in each phase

-- | After parsing, the Haskell code is represented as a string since it still has to be compiled
-- and turned into a Template Haskell expression, we do so after type checking and during code generation
-- so that arguments can be unboxed wherever possible.
--
-- In terms of type information, the parser does not add any, this is up to the type checker to
-- update the AST into a typed annotated AST.
data ParsePhase
type instance XHaskellExpr ParsePhase = String
type instance XTypeAnnot ParsePhase = ()
type instance XEmbeddedValue ParsePhase = Void -- cannot construct embedded values during parsing

data TypingPhase
type instance XHaskellExpr TypingPhase = String -- after typing the Haskell expression is still a string
type instance XTypeAnnot TypingPhase = Typ
type instance XEmbeddedValue TypingPhase = Void -- cannot construct embedded values during typing

-- | For all phase add the specified constraint
type ForAllPhases :: (Type -> Constraint) -> Type -> Constraint
type ForAllPhases c p = (c (XHaskellExpr p), c (XTypeAnnot p), c (XEmbeddedValue p))


-------------------------------------------------------------
-- The AST
-------------------------------------------------------------

-- | A program is a sequence of declarations with optional top-level comments
data Program' p  = Program { getDecls :: [Decl' p], getComments ::  [Comment' p] }
deriving instance (ForAllPhases Ord p) => Ord (Program' p)
deriving instance (ForAllPhases Eq p) => Eq (Program' p)
deriving instance (ForAllPhases Show p) => Show (Program' p)
type Program = Program' ParsePhase
type TypedProgram = Program' TypingPhase

-- | Extract the Haskell blocks from the program
haskellBlocks :: Program' p -> [String]
haskellBlocks = mapMaybe extract . getDecls
  where extract (HaskellDecl s _) = Just s
        extract _ = Nothing

-- | A comment with its position
data Comment' p = Comment String Range deriving (Ord, Eq, Show)
type Comment = Comment' ParsePhase

typeComment :: Comment' p -> Comment' q
-- NOTE: id does not work since "id :: a -> a", and p /= q forall p, q.
typeComment (Comment nam ran) = Comment nam ran

-- | Names of types
type Tpy = String

-- | A type constructor in the AST
data TypeCon = TypeApp TypeCon [TypeCon] Range -- ^ T(T1, ..., TN)
             | TypeTyp String Range            -- ^ T
             | TypeVar String Range            -- ^ a
             | TypeHas String Range            -- ${Map String V}
              deriving (Ord, Eq, Show)

instance RangeOf TypeCon where
  rangeOf = \case TypeApp _ _ r -> r
                  TypeTyp _ r   -> r
                  TypeVar _ r   -> r
                  TypeHas _ r   -> r

  -- | Convert a type constructor to a type
fromTypeCon :: TypeCon -> Either String Typ
fromTypeCon = \case (TypeApp (TypeTyp "Set" _) [t] _) -> SetOf <$> fromTypeCon t
                    (TypeApp (TypeTyp "Map" _) [t1, t2] _) -> FunType <$> fromTypeCon t1 <*> fromTypeCon t2
                    (TypeHas h _)                  -> return $ HaskType h
                    (TypeApp (TypeTyp nam _) [] _) -> return $ fromSortName nam
                    ctor -> throwError $ "Invalid type constructor used " ++ show ctor

-- | Substitute variables in a type with another type
typeSubst :: String -> TypeCon -> TypeCon -> TypeCon
typeSubst var typ =
  \case TypeApp opt opr r -> TypeApp (typeSubst var typ opt)
                                     (map (typeSubst var typ) opr) r
        t@TypeTyp {} -> t
        tv@(TypeVar var' _) -> if var == var' then typ else tv
        th@TypeHas {} -> th

-- | A declaration is either a syntax section, rules section, transition
-- declaration or or a rewrite rule.
data Decl' p = Syntax [SyntaxDecl' p] Range
             | Rewrite (RewriteDecl' p) Range
             | RulesDecl [RuleDecl' p] Range
             | TransitionDecl String (Typ, Range) (Typ, Range) Range
             | HaskellDecl String Range
             | Import String Range  -- ^ Import declaration with filename
deriving instance (ForAllPhases Ord p) => Ord (Decl' p)
deriving instance (ForAllPhases Eq p) => Eq (Decl' p)
deriving instance (ForAllPhases Show p) => Show (Decl' p)
type Decl = Decl' ParsePhase
type TypedDecl = Decl' TypingPhase

-- | var in Tpy ::= term0 | term1 | ...
data SyntaxDecl' p = SyntaxDecl {
    syntaxVars :: [String],
    syntaxType :: TypeCon,
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
--                  | value
data Term' p f  = Atom (f String) (XTypeAnnot p)  Range
                | Functor String [Term' p f]  (XTypeAnnot p) Range
                | Eqq (Term' p f) (Term' p f) (XTypeAnnot p) Range
                | Neq (Term' p f) (Term' p f) (XTypeAnnot p) Range
                | Transition String (Term' p f) (Term' p f) (XTypeAnnot p)  Range
                | HaskellExpr (XHaskellExpr p) (XTypeAnnot p) Range
                | TermValue Value (XTypeAnnot p) Range
                | IncludedIn String (Term' p f) Range
                | SetOfTerms (Set (Term' p f)) (XTypeAnnot p) Range
                | TermHask (XEmbeddedValue p) (XTypeAnnot p) Range
type Term = Term' ParsePhase

deriving instance (Ord (f String), ForAllPhases Ord p) => Ord (Term' p f)
deriving instance (Eq (f String), ForAllPhases  Eq p) => Eq (Term' p f)

-- | Show instance that displays terms as they appear in source code
instance (Show (f String), ForAllPhases Show p) => Show (Term' p f) where
  show (Atom name _ _) = show name
  show (Functor fname [] _ _) = fname ++ "()"
  show (Functor fname args _ _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right  _ _) = show left ++ " = " ++ show right
  show (Neq left right _ _) = show left ++ " /= " ++ show right
  show (Transition tname left right _ _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr t _) = "${" ++ show expr ++ "} :: " ++ show t
  show (TermValue value _ _) = show value
  show (IncludedIn var term _) = var ++ " in " ++ show term
  show (SetOfTerms terms _ _) = "{" ++ intercalate ", " (Prelude.map show $ Set.toList terms) ++ "}"
  show (TermHask value _ _) = "TermHask(" ++ show value ++ ")"

-- | Specialized Show instance for PureTerm that doesn't show Identity wrapper
instance {-# OVERLAPPING #-} Show PureTerm where
  show (Atom (Identity name)_ _) = name
  show (Functor fname [] _ _) = fname ++ "()"
  show (Functor fname args _ _) = fname ++ "(" ++ intercalate ", " (Prelude.map show args) ++ ")"
  show (Eqq left right _ _) = show left ++ " = " ++ show right
  show (Neq left right _ _) = show left ++ " /= " ++ show right
  show (Transition tname left right _ _) = show left ++ " " ++ tname ++ " " ++ show right
  show (HaskellExpr expr _ _) = "${" ++ expr ++ "}"
  show (TermValue value _ _) = show value
  show (IncludedIn var term _) = var ++ " in " ++ show term
  show (SetOfTerms terms _ _) = "{" ++ intercalate ", " (Prelude.map show $ Set.toList terms) ++ "}"
  show (TermHask v _ _) = absurd v

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
                  TermValue {} -> Set.empty
                  IncludedIn var term _ -> Set.singleton var `Set.union` atomNames term
                  SetOfTerms terms _ _ -> foldMap atomNames terms
                  TermHask {} -> Set.empty


-- | Returns the name of the functor embedded in the term (if any),
-- also matches on equality and transition relations and returns the obvious functor names for them.
functorName :: PureTerm' p -> Maybe String
functorName = \case Functor nam _ _ _ -> Just nam
                    Eqq {} -> Just "="
                    Neq {} -> Just "/="
                    Transition {} -> Just "~>"
                    HaskellExpr {} -> Nothing
                    TermValue {} -> Nothing
                    _ -> Nothing

instance RangeOf (Term' p f) where
  rangeOf = \case Atom _ _ r -> r
                  Functor _ _ _ r -> r
                  Eqq _ _ _ r -> r
                  Neq _ _ _ r -> r
                  Transition _ _ _ _  r -> r
                  HaskellExpr _ _ r -> r
                  TermValue _ _ r -> r
                  IncludedIn _ _ r -> r
                  SetOfTerms _ _ r -> r
                  TermHask _ _ r -> r

-- | Extract the name of the variable from variables suffixed with numbers
variableName :: String -> String
variableName s = fromMaybe (error $ "could not get variable name of " ++ s) $ safeVariableName s

-- | Same as 'variableName' but returns 'Nothing' if the variable cannot be extracted
safeVariableName :: String -> Maybe String
safeVariableName s = head <$> matchRegex r s
  where r = mkRegex "([a-zA-Z]+)\\d*"

-- | Allowed infix names that can be used in a term
infixNames :: [String]
infixNames = [ "=", "/=", "~>"]

-------------------------------------------------------------
-- Predicates
-------------------------------------------------------------

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
termEqIgnoreRange (TermValue value1 _ _) (TermValue value2 _ _) = value1 == value2
termEqIgnoreRange (IncludedIn v1 t1 _) (IncludedIn v2 t2 _) = v1 == v2 && termEqIgnoreRange t1 t2
termEqIgnoreRange (SetOfTerms terms1 _ _) (SetOfTerms terms2 _ _) =
  Set.size terms1 == Set.size terms2 &&
  all (\t1 -> any (termEqIgnoreRange t1) (Set.toList terms2)) (Set.toList terms1)
termEqIgnoreRange (TermHask value1 _ _) (TermHask value2 _ _) = value1 == value2
termEqIgnoreRange _ _ = False

-- | Check whether the term is fully ground (i.e., does not contain any atoms)
isTermGround :: Term' p f -> Bool
isTermGround = \case
  Atom {} -> False
  Functor _ args _ _ -> all isTermGround args
  Eqq left right _ _ -> isTermGround left && isTermGround right
  Neq left right _ _ -> isTermGround left && isTermGround right
  Transition _ left right _ _ -> isTermGround left && isTermGround right
  HaskellExpr {} -> True
  TermValue {} -> True
  IncludedIn _ term _ -> isTermGround term
  SetOfTerms terms _ _ -> all isTermGround terms
  TermHask {} -> True


-------------------------------------------------------------
-- Phase-dependent Haskell expression execution
-------------------------------------------------------------

-- | Type class for executing Haskell expressions in different phases
class HaskellExprExecutor p where
  -- | Execute a Haskell expression, potentially returning an error or a term
  executeHaskellExpr :: XHaskellExpr p -> Map.Map String (PureTerm' p) -> Either String (PureTerm' p)

-- | ParsePhase instance: Haskell expressions cannot be executed before code generation
instance HaskellExprExecutor ParsePhase where
  executeHaskellExpr _ _ = Left "Haskell expressions cannot be executed in ParsePhase - code generation required"

-- | TypingPhase instance: Haskell expressions are not executable (they're just strings)
instance HaskellExprExecutor TypingPhase where
  executeHaskellExpr _ _ = Left "Haskell expressions in TypingPhase are not executable - code generation required"

-- | Type class for associating a type with a term in a phase-independent manner 
class AnnotateType p where
  typeAnnot :: Proxy p -> Typ -> XTypeAnnot p

instance AnnotateType ParsePhase where
  typeAnnot _ = const ()
instance AnnotateType TypingPhase where
  typeAnnot _ = id

-- | Phase-indexed type class for associating a renaming mapping with the Haskell expression
class HaskellExprRename p where
  -- | Associate the given renaming mapping with the haskell expression (if possible)
  haskellExprRename :: Map String String
                    -> XHaskellExpr p
                    -> XHaskellExpr p
  -- | Returns the free variables of the Haskell expression if the c
  -- current representation supports it.
  haskellExprFreeVars :: XHaskellExpr p -> Set String
instance HaskellExprRename ParsePhase where
  haskellExprRename = const id
  haskellExprFreeVars = const Set.empty
instance HaskellExprRename TypingPhase where
  haskellExprRename = const id
  haskellExprFreeVars = const Set.empty
