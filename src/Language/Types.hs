{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module Language.Types(
    -- * Type definitions
    Typ(..), 
    TypHask(..), 
    Value(..),
    
    -- * Type conversions
    toSortName,
    fromSortName,
    asType,
    toValue,
    typeOf,
    primTyp,

    -- * Type modifiers
    narrowType,  
  
    -- * Constructing types
    curryFunType,

    -- * Type extractions
    referencedTypes,

    -- * Type predicates
    isUserDefined, 
    isSubtypeOf,
    
    -- * Template Haskell
    typHaskEx,

    -- * Typing environments
    TypingContext,
    Subtyping,
    emptySubtyping,
    fromAdjacencyList,
    toAdjacencyList
  ) where

import Language.Haskell.TH.Syntax hiding (Type)
import Data.Dynamic
import Data.Kind
import Data.Singletons
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bifunctor (second)
import GHC.TypeError
import Data.Graph (UnlabeledGraph)
import qualified Data.Graph as Graph


------------------------------------------------------------
-- Typing environments 
------------------------------------------------------------

-- | A mapping from term names to their types
type TypingContext = Map String Typ
-- | A hierarchy of types
type Subtyping     = UnlabeledGraph Typ

-- | Create an empty subtyping graph
emptySubtyping :: Subtyping
emptySubtyping = Graph.empty

-- | Create a subtyping graph from an adjacency list representation
fromAdjacencyList :: [(Typ, [Typ])] -> Subtyping
fromAdjacencyList = foldr (\(from, tos) g -> foldr (Graph.addEdge () from) g tos) Graph.empty

-- | Extract an adjacency list representation from a subtyping graph
toAdjacencyList :: Subtyping -> [(Typ, [Typ])]
toAdjacencyList = fmap (second (fmap fst . Set.toList)) . Map.toList . Graph.getAdjList

-- | To define subtyping rules we create a new judgement "T1 <: T2". In practice, this judgement needs a 'CheckingContext'
-- to access the subtyping graph, however, we omit this here for brevity and assume that can ask whether a type is a subtype
-- of another "out of thin air". The rules below define the judgement
--
-- (1) T <: T (reflexive)
-- (2) Void <: T (void is a subtype of everything)
-- (3) T <: Any  (any is a supertype of everything)
-- (4) T1 <: T2 => Set(T1) <: Set(T2) (covariance of sets)
-- (5) T2 <: T1 && T3 <: T4 => T1 -> T3 <: T2 -> T4 (contravariance in argument, covariance in return)
-- (6) contravariance of map keys and covariance of map values
--
-- Other than these rules, subtyping relationships can be defined by the user of AnalysisLang resulting
-- in additional "<:" judgements to be created (cf. 'Language.TypeCheck').
--
-- Returns true if the first type is a subtype of the second type
isSubtypeOf :: Typ -> Typ -> Subtyping -> Bool
isSubtypeOf _ AnyType = const True
isSubtypeOf AnyType _ = const False
isSubtypeOf VoidType _ = const True
isSubtypeOf _ VoidType = const False
isSubtypeOf (SetOf t1) (SetOf t2) = isSubtypeOf t1 t2
isSubtypeOf (MapOf k1 v1) (MapOf k2 v2) =
  \subtyping -> isSubtypeOf k2 k1 subtyping && isSubtypeOf v1 v2 subtyping
-- TODO: functions
isSubtypeOf t1 t2 
  | t1 == t2 = const True
  | otherwise = Graph.isReachable t1 t2

-- | Narrow the type to the most specific argument, returns Nothing if types are incompatible
narrowType :: Typ -> Typ -> Subtyping -> Maybe Typ
narrowType from to subtyping
  | isSubtypeOf from to subtyping = Just from
  | isSubtypeOf to from subtyping = Just to
  | otherwise = Nothing  -- Types are incompatible
  
------------------------------------------------------------
-- Utilities
------------------------------------------------------------

type Assoc :: forall k1 k2 . k1 -> k2 -> Type 
data Assoc (a :: k1) (b :: k2)

type family Find (k :: [Type]) (t :: k1) :: Type where
  Find '[] t = TypeError (Text "The type " :<>: ShowType t :<>: Text "was not found")
  Find (Assoc k t ': _) k = t
  Find (_ ': ts) t = Find ts t  


------------------------------------------------------------
-- Type definitions
------------------------------------------------------------

-- | The different types that can be taken by a term
data Typ = Sort String     -- ^ a user-defined (or system) sort
         | IntType         -- ^ values are strings 
         | StrType         -- ^ values are integers
         | BooType         -- ^ values are boolean
         | SetOf Typ       -- ^ a set of values from the given type
         | AnyType
         | VoidType        -- ^ type for expressions that don't have a meaningful type 
         | HaskType String 
         | FunType Typ Typ -- ^ type of total functions
         | MapOf Typ Typ   -- ^ type of partial functions
        deriving (Ord, Eq, Show)        

-- | Checks whether the type is a user-defined type
isUserDefined :: Typ -> Bool
isUserDefined (Sort _) = True
isUserDefined _ = False

-- | Extract all referenced types 
referencedTypes :: Typ -> [Typ]
referencedTypes (SetOf t) = referencedTypes t
referencedTypes (FunType t1 t2) = referencedTypes t1 ++ referencedTypes t2
referencedTypes (MapOf t1 t2) = referencedTypes t1 ++ referencedTypes t2
referencedTypes t = [t]

-- | Curry a list of types into a "FunType"
curryFunType :: [Typ] -> Typ
curryFunType [] = VoidType
curryFunType ts = foldr1 FunType ts 

-- | LEGACY (TODO): converts a user-defined type to primitive type
primTyp :: Typ -> Typ
primTyp (Sort str) = fromSortName str
primTyp t = t

-- | Association of type tags to their Haskell types
data TypHask (k :: Typ)  where
  SIntType  :: TypHask IntType
  SStrType  :: TypHask StrType 
  SBooType  :: TypHask BooType
  SAnyType  :: TypHask AnyType
  

instance Show (TypHask k) where
  show SIntType = "IntType"
  show SStrType = "StrType"
  show SAnyType = "AnyType"
  show SBooType = "BooType"

type instance Sing = TypHask

type TypHaskAssoc = '[
    Assoc IntType Int,
    Assoc StrType String,
    Assoc BooType Bool,
    Assoc AnyType Dynamic
  ]

------------------------------------------------------------
-- Value domain
------------------------------------------------------------

-- | Primitive value domain
data Value where
  IntValue :: Int -> Value
  StrValue :: String -> Value
  BooValue :: Bool -> Value
  deriving (Eq, Ord)

instance Show Value where
  show = \case IntValue i -> show i
               StrValue s -> s
               BooValue b -> show b

------------------------------------------------------------
-- Type conversions
------------------------------------------------------------

-- | Converts a dynamic type to a fixed Haskell type (if possible)
asType :: TypHask k -> Value -> Maybe (Find TypHaskAssoc k)
asType SIntType (IntValue v) = Just v
asType SStrType (StrValue v) = Just v
asType SBooType (BooValue v) = Just v
asType _ _ = Nothing

-- | Convert Haskell types to Value
toValue :: Find TypHaskAssoc k -> TypHask k -> Value
toValue v SIntType = IntValue v
toValue v SStrType = StrValue v
toValue _ s = error $ "could not convert value of type " ++ show s

-- | Get the type of a Value
typeOf :: Value -> Typ
typeOf (IntValue _) = IntType
typeOf (StrValue _) = StrType
typeOf (BooValue _) = BooType

------------------------------------------------------------
-- Template Haskell
------------------------------------------------------------

-- | Template haskell integration
instance Lift Typ where 
  liftTyped (Sort str) = [|| Sort $$(liftTyped str) ||]
  liftTyped IntType   = [|| IntType ||]
  liftTyped StrType   = [|| StrType ||]
  liftTyped BooType   = [|| BooType ||]
  liftTyped AnyType   = [|| AnyType ||]
  liftTyped VoidType  = [|| VoidType ||]
  liftTyped (SetOf t) = [|| SetOf $$(liftTyped t) ||]
  liftTyped (HaskType s)  = [|| HaskType $$(liftTyped s) ||]
  liftTyped (FunType a b) = [|| FunType $$(liftTyped a) $$(liftTyped b) ||]
  liftTyped (MapOf a b)   = [|| MapOf $$(liftTyped a) $$(liftTyped b) ||]

instance Lift Value where
  liftTyped (IntValue i) = [|| IntValue $$(liftTyped i) ||]
  liftTyped (StrValue s) = [|| StrValue $$(liftTyped s) ||]
  liftTyped (BooValue b) = [|| BooValue $$(liftTyped b) ||]  


-- | Convert a Typ to the corresponding TypHask instance
typHaskEx :: Typ -> Q Exp
typHaskEx IntType = [| SIntType |]
typHaskEx StrType = [| SStrType |]
typHaskEx AnyType = [| SAnyType |]
typHaskEx BooType = [| SBooType |]
typHaskEx t = error $ "could not convert " ++ show t

------------------------------------------------------------
-- String <-> Type
------------------------------------------------------------

-- | Convert a type name to a built-in type
fromSortName :: String -> Typ
fromSortName "Int" = IntType
fromSortName "String" = StrType
fromSortName "Any" = AnyType
fromSortName "Bool" = BooType
fromSortName str = Sort str

-- | Convert a Typ to its corresponding sort name string (inverse of fromSortName)
toSortName :: Typ -> String
toSortName (Sort str) = str
toSortName IntType = "Int"
toSortName StrType = "String"
toSortName BooType = "Bool"
toSortName AnyType = "Any"
toSortName (SetOf typ) = "Set(" ++ toSortName typ ++ ")"
toSortName VoidType = "Void"
toSortName (FunType a b) = toSortName a ++ "->" ++ toSortName b
toSortName (MapOf a b) = toSortName a ++ "↦" ++ toSortName b
toSortName (HaskType s) = "${" ++ s ++ "}"
