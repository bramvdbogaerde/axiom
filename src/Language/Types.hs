{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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
    
    -- * Template Haskell
    typHaskEx
  ) where

import Language.Haskell.TH.Syntax hiding (Type)
import Data.Dynamic hiding (Type)
import Data.Kind
import Data.Singletons
import GHC.TypeError

------------------------------------------------------------
-- Utilities
------------------------------------------------------------

type Assoc :: forall k1 k2 . k1 -> k2 -> * 
data Assoc (a :: k1) (b :: k2)

type family Find (k :: [Type]) (t :: k1) :: Type where
  Find '[] t = TypeError (Text "The type " :<>: ShowType t :<>: Text "was not found")
  Find (Assoc k t ': _) k = t
  Find (_ ': ts) t = Find ts t  


------------------------------------------------------------
-- Type definitions
------------------------------------------------------------

-- | The different types that can be taken by a term
data Typ = Sort String -- ^ a user-defined (or system) sort
         | IntType     -- ^ values are strings 
         | StrType     -- ^ values are integers
         | SetOf Typ   -- ^ a set of values from the given type
         | AnyType
         | VoidType    -- ^ type for expressions that don't have a meaningful type (incompatible with all others)
        deriving (Ord, Eq, Show)        

-- | LEGACY (TODO): converts a user-defined type to primitive type
primTyp :: Typ -> Typ
primTyp (Sort str) = fromSortName str
primTyp t = t

-- | Association of type tags to their Haskell types
data TypHask (k :: Typ)  where
  SIntType  :: TypHask IntType
  SStrType  :: TypHask StrType 
  SAnyType  :: TypHask AnyType

type instance Sing = TypHask

type TypHaskAssoc = '[
    Assoc IntType Int,
    Assoc StrType String,
    Assoc AnyType Dynamic
  ]

------------------------------------------------------------
-- Value domain
------------------------------------------------------------

-- | Primitive value domain
data Value where
  IntValue :: Int -> Value
  StrValue :: String -> Value
  deriving (Eq, Ord, Show)

------------------------------------------------------------
-- Type conversions
------------------------------------------------------------

-- | Converts a dynamic type to a fixed Haskell type (if possible)
asType :: TypHask k -> Value -> Maybe (Find TypHaskAssoc k)
asType SIntType (IntValue v) = Just v
asType SStrType (StrValue v) = Just v
asType _ _ = Nothing

-- | Convert Haskell types to Value
toValue :: Find TypHaskAssoc k -> TypHask k -> Value
toValue v SIntType = IntValue v
toValue v SStrType = StrValue v

-- | Get the type of a Value
typeOf :: Value -> Typ
typeOf (IntValue _) = IntType
typeOf (StrValue _) = StrType

------------------------------------------------------------
-- Template Haskell
------------------------------------------------------------

-- | Template haskell integration
instance Lift Typ where 
  liftTyped (Sort str) = [|| Sort $$(liftTyped str) ||]
  liftTyped IntType = [|| IntType ||]
  liftTyped StrType = [|| StrType ||]

instance Lift Value where
  liftTyped (IntValue i) = [|| IntValue $$(liftTyped i) ||]
  liftTyped (StrValue s) = [|| StrValue $$(liftTyped s) ||]


-- | Convert a Typ to the corresponding TypHask instance
typHaskEx :: Typ -> Q Exp
typHaskEx IntType = [| SIntType |]
typHaskEx StrType = [| SStrType |]

------------------------------------------------------------
-- String <-> Type
------------------------------------------------------------

-- | Convert a type name to a built-in type
fromSortName :: String -> Typ
fromSortName "Int" = IntType
fromSortName "String" = StrType
fromSortName "Any" = AnyType
fromSortName str = Sort str

-- | Convert a Typ to its corresponding sort name string (inverse of toSortName)
toSortName :: Typ -> String
toSortName (Sort str) = str
toSortName IntType = "Int"
toSortName StrType = "String"
toSortName AnyType = "Any"
toSortName (SetOf typ) = "Set(" ++ toSortName typ ++ ")"
toSortName VoidType = "Void"


