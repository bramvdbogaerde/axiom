module Language.Types(Typ(..), toSortName) where

import Language.Haskell.TH.Syntax

-- | The different types that can be taken by a term
data Typ = Sort String -- ^ a user-defined (or system) sort
         | IntType     -- ^ values are strings 
         | StrType     -- ^ values are integers
         | AnyType
        deriving (Ord, Eq, Show)        


-- | Template haskell integration
instance Lift Typ where 
  liftTyped (Sort str) = [|| Sort $$(liftTyped str) ||]
  liftTyped IntType = [|| IntType ||]
  liftTyped StrType = [|| StrType ||]

-- | Convert a Typ to its corresponding sort name string
toSortName :: Typ -> String
toSortName (Sort str) = str
toSortName IntType = "Int"
toSortName StrType = "String"
toSortName AnyType = "Any"
