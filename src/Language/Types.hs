module Language.Types(Typ(..)) where

import Language.Haskell.TH.Syntax

-- | The different types that can be taken by a term
data Typ = Sort String -- ^ a user-defined (or system) sort
         | IntType     -- ^ values are strings 
         | StrType     -- ^ values are integers
        deriving (Ord, Eq, Show)        


-- | Template haskell integration
instance Lift Typ where 
  liftTyped (Sort str) = [|| Sort $$(liftTyped str) ||]
  liftTyped IntType = [|| IntType ||]
  liftTyped StrType = [|| StrType ||]
