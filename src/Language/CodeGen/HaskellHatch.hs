{-# LANGUAGE RankNTypes #-}
-- | Module for Haskell expression execution and related functionality
module Language.CodeGen.HaskellHatch(
    HaskellHatch(..),
    HatchError(..)
  ) where

import Language.AST
import Language.Types
import Data.Map (Map)
import Data.Data

-- | Errors that can be produced by embedded Haskell expressions
data HatchError = InvalidTypePassed Typ Typ -- ^ invalid type: expected, actual
                | UserError String -- ^ any error that gets thrown by the Haskell expression
                deriving (Show)

-- | Represents an embedded Haskell expression
data HaskellHatch p = HaskellHatch {
    -- | Original haskell expression
    original :: String,
    -- | List of free variables mentioned in the Haskell expression,
    -- used to construct a mapping for the execute function containing these variables and their mapping to pure terms.
    freeVars :: [String],
    -- | Mapping from renamed variables to their original names, initially empty, but updated
    -- by the renamer when renaming a term containing a Haskell expression.
    renamedVariables :: Map String String,
    -- | The execution function, gives a mapping of free variables to their ground terms, and returns either a failure (indicated by Left) or a pure term.
    --
    -- Failures are encoded using the HatchError data type.
    execute :: AnnotateType p => Proxy p -> Map String (PureTerm' p) -> Either HatchError (PureTerm' p)
  }

instance Eq (HaskellHatch p) where
    (==) a b = (==) (original a) (original b)
instance Ord (HaskellHatch p) where
    compare a b = compare (original a) (original b)
instance Show (HaskellHatch p) where
    show = show . original
