-- | Module that is loaded by code generted in the 'Language.AST.CodeGen' module.
module Language.AST.CodeGen.Prelude(
    HaskellHatch(..),
    HatchError(..),
    module Language.AST,
    module Language.Types,
    module Language.Range
  ) where

import Language.AST
import Language.Range
import Language.Types
import Data.Map (Map)
import qualified Data.Map as Map

-- | Errors that can be produced by embedded Haskell expressions
data HatchError = InvalidTypePassed Typ Typ -- ^ invalid type: expected, actual
                | UserError String -- ^ any error that gets thrown by the Haskell expression

-- | Represents an embedded Haskell expression
data HaskellHatch = HaskellHatch {
    -- List of free variables mentioned in the Haskell expression,
    -- used to construct a mapping for the execute function containing these variables and their mapping to pure terms.
    freeVars :: [String],
    -- | The execution function, gives a mapping of free variables to their ground terms, and returns either a failure (indicated by Left) or a pure term.
    --
    -- Failures are encoded using the HatchError data type.
    execute :: Map String PureTerm -> Either HatchError PureTerm
  }

