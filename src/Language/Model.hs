{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
module Language.Model where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State
import Control.Lens hiding (Context)
import Language.AST
import Control.Monad.Except

-----------------------------------------
-- Model datatypes
-----------------------------------------


-- | Names to refer to sorts
newtype SortName = SortName String deriving (Ord, Eq, Show)

-- | Variables
type Var = String

-- | A data constructor is either an atom or a tag paired with a list of sorts
data DataCtor = DataAtom String | DataTagged String [SortName]
              deriving (Ord, Eq, Show)

-- | A sort consists of a set of data constructors, a name of the sort and
-- the variables associated with that sort.
data Sort = Sort String (Set Var) (Set DataCtor)
          deriving (Ord, Eq, Show)

-----------------------------------------
-- Errors
-----------------------------------------

data ModelError = DuplicateVariable String
                deriving (Ord, Eq, Show)

-----------------------------------------
-- Context typing
-----------------------------------------

-- | A typing context used when parsing the syntax rules
data Context = Context { _atomToSorts   :: Map String Sort, -- ^ mapping from data atoms (in variables or datactors) to their sorts
                         _functorToCtor :: Map String DataCtor,
                         _sorts         :: Map String Sort  -- ^ a mapping from a sort name to their sort data structure
                       } deriving (Ord, Eq, Show)

$(makeLenses ''Context)

-- | Monad for modifying and tracking the typing context
type MonadTy m = (MonadState Context m, MonadError ModelError m)
        
-- | Associate a single 

-- | Add the contents of a single syntax rule Context to the typing context 
addSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
addSyntaxRule (SyntaxDecl vars tpy ctors) = do
  -- associate a variable with the sort, ensuring that no other
  -- variables are already associated with it
  mapM (assocAtomToSort tpy) vars 

