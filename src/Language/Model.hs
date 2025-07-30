{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.Model where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.State
import Language.AST

type Var = String

-- | A data constructor is either an atom or a tag paired with a list of sorts
data DataCtor = DataAtom String | DataTagged String [Sort]
              deriving (Ord, Eq, Show)

-- | A sort consists of a set of data constructors, a name of the sort and
-- the variables associated with that sort.
data Sort = Sort String (Set Var) (Set DataCtor)
          deriving (Ord, Eq, Show)

-- | A typing context used when parsing the syntax rules
data Context = Context { atomToSorts :: Map String Sort, -- ^ mapping from data atoms (in variables or datactors) to their sorts
                         functorToCtor :: Map String DataCtor,
                         sorts :: Map String Sort        -- ^ a mapping from a sort name to their sort data structure
                       } deriving (Ord, Eq, Show)


-- | Monad for modifying and tracking the typing context
type MonadTy m = (MonadState Context m)
        
-- | Add the contents of a single syntax rule Contextto the typing context 
addSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
addSyntaxRule = undefined

