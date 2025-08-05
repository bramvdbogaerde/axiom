{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Solver where

import Control.Lens
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Language.AST
import Control.Monad.State

------------------------------------------------------------
-- Core data structures
------------------------------------------------------------

data EngineCtx = EngineCtx {
    -- | Mapping from functor names to their appropriate rule, used in backwards reasoning
      _conclusionFunctors :: Map String (Set RuleDecl)
    -- | Unique variables counter
    , _numUniqueVariables :: Int
  } deriving (Ord, Eq, Show)


-- | Create an empty solver engine context
emptyEngineCtx :: EngineCtx
emptyEngineCtx = EngineCtx Map.empty 0


$(makeLenses ''EngineCtx)

-- | Associate the given functor with the given rule, indicating that it can be found in the conclusion of that rule.
addConclusionFunctor :: String -> RuleDecl -> EngineCtx -> EngineCtx
addConclusionFunctor nam decl =
   over conclusionFunctors (Map.insertWith Set.union nam (Set.singleton decl))

-- | Construct an initial context from the rules defined the program
fromRules :: [RuleDecl] -> EngineCtx
fromRules = foldr visit emptyEngineCtx
  where visit rule@(RuleDecl _ precedent consequent _) =
          flip (foldr (`addConclusionFunctor` rule)) (foldMap functorNames consequent)

------------------------------------------------------------
-- Monad context
------------------------------------------------------------



------------------------------------------------------------
-- Unification
------------------------------------------------------------



------------------------------------------------------------
-- Solver
------------------------------------------------------------
