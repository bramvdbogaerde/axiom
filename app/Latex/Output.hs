module Latex.Output where

import Data.Map (Map)
import qualified Data.Map as Map

-- | The type of LateX output, indicating whether the output is a syntax declaration,
-- rule declarations or rewrite declarations.
data LatexType = Syntax | Rules | Rewrites
  deriving (Show, Eq, Ord)

-- | Represents a LateX output consisting of mappings from block names
-- to their LateX math.
newtype LatexOutput = LatexOutput (Map (String, LatexType) String)
  deriving (Show)

-- | Add a rendered block to the output
addBlock :: String -> LatexType -> String -> LatexOutput -> LatexOutput
addBlock name latexType rendered (LatexOutput m) =
  LatexOutput $ Map.insert (name, latexType) rendered m

-- | Get the blocks from the output
getBlocks :: LatexOutput -> Map (String, LatexType) String
getBlocks (LatexOutput m) = m
