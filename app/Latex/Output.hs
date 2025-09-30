module Latex.Output where

import Data.Map (Map)

-- | The type of LateX output, indicating whether the output is a syntax declaration,
-- rule declarations or rewrite declarations.
data LatexType = Syntax | Rules | Rewrites
               deriving (Show)

-- | Represents a LateX output consisting of mappings from block names
-- to their LateX math.
newtype LatexOutput = LatexOutput ( Map (String, LatexType) String )
                 deriving (Show)
