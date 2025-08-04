module Language.Range(RangeOf(..), Position(..), Range(..)) where

-- | Compute the range of arbitrary program elements
class RangeOf a where
  rangeOf :: a -> Range

-- | A position in a source file
data Position = Position {
                  positionLine   :: Int,
                  positionColumn :: Int,
                  filename :: Maybe String
              } deriving (Ord, Show, Eq)
                          

-- | Represents the range of the AST node  
data Range = Range { rangeStart :: Position,
                     rangeEnd :: Position }
           deriving (Ord, Eq, Show)

