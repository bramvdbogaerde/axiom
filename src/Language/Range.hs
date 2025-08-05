module Language.Range(RangeOf(..), Position(..), Range(..), dummyRange) where

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

-- | A dummy range for cases where range information is not available
dummyRange :: Range
dummyRange = Range (Position 0 0 Nothing) (Position 0 0 Nothing)

