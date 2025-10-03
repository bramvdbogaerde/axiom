module Language.Lexer.Token where

import Language.Range

data Token = EOF
           | IsDefinedAs -- ^ '::='
           | LeadsTo     -- ^ '~>'
           | Implies     -- ^ '=>'
           | Equal       -- ^ '='
           | NotEqual    -- ^ '/='
           | BigStep     -- ^ '⇓'
           | Rules       -- ^ 'rules'
           | Rule        -- ^ 'rule'
           | Lpar        -- ^ '('
           | Rpar        -- ^ ')'
           | LCBa        -- ^ '{'
           | RCBa        -- ^ '}'
           | LBra        -- ^ '['
           | RBra        -- ^ ']'
           | Boo { tokBool :: Bool }      -- ^ 'true' | 'false'
           | Comment { tokVal :: String } -- ^ '%' followed by comment text
           | Comma       -- ^ ',' 
           | Dot         -- ^ '.'
           | Sem         -- ^ ';'
           | Bar         -- ^ '|'
           | Ident { tokVal :: String }
           | Quo { tokVal :: String }
           | IntLit { tokInt :: Int  }         -- ^ integer literal
           | Hask { tokVal ::  String }        -- ^ multiple lines of Haskell declarations
           | HaskPost { tokVal :: String }     -- ^ same as Hask but appends the contents to the end of the generated code
           | HaskellExpr { tokVal :: String }  -- ^ '{ HASKELL_EXP }'
           | MapsTo
           deriving (Ord, Eq, Show)

data TokenWithRange = TokenWithRange {
                      tokenToken :: Token,
                      tokenRange :: Range
                    } deriving (Ord, Eq)

instance RangeOf TokenWithRange where
  rangeOf = tokenRange

instance Show TokenWithRange where
   show = show . tokenToken
           

-- | Construct a range from two elements that have a range, using the first element
-- as the start of the new range, and the second element as its end.
mkRange :: (RangeOf a, RangeOf b) => a -> b -> Range
mkRange start end = Range (rangeStart (rangeOf start)) (rangeEnd (rangeOf end))
