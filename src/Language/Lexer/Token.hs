module Language.Lexer.Token where

import Language.Range

data Token = IsDefinedAs -- ^ '::='
           | LeadsTo     -- ^ '~>'
           | Implies     -- ^ '=>'
           | Equal       -- ^ '='
           | NotEqual    -- ^ '/='
           | Rules       -- ^ 'rules'
           | Rule        -- ^ 'rule'
           | Lpar        -- ^ '('
           | Rpar        -- ^ ')'
           | LCBa        -- ^ '{'
           | RCBa        -- ^ '}'
           | LBra        -- ^ '['
           | RBra        -- ^ ']'
           | Comment String -- ^ '%' followed by comment text
           | Comma       -- ^ ',' 
           | Dot         -- ^ '.'
           | Sem         -- ^ ';'
           | Bar         -- ^ '|'
           | Ident String
           | Quo String
           | IntLit Int  -- ^ integer literal
           | HaskellExpr String -- ^ '{ HASKELL_EXP }'
           deriving (Ord, Eq, Show)

data TokenWithRange = TokenWithRange {
                      tokenToken :: Token,
                      tokenRange :: Range
                    } deriving (Ord, Eq)

instance RangeOf TokenWithRange where
  rangeOf = tokenRange

instance Show TokenWithRange where
   show = show . tokenToken
           
