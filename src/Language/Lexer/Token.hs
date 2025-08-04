module Language.Lexer.Token where

import Language.Range

data Token = IsDefinedAs -- ^ '::='
           | LeadsTo     -- ^ '~>'
           | Implies     -- ^ '=>'
           | Equal       -- ^ '='
           | Rules       -- ^ 'rules'
           | Rule        -- ^ 'rule'
           | Lpar        -- ^ '('
           | Rpar        -- ^ ')'
           | LCBa        -- ^ '{'
           | RCBa        -- ^ '}'
           | LBra        -- ^ '['
           | RBra        -- ^ ']'
           | Comment     -- ^ '%'
           | Comma       -- ^ ',' 
           | Dot         -- ^ '.'
           | Sem         -- ^ ';'
           | Bar         -- ^ '|'
           | Ident String
           | Quo String
           deriving (Ord, Eq, Show)

data TokenWithRange = TokenWithRange {
                      tokenToken :: Token,
                      tokenRange :: Range
                    } deriving (Ord, Eq, Show)
