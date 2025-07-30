{-# LANGUAGE RecordWildCards #-}
module Language.Lexer where
import Prelude hiding (lex)
import Data.Char (isLetter)
import GHC.Unicode

data Ctx = Ctx { line :: Int, col :: Int }
emptyCtx = Ctx 1 1


incc :: Ctx -> Ctx
incc ctx@Ctx { .. } = ctx { col = col + 1 }


incck :: Int -> Ctx -> Ctx
incck k ctx@Ctx { .. } = ctx { col = col + k }

incl :: Ctx -> Ctx
incl ctx@Ctx { .. } = ctx { line = line + 1, col = 1 }

inc :: Char -> Ctx -> Ctx
inc ' ' = incc
inc '\t' = incc
inc '\n' = incl

inck :: Int -> Char -> Ctx -> Ctx
inck k ' ' = incck k
inck k '\t' = incck k
inck k '\n' = incl


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

data TokenWithCtx = TokenWithCtx { tokenToken :: Token, tokenCtx :: Ctx }
instance Show TokenWithCtx where
  show (TokenWithCtx token Ctx {..}) = show token ++ "@" ++ show line ++ ":" ++ show col


-- Whitespace characters
isWhitespace :: Char -> Bool
isWhitespace c =
  c == ' ' || c == '\n' || c == '\t'

isBracket :: Char -> Bool
isBracket c =
  c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']'

-- Lex characters starting with a '='
lexEq :: Ctx -> String -> [TokenWithCtx]
lexEq ctx ('>':rest) = TokenWithCtx Implies ctx : lex (incc ctx) rest
lexEq ctx (c:rest)
  | isWhitespace c = TokenWithCtx Equal ctx : lex (inc c ctx) rest
  | otherwise = error $ "unexpected token " ++ show c
lexEq ctx [] = error $ "unexpected end of file at " ++ show (line ctx) ++ ":" ++ show (col ctx)

-- | Lex an identifier and then continue
lexIdentifier :: Ctx -> String -> String -> [TokenWithCtx]
lexIdentifier ctx acc (c:rest)
  | isWhitespace c || not (isAlphaNum c) = TokenWithCtx (Ident (reverse acc)) ctx : lex ctx (c:rest)
  | otherwise = lexIdentifier (incc ctx) (c:acc) rest

-- | Scan until another quote is found, no escaping, multiline supported
lexQuoted :: String -> Ctx -> String -> [TokenWithCtx]
lexQuoted acc ctx (c:rest)
  | c == '\n' = lexQuoted acc (incl ctx) rest
  | c /= '"'  = lexQuoted (c:acc) (incc ctx) rest
  | otherwise = TokenWithCtx (Quo $ reverse acc) ctx : lex (incc ctx) rest
lexQuoted acc ctx [] = error $ "unexpected end of file at " ++ show (line ctx) ++ ":" ++ show (col ctx)

lex :: Ctx -> String -> [TokenWithCtx]
-- ignore whitespace
lex ctx (' ':rest)  = lex (incc ctx) rest
lex ctx ('\n':rest) = lex (incl ctx) rest
lex ctx ('\t':rest)  = lex (incc ctx) rest
-- ignore comments until end of line
lex ctx ('%':rest) = ignore ctx rest
  where ignore ctx ('\n':rest') = lex (incl ctx) rest'
        ignore ctx (c:rest') = ignore (incc ctx) rest' 
-- brackets
lex ctx ('(':rest)  = TokenWithCtx Lpar ctx : lex (incc ctx) rest
lex ctx (')':rest)  = TokenWithCtx Rpar ctx : lex (incc ctx) rest
lex ctx ('{':rest)  = TokenWithCtx LCBa ctx : lex (incc ctx) rest
lex ctx ('}':rest)  = TokenWithCtx RCBa ctx : lex (incc ctx) rest
lex ctx ('[':rest)  = TokenWithCtx LBra ctx : lex (incc ctx) rest
lex ctx (']':rest)  = TokenWithCtx RBra ctx : lex (incc ctx) rest
lex ctx (',':rest)  = TokenWithCtx Comma ctx : lex (incc ctx) rest
lex ctx ('.':rest)  = TokenWithCtx Dot ctx : lex (incc ctx) rest
lex ctx ('|':rest)  = TokenWithCtx Bar ctx : lex (incc ctx) rest
-- special operators
lex ctx ('=':rest)     = lexEq (incc ctx) rest
lex ctx ('~':'>':rest) = TokenWithCtx LeadsTo ctx : lex (incck 2 ctx) rest
lex ctx (':':':':'=':rest) = TokenWithCtx IsDefinedAs ctx : lex (incck 4 ctx) rest
lex ctx (';':rest) = TokenWithCtx Sem ctx : lex (incc ctx) rest
lex ctx ('"':rest) = lexQuoted [] (incc ctx) rest
lex ctx ('r':'u':'l':'e':c1:c2:rest)
    | c1 == 's' && isWhitespace c2 = TokenWithCtx Rules ctx : lex (inck 6 c2 ctx) rest
    | isWhitespace c1 = TokenWithCtx Rule ctx : lex (inck 5 c1 ctx) (c2:rest)
    | otherwise = lexIdentifier (incck 4 ctx) (reverse "rule") (c1:c2:rest)
lex ctx (c:rest) = lexIdentifier ctx [c] rest
lex _ [] = []
-- lex ctx (c:rest) = error $ "unexpected character " ++ show c ++ " at " ++ show (line ctx) ++ ":" ++ show (col ctx)

