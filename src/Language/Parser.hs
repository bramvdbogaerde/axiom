{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PartialTypeSignatures #-}
#endif
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Parser(parseProgram, parseTerm, parseRule, parseGoal, Error(..)) where

import Prelude hiding (lex)

import Language.Lexer hiding (HaskellExpr)
import Language.Lexer.Token (TokenWithRange(..), Token(..), mkRange)
import qualified Language.Lexer.Token as Token
import Language.AST hiding (Comment)
import qualified Language.AST as AST
import Language.Range
import Language.Types
import Data.Functor.Identity
import Data.Maybe
import Data.Bifunctor
import Data.Either (partitionEithers)
import qualified Data.Set as Set
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.0.2

data HappyAbsSyn 
        = HappyTerminal (TokenWithRange)
        | HappyErrorToken Prelude.Int
        | HappyAbsSyn7 (Program)
        | HappyAbsSyn8 ([Decl])
        | HappyAbsSyn9 (Decl)
        | HappyAbsSyn12 ([SyntaxDecl])
        | HappyAbsSyn13 (SyntaxDecl)
        | HappyAbsSyn14 (TypeCon)
        | HappyAbsSyn15 ([TypeCon])
        | HappyAbsSyn16 ([String])
        | HappyAbsSyn17 (Maybe [PureTerm])
        | HappyAbsSyn18 ([PureTerm])
        | HappyAbsSyn20 ([RuleDecl])
        | HappyAbsSyn21 (RuleDecl)
        | HappyAbsSyn31 (PureTerm)

happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\xd2\x00\x44\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\xd2\x00\x44\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\x00\x44\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x20\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x10\x00\x40\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x41\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x40\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x02\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x01\x00\x00\x00\x00\x00\x08\x05\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseProgramTokens","%start_parseTermTokens","%start_parseRuleTokens","%start_parseGoalTokens","Program","ProgramElements","ProgramElement","Declaration","SyntaxBlock","SyntaxDecls","SyntaxDecl","TypeRef","TypeRefs","IdentList","MaybeProductions","Productions","RulesBlock","Rules","Rule","Goals","TermList","Terms","TransitionDecl","RewriteRule","ImportDecl","TermArgs","TermArgList","HaskellBlock","Term","BigStepExpr","Elements","BasicTerm","Goal","'::='","'~>'","'=>'","'='","'/='","'\8659'","'rules'","'rule'","BOOL","'syntax'","'in'","'transition'","'import'","'('","')'","'{'","'}'","'['","']'","','","'.'","';'","'|'","IDENT","STRING","INTLIT","HASKEXPR","HASKBLOCK","%eof"]
        bit_start = st               Prelude.* 64
        bit_end   = (st Prelude.+ 1) Prelude.* 64
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..63]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x23\x00\x00\x00\x34\x00\x00\x00\x03\x00\x00\x00\x38\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\xfd\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x24\x00\x00\x00\x2a\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x37\x00\x00\x00\x00\x00\x00\x00\xc1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x5d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2c\x00\x00\x00\x2e\x00\x00\x00\xff\xff\xff\xff\x4b\x00\x00\x00\x3e\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x5b\x00\x00\x00\x48\x00\x00\x00\x0a\x00\x00\x00\x83\x00\x00\x00\x62\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x00\x00\x74\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x96\x00\x00\x00\xae\x00\x00\x00\xa0\x00\x00\x00\xc7\x00\x00\x00\xab\x00\x00\x00\xbe\x00\x00\x00\x49\x00\x00\x00\x00\x00\x00\x00\xb5\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\xd3\x00\x00\x00\xd3\x00\x00\x00\xda\x00\x00\x00\x00\x00\x00\x00\x48\x00\x00\x00\xd7\x00\x00\x00\x4c\x00\x00\x00\xd7\x00\x00\x00\xd2\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x00\x00\xcd\x00\x00\x00\x00\x00\x00\x00\xd6\x00\x00\x00\x5c\x00\x00\x00\xe1\x00\x00\x00\x00\x00\x00\x00\x81\x00\x00\x00\xce\x00\x00\x00\x81\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\xc3\x00\x00\x00\x00\x00\x00\x00\xdf\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00\xe7\x00\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\xd8\x00\x00\x00\xd9\x00\x00\x00\xdc\x00\x00\x00\x5c\x00\x00\x00\x00\x00\x00\x00\x5c\x00\x00\x00\x60\x00\x00\x00\xeb\x00\x00\x00\xdd\x00\x00\x00\x00\x00\x00\x00\xe2\x00\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\xe0\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x02\x00\x00\x00\x46\x00\x00\x00\xe4\x00\x00\x00\x1a\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x00\x00\x00\x9f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x00\x00\x00\xaf\x00\x00\x00\x00\x00\x00\x00\xb3\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb7\x00\x00\x00\xbb\x00\x00\x00\x89\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa4\x00\x00\x00\xfc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\xec\x00\x00\x00\xed\x00\x00\x00\x00\x00\x00\x00\x64\x00\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x65\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xee\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbf\x00\x00\x00\x00\x00\x00\x00\xa7\x00\x00\x00\x7a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x6a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xfa\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\xff\xff\xfa\xff\xff\xff\x00\x00\x00\x00\xf6\xff\xff\xff\xf5\xff\xff\xff\xf4\xff\xff\xff\xf3\xff\xff\xff\xf2\xff\xff\xff\xf7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\xff\xff\xc0\xff\xff\xff\xcb\xff\xff\xff\xcd\xff\xff\xff\x00\x00\x00\x00\xc1\xff\xff\xff\xc8\xff\xff\xff\xc8\xff\xff\xff\xc5\xff\xff\xff\xc2\xff\xff\xff\xc3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc5\xff\xff\xff\x00\x00\x00\x00\xd2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd2\xff\xff\xff\xd3\xff\xff\xff\x00\x00\x00\x00\xf0\xff\xff\xff\xdf\xff\xff\xff\xf8\xff\xff\xff\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe4\xff\xff\xff\x00\x00\x00\x00\xec\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xd1\xff\xff\xff\xd0\xff\xff\xff\xbd\xff\xff\xff\xbe\xff\xff\xff\x00\x00\x00\x00\xca\xff\xff\xff\xc8\xff\xff\xff\xbf\xff\xff\xff\xdc\xff\xff\xff\xcc\xff\xff\xff\x00\x00\x00\x00\xc4\xff\xff\xff\x00\x00\x00\x00\xdb\xff\xff\xff\xc6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd5\xff\xff\xff\xe9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xee\xff\xff\xff\x00\x00\x00\x00\xf0\xff\xff\xff\xf1\xff\xff\xff\xdf\xff\xff\xff\xe0\xff\xff\xff\xde\xff\xff\xff\xef\xff\xff\xff\xe3\xff\xff\xff\xe2\xff\xff\xff\xe4\xff\xff\xff\xec\xff\xff\xff\xe5\xff\xff\xff\xe6\xff\xff\xff\xe8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\xff\xff\xc8\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xda\xff\xff\xff\x00\x00\x00\x00\xd4\xff\xff\xff\xeb\xff\xff\xff\xe9\xff\xff\xff\xed\xff\xff\xff\x00\x00\x00\x00\xe1\xff\xff\xff\xe7\xff\xff\xff\xc9\xff\xff\xff\xd9\xff\xff\xff\x00\x00\x00\x00\xd8\xff\xff\xff\xd7\xff\xff\xff\x00\x00\x00\x00\xdd\xff\xff\xff\xd6\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x08\x00\x00\x00\x02\x00\x00\x00\x02\x00\x00\x00\x0c\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x16\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x17\x00\x00\x00\x0c\x00\x00\x00\x16\x00\x00\x00\x1d\x00\x00\x00\x17\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x17\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00\x03\x00\x00\x00\x04\x00\x00\x00\x07\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0a\x00\x00\x00\x1b\x00\x00\x00\x0c\x00\x00\x00\x0d\x00\x00\x00\x0c\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x10\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x10\x00\x00\x00\x18\x00\x00\x00\x17\x00\x00\x00\x09\x00\x00\x00\x18\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x09\x00\x00\x00\x0e\x00\x00\x00\x11\x00\x00\x00\x10\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x19\x00\x00\x00\x10\x00\x00\x00\x1d\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x09\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x0b\x00\x00\x00\x09\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x1d\x00\x00\x00\x10\x00\x00\x00\x14\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x09\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x0b\x00\x00\x00\x09\x00\x00\x00\x0e\x00\x00\x00\x0e\x00\x00\x00\x10\x00\x00\x00\x12\x00\x00\x00\x0e\x00\x00\x00\x0b\x00\x00\x00\x10\x00\x00\x00\x0f\x00\x00\x00\x0d\x00\x00\x00\x0e\x00\x00\x00\x18\x00\x00\x00\x0b\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x09\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0e\x00\x00\x00\x1b\x00\x00\x00\x10\x00\x00\x00\x08\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0f\x00\x00\x00\x1b\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x18\x00\x00\x00\x0f\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1b\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x11\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1b\x00\x00\x00\x1b\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x02\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x11\x00\x00\x00\x1b\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x11\x00\x00\x00\x09\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x02\x00\x00\x00\x16\x00\x00\x00\x04\x00\x00\x00\x05\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0b\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x18\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x0f\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x19\x00\x00\x00\x02\x00\x00\x00\x1b\x00\x00\x00\x18\x00\x00\x00\x18\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x1b\x00\x00\x00\x06\x00\x00\x00\x0f\x00\x00\x00\x13\x00\x00\x00\x16\x00\x00\x00\x0e\x00\x00\x00\x04\x00\x00\x00\x18\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\x0f\x00\x00\x00\x14\x00\x00\x00\x14\x00\x00\x00\x03\x00\x00\x00\x12\x00\x00\x00\x0a\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x13\x00\x00\x00\x07\x00\x00\x00\x09\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x25\x00\x00\x00\x22\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x25\x00\x00\x00\x25\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x20\x00\x00\x00\x25\x00\x00\x00\x25\x00\x00\x00\x09\x00\x00\x00\x05\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x32\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\x0d\x00\x00\x00\x09\x00\x00\x00\x78\x00\x00\x00\xff\xff\xff\xff\x70\x00\x00\x00\x44\x00\x00\x00\x4e\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x74\x00\x00\x00\x75\x00\x00\x00\x0d\x00\x00\x00\x32\x00\x00\x00\x06\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x0f\x00\x00\x00\x76\x00\x00\x00\x15\x00\x00\x00\x10\x00\x00\x00\x16\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x09\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x31\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x30\x00\x00\x00\x13\x00\x00\x00\x0d\x00\x00\x00\x19\x00\x00\x00\x2f\x00\x00\x00\x14\x00\x00\x00\x2e\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x79\x00\x00\x00\x1b\x00\x00\x00\x2d\x00\x00\x00\x1a\x00\x00\x00\x26\x00\x00\x00\x1b\x00\x00\x00\xff\xff\xff\xff\x76\x00\x00\x00\x15\x00\x00\x00\x22\x00\x00\x00\x16\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\xe6\xff\xff\xff\x19\x00\x00\x00\x1a\x00\x00\x00\x51\x00\x00\x00\x1b\x00\x00\x00\x24\x00\x00\x00\x1a\x00\x00\x00\xff\xff\xff\xff\x1b\x00\x00\x00\x52\x00\x00\x00\x20\x00\x00\x00\x15\x00\x00\x00\x22\x00\x00\x00\x16\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x27\x00\x00\x00\x19\x00\x00\x00\x1a\x00\x00\x00\x24\x00\x00\x00\x1b\x00\x00\x00\x46\x00\x00\x00\x1a\x00\x00\x00\x5b\x00\x00\x00\x1b\x00\x00\x00\x42\x00\x00\x00\x59\x00\x00\x00\x34\x00\x00\x00\x22\x00\x00\x00\x70\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1c\x00\x00\x00\x19\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x5c\x00\x00\x00\x15\x00\x00\x00\x1a\x00\x00\x00\x16\x00\x00\x00\x1b\x00\x00\x00\x20\x00\x00\x00\x5c\x00\x00\x00\x15\x00\x00\x00\x49\x00\x00\x00\x16\x00\x00\x00\x61\x00\x00\x00\x71\x00\x00\x00\x22\x00\x00\x00\x69\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x3a\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x3b\x00\x00\x00\x16\x00\x00\x00\x4a\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x43\x00\x00\x00\x16\x00\x00\x00\x4a\x00\x00\x00\x47\x00\x00\x00\x3d\x00\x00\x00\x5f\x00\x00\x00\x3e\x00\x00\x00\x15\x00\x00\x00\x3b\x00\x00\x00\x16\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x3c\x00\x00\x00\x3e\x00\x00\x00\x15\x00\x00\x00\x64\x00\x00\x00\x16\x00\x00\x00\x3e\x00\x00\x00\x15\x00\x00\x00\x59\x00\x00\x00\x16\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x55\x00\x00\x00\x38\x00\x00\x00\x5a\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x57\x00\x00\x00\x38\x00\x00\x00\x27\x00\x00\x00\x15\x00\x00\x00\x29\x00\x00\x00\x16\x00\x00\x00\x27\x00\x00\x00\x15\x00\x00\x00\x28\x00\x00\x00\x16\x00\x00\x00\x27\x00\x00\x00\x15\x00\x00\x00\x4b\x00\x00\x00\x16\x00\x00\x00\x27\x00\x00\x00\x15\x00\x00\x00\x6a\x00\x00\x00\x16\x00\x00\x00\x25\x00\x00\x00\x58\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x46\x00\x00\x00\x15\x00\x00\x00\x53\x00\x00\x00\x16\x00\x00\x00\x44\x00\x00\x00\x15\x00\x00\x00\x50\x00\x00\x00\x16\x00\x00\x00\x40\x00\x00\x00\x15\x00\x00\x00\x4f\x00\x00\x00\x16\x00\x00\x00\x3f\x00\x00\x00\x15\x00\x00\x00\x25\x00\x00\x00\x16\x00\x00\x00\x6b\x00\x00\x00\x15\x00\x00\x00\x25\x00\x00\x00\x16\x00\x00\x00\x3a\x00\x00\x00\x5f\x00\x00\x00\x56\x00\x00\x00\x3b\x00\x00\x00\x3b\x00\x00\x00\x4d\x00\x00\x00\x49\x00\x00\x00\x68\x00\x00\x00\x67\x00\x00\x00\x66\x00\x00\x00\x64\x00\x00\x00\x61\x00\x00\x00\x20\x00\x00\x00\x55\x00\x00\x00\x51\x00\x00\x00\x25\x00\x00\x00\x6d\x00\x00\x00\x52\x00\x00\x00\x6e\x00\x00\x00\x69\x00\x00\x00\x74\x00\x00\x00\x53\x00\x00\x00\x73\x00\x00\x00\x1e\x00\x00\x00\x79\x00\x00\x00\x5d\x00\x00\x00\x5f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (4, 66) [
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66)
        ]

happy_n_terms = 30 :: Prelude.Int
happy_n_nonterms = 29 :: Prelude.Int

happyReduce_4 = happySpecReduce_1  0# happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn7
                 (Program happy_var_1 []
        )
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  1# happyReduction_5
happyReduction_5  =  HappyAbsSyn8
                 ([]
        )

happyReduce_6 = happySpecReduce_2  1# happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_2)
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn8
                 (happy_var_1 : happy_var_2
        )
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  2# happyReduction_7
happyReduction_7 _
        (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  2# happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  3# happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  3# happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  3# happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  3# happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  3# happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn9
                 (happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4# 4# happyReduction_14
happyReduction_14 ((HappyTerminal happy_var_4) `HappyStk`
        (HappyAbsSyn12  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Syntax happy_var_3 (mkRange happy_var_1 happy_var_4)
        ) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_0  5# happyReduction_15
happyReduction_15  =  HappyAbsSyn12
                 ([]
        )

happyReduce_16 = happySpecReduce_3  5# happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
        _
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn12
                 (happy_var_1 : happy_var_3
        )
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  6# happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_2)
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn13
                 (SyntaxDecl [] happy_var_1 (fromMaybe [] happy_var_2) (rangeOf happy_var_1)
        )
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4# 6# happyReduction_18
happyReduction_18 ((HappyAbsSyn17  happy_var_4) `HappyStk`
        (HappyAbsSyn14  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn16  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn13
                 (SyntaxDecl happy_var_1 happy_var_3 (fromMaybe [] happy_var_4) (rangeOf happy_var_3)
        ) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  7# happyReduction_19
happyReduction_19 (HappyTerminal happy_var_1)
         =  HappyAbsSyn14
                 (TypeApp (TypeTyp (getIdent happy_var_1) (rangeOf happy_var_1)) [] (rangeOf happy_var_1)
        )
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 4# 7# happyReduction_20
happyReduction_20 (_ `HappyStk`
        (HappyAbsSyn15  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn14
                 (TypeApp (TypeTyp (getIdent happy_var_1) (rangeOf happy_var_1)) happy_var_3 (rangeOf happy_var_1)
        ) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  7# happyReduction_21
happyReduction_21 (HappyTerminal happy_var_1)
         =  HappyAbsSyn14
                 (TypeHas (getHaskellExpr happy_var_1) (rangeOf happy_var_1)
        )
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  8# happyReduction_22
happyReduction_22  =  HappyAbsSyn15
                 ([]
        )

happyReduce_23 = happySpecReduce_1  8# happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn15
                 ([happy_var_1]
        )
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  8# happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_3)
        _
        (HappyAbsSyn14  happy_var_1)
         =  HappyAbsSyn15
                 (happy_var_1 : happy_var_3
        )
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9# happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
         =  HappyAbsSyn16
                 ([getIdent happy_var_1]
        )
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  9# happyReduction_26
happyReduction_26 (HappyAbsSyn16  happy_var_3)
        _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn16
                 (getIdent happy_var_1 : happy_var_3
        )
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  10# happyReduction_27
happyReduction_27  =  HappyAbsSyn17
                 (Nothing
        )

happyReduce_28 = happySpecReduce_2  10# happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_2)
        _
         =  HappyAbsSyn17
                 (Just happy_var_2
        )
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  11# happyReduction_29
happyReduction_29 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  11# happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 : happy_var_3
        )
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happyReduce 4# 12# happyReduction_31
happyReduction_31 ((HappyTerminal happy_var_4) `HappyStk`
        (HappyAbsSyn20  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (RulesDecl happy_var_3 (mkRange happy_var_1 happy_var_4)
        ) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_0  13# happyReduction_32
happyReduction_32  =  HappyAbsSyn20
                 ([]
        )

happyReduce_33 = happySpecReduce_3  13# happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
        _
        (HappyAbsSyn21  happy_var_1)
         =  HappyAbsSyn20
                 (happy_var_1 : happy_var_3
        )
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 9# 14# happyReduction_34
happyReduction_34 ((HappyTerminal happy_var_9) `HappyStk`
        (HappyAbsSyn18  happy_var_8) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn21
                 (RuleDecl (getString happy_var_2) happy_var_4 happy_var_8 (mkRange happy_var_1 happy_var_9)
        ) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  15# happyReduction_35
happyReduction_35  =  HappyAbsSyn18
                 ([]
        )

happyReduce_36 = happySpecReduce_1  15# happyReduction_36
happyReduction_36 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  15# happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 : happy_var_3
        )
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  16# happyReduction_38
happyReduction_38  =  HappyAbsSyn18
                 ([]
        )

happyReduce_39 = happySpecReduce_1  16# happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  17# happyReduction_40
happyReduction_40 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  17# happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 : happy_var_3
        )
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyReduce 4# 18# happyReduction_42
happyReduction_42 ((HappyTerminal happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (TransitionDecl "~>" (Sort (getIdent happy_var_2), rangeOf happy_var_2) (Sort (getIdent happy_var_4), rangeOf happy_var_4) (mkRange happy_var_1 happy_var_4)
        ) `HappyStk` happyRest

happyReduce_43 = happyReduce 6# 19# happyReduction_43
happyReduction_43 ((HappyAbsSyn31  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn9
                 (Rewrite (RewriteDecl (getIdent happy_var_1) happy_var_3 happy_var_6 (mkRange happy_var_1 happy_var_6)) (mkRange happy_var_1 happy_var_6)
        ) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  20# happyReduction_44
happyReduction_44 (HappyTerminal happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (Import (getQuo happy_var_2) (mkRange happy_var_1 happy_var_2)
        )
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  21# happyReduction_45
happyReduction_45  =  HappyAbsSyn18
                 ([]
        )

happyReduce_46 = happySpecReduce_1  21# happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1
        )
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  22# happyReduction_47
happyReduction_47 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  22# happyReduction_48
happyReduction_48 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 : happy_var_3
        )
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  23# happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
         =  HappyAbsSyn9
                 (HaskellDecl (getHaskellBlock happy_var_1) (rangeOf happy_var_1)
        )
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  24# happyReduction_50
happyReduction_50 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (happy_var_1
        )
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  24# happyReduction_51
happyReduction_51 (HappyAbsSyn31  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (Transition "~>" happy_var_1 happy_var_3 () (mkRange happy_var_1 happy_var_3)
        )
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  24# happyReduction_52
happyReduction_52 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (happy_var_1
        )
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  24# happyReduction_53
happyReduction_53 (HappyTerminal happy_var_3)
        (HappyAbsSyn18  happy_var_2)
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (SetOfTerms (Set.fromList happy_var_2) () (mkRange happy_var_1 happy_var_3)
        )
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happyReduce 7# 25# happyReduction_54
happyReduction_54 ((HappyTerminal happy_var_7) `HappyStk`
        (HappyAbsSyn18  happy_var_6) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn31
                 (Functor "\8659" (happy_var_2 ++ happy_var_6) () (mkRange happy_var_1 happy_var_7)
        ) `HappyStk` happyRest

happyReduce_55 = happySpecReduce_0  26# happyReduction_55
happyReduction_55  =  HappyAbsSyn18
                 ([]
        )

happyReduce_56 = happySpecReduce_1  26# happyReduction_56
happyReduction_56 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 ([happy_var_1]
        )
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  26# happyReduction_57
happyReduction_57 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn18
                 (happy_var_1 : happy_var_3
        )
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  27# happyReduction_58
happyReduction_58 (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (Atom (Identity (getIdent happy_var_1)) () (rangeOf happy_var_1)
        )
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happyReduce 4# 27# happyReduction_59
happyReduction_59 ((HappyTerminal happy_var_4) `HappyStk`
        (HappyAbsSyn18  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyTerminal happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn31
                 (Functor (getIdent happy_var_1) happy_var_3 () (mkRange happy_var_1 happy_var_4)
        ) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_1  27# happyReduction_60
happyReduction_60 (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (AST.HaskellExpr (getHaskellExpr happy_var_1) () (rangeOf happy_var_1)
        )
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  27# happyReduction_61
happyReduction_61 (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (TermValue (IntValue (getIntLit happy_var_1)) () (rangeOf happy_var_1)
        )
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  27# happyReduction_62
happyReduction_62 (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (TermValue (BooValue (getBooLit happy_var_1))  () (rangeOf happy_var_1)
        )
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  28# happyReduction_63
happyReduction_63 (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (happy_var_1
        )
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  28# happyReduction_64
happyReduction_64 (HappyAbsSyn31  happy_var_3)
        _
        (HappyTerminal happy_var_1)
         =  HappyAbsSyn31
                 (IncludedIn (getIdent happy_var_1) happy_var_3 (mkRange happy_var_1 happy_var_3)
        )
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  28# happyReduction_65
happyReduction_65 (HappyAbsSyn31  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (Eqq happy_var_1 happy_var_3 () (mkRange happy_var_1 happy_var_3)
        )
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_3  28# happyReduction_66
happyReduction_66 (HappyAbsSyn31  happy_var_3)
        _
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn31
                 (Neq happy_var_1 happy_var_3 () (mkRange happy_var_1 happy_var_3)
        )
happyReduction_66 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
        happyDoAction 29# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
        let cont i = happyDoAction i tk action sts stk tks in
        case tk of {
        TokenWithRange IsDefinedAs _ -> cont 1#;
        TokenWithRange LeadsTo _ -> cont 2#;
        TokenWithRange Implies _ -> cont 3#;
        TokenWithRange Equal _ -> cont 4#;
        TokenWithRange NotEqual _ -> cont 5#;
        TokenWithRange BigStep _ -> cont 6#;
        TokenWithRange Rules _ -> cont 7#;
        TokenWithRange Rule _ -> cont 8#;
        TokenWithRange (Boo _) _ -> cont 9#;
        TokenWithRange (Ident "syntax") _ -> cont 10#;
        TokenWithRange (Ident "in") _ -> cont 11#;
        TokenWithRange (Ident "transition") _ -> cont 12#;
        TokenWithRange (Ident "import") _ -> cont 13#;
        TokenWithRange Lpar _ -> cont 14#;
        TokenWithRange Rpar _ -> cont 15#;
        TokenWithRange LCBa _ -> cont 16#;
        TokenWithRange RCBa _ -> cont 17#;
        TokenWithRange LBra _ -> cont 18#;
        TokenWithRange RBra _ -> cont 19#;
        TokenWithRange Comma _ -> cont 20#;
        TokenWithRange Dot _ -> cont 21#;
        TokenWithRange Sem _ -> cont 22#;
        TokenWithRange Bar _ -> cont 23#;
        TokenWithRange (Ident _) _ -> cont 24#;
        TokenWithRange (Quo _) _ -> cont 25#;
        TokenWithRange (IntLit _) _ -> cont 26#;
        TokenWithRange (Token.HaskellExpr _) _ -> cont 27#;
        TokenWithRange (Token.Hask _) _ -> cont 28#;
        _ -> happyError' ((tk:tks), [])
        }

happyError_ explist 29# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either Error a -> (a -> Either Error b) -> Either Error b
happyThen = (>>=)
happyReturn :: () => a -> Either Error a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either Error a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(TokenWithRange)], [Prelude.String]) -> Either Error a
happyError' = (\(tokens, _) -> parseError tokens)
parseProgramTokens tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

parseTermTokens tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

parseRuleTokens tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

parseGoalTokens tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------

-- Token accessor functions using record fields
getIdent :: TokenWithRange -> String
getIdent (TokenWithRange tok _) = tokVal tok

getString :: TokenWithRange -> String  
getString (TokenWithRange tok _) = tokVal tok

getIntLit :: TokenWithRange -> Int
getIntLit (TokenWithRange tok _) = tokInt tok

getBooLit :: TokenWithRange -> Bool
getBooLit (TokenWithRange tok _) = tokBool tok


getHaskellExpr :: TokenWithRange -> String
getHaskellExpr (TokenWithRange tok _) = tokVal tok

getHaskellBlock :: TokenWithRange -> String
getHaskellBlock (TokenWithRange tok _) = tokVal tok

getComment :: TokenWithRange -> String
getComment (TokenWithRange tok _) = tokVal tok

getQuo :: TokenWithRange -> String
getQuo (TokenWithRange tok _) = tokVal tok

-------------------------------------------------------------
-- Error handling
-------------------------------------------------------------

data Error = ParsingError { parseErrorPosition :: Position, parseErrorMessage :: String }
          deriving (Ord, Show, Eq)

parseError :: [TokenWithRange] -> Either Error a
parseError [] = Left $ ParsingError (Position 0 0 Nothing) "Parse error at end of input"
parseError (t:_) = Left $ ParsingError (rangeStart $ rangeOf t) $ "Parse error: unexpected " ++ show (tokenToken t)

-------------------------------------------------------------
-- Entrypoints
-------------------------------------------------------------

-- | Parse a program from string
parseProgram :: String -> Either Error Program
parseProgram input = 
  case lex input of
    tokens -> 
      let (comments, otherTokens) = partitionTokens tokens
          commentsAST = map (\t -> AST.Comment (getComment t) (rangeOf t)) comments
      in case parseProgramTokens otherTokens of
           Right program -> Right (Program (getDecls program) (commentsAST ++ getComments program))
           Left err -> Left err

-- | Separate comments from other tokens
partitionTokens :: [TokenWithRange] -> ([TokenWithRange], [TokenWithRange])
partitionTokens = partitionEithers . map classifyToken
  where
    classifyToken t@(TokenWithRange (Token.Comment _) _) = Left t
    classifyToken t = Right t

-- | Parse a single term from string  
parseTerm :: String -> Either Error PureTerm
parseTerm input = 
  case lex input of
    tokens -> 
      let (_, otherTokens) = partitionTokens tokens
      in parseTermTokens otherTokens

-- | Parse a single goal from string  
parseGoal :: String -> Either Error PureTerm
parseGoal input = 
  case lex input of
    tokens -> 
      let (_, otherTokens) = partitionTokens tokens
      in parseGoalTokens otherTokens


-- | Parse a single rule from string
parseRule :: String -> Either Error RuleDecl
parseRule input = 
  case lex input of
    tokens -> 
      let (_, otherTokens) = partitionTokens tokens
      in parseRuleTokens otherTokens
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#  define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#  define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#  define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#  define LT(n,m) (n Happy_GHC_Exts.<# m)
#  define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#  define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define ERROR_TOK 0#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) $
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO $ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " ++ show (Happy_GHC_Exts.I# st) ++
              ",\ttoken: " ++ show (Happy_GHC_Exts.I# i) ++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail (happyExpListPerState (Happy_GHC_Exts.I# st)) i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " ++ show (Happy_GHC_Exts.I# rule) ++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Just (Happy_GHC_Exts.I# act) -> act
  Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  = Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | otherwise
  = Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | otherwise         = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

{-# INLINE happyLt #-}
happyLt x y = LT(x,y)

readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (happyIndexOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 5#))) (bit `Prelude.mod` 32)
  where unbox_int (Happy_GHC_Exts.I# x) = x

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
-- trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_0 nt fn j tk st sts stk
     = happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk)

happySpecReduce_1 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_2 nt fn j tk _
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happySpecReduce_3 nt fn j tk _
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk)
                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn ERROR_TOK tk st sts stk
     = happyFail [] ERROR_TOK tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyAdjustOffset (happyIndexOffAddr happyGotoOffsets st1)
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            happyThen1 (fn stk tk)
                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " ++ show (Happy_GHC_Exts.I# new_state) ++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist ERROR_TOK tk old_st _ stk@(x `HappyStk` _) =
     let i = GET_ERROR_TOKEN(x) in
--      trace "failing" $
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st (HappyCons action sts)
                               (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction ERROR_TOK tk action sts (saved_tok`HappyStk`stk)
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk action sts stk =
-- trace "entering error recovery" $
        happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
