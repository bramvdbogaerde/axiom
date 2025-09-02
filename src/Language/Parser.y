{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Parser(parseProgram, parseTerm, parseRule, Error(..)) where

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

}

%name parseProgramTokens Program
%name parseTermTokens Term  
%name parseRuleTokens Rule
%tokentype { TokenWithRange }
%error { parseError }
%monad { Either Error } { >>= } { return }

%token
  '::='       { TokenWithRange IsDefinedAs _ }
  '~>'        { TokenWithRange LeadsTo _ }
  '=>'        { TokenWithRange Implies _ }
  '='         { TokenWithRange Equal _ }
  '/='        { TokenWithRange NotEqual _ }
  'rules'     { TokenWithRange Rules _ }
  'rule'      { TokenWithRange Rule _ }
  'syntax'    { TokenWithRange (Ident "syntax") _ }
  'in'        { TokenWithRange (Ident "in") _ }
  'transition' { TokenWithRange (Ident "transition") _ }
  '('         { TokenWithRange Lpar _ }
  ')'         { TokenWithRange Rpar _ }
  '{'         { TokenWithRange LCBa _ }
  '}'         { TokenWithRange RCBa _ }
  '['         { TokenWithRange LBra _ }
  ']'         { TokenWithRange RBra _ }
  ','         { TokenWithRange Comma _ }
  '.'         { TokenWithRange Dot _ }
  ';'         { TokenWithRange Sem _ }
  '|'         { TokenWithRange Bar _ }
  IDENT       { TokenWithRange (Ident _) _ }
  STRING      { TokenWithRange (Quo _) _ }
  INTLIT      { TokenWithRange (IntLit _) _ }
  HASKEXPR    { TokenWithRange (Token.HaskellExpr _) _ }
  HASKBLOCK   { TokenWithRange (Token.Hask _) _ }

%right '~>'
%left '=' '/='

%%

Program :: { Program }
Program : ProgramElements                    { Program $1 [] }

ProgramElements :: { [Decl] }
ProgramElements : {- empty -}                { [] }
                | ProgramElement ProgramElements { $1 : $2 }

ProgramElement :: { Decl }
ProgramElement : Declaration ';'             { $1 }
               | HaskellBlock                { $1 }

Declaration :: { Decl }
Declaration : SyntaxBlock                    { $1 }
            | RulesBlock                     { $1 }
            | TransitionDecl                 { $1 }
            | RewriteRule                    { $1 }

-- Syntax block: syntax { ... }
SyntaxBlock :: { Decl }
SyntaxBlock : 'syntax' '{' SyntaxDecls '}'   { Syntax $3 (mkRange $1 $4) }

SyntaxDecls :: { [SyntaxDecl] }
SyntaxDecls : {- empty -}                    { [] }
            | SyntaxDecl ';' SyntaxDecls     { $1 : $3 }

SyntaxDecl :: { SyntaxDecl }
SyntaxDecl : IdentList 'in' TypeRef MaybeProductions { SyntaxDecl $1 $3 (fromMaybe [] $4) (rangeOf $3) }

TypeRef :: { TypeCtor }
TypeRef : IDENT { TypeCtor (getIdent $1) [] (rangeOf $1) }
        | IDENT '(' TypeRefs ')' { TypeCtor (getIdent $1) $3 (rangeOf $1) }
      
TypeRefs :: { [TypeCtor] }
TypeRefs : {- empty -} { [] }
         | TypeRef { [$1] }
         | TypeRef ',' TypeRefs { $1 : $3 }


IdentList :: { [String] }
IdentList : IDENT                            { [getIdent $1] }
          | IDENT ',' IdentList              { getIdent $1 : $3 }

MaybeProductions :: { Maybe [PureTerm] }
MaybeProductions : {- empty -}               { Nothing }
                 | '::=' Productions         { Just $2 }

Productions :: { [PureTerm] }
Productions : Term                           { [$1] }
            | Term '|' Productions           { $1 : $3 }

-- Rules block: rules { ... }
RulesBlock :: { Decl }
RulesBlock : 'rules' '{' Rules '}'           { RulesDecl $3 (mkRange $1 $4) }

Rules :: { [RuleDecl] }
Rules : {- empty -}                          { [] }
      | Rule ';' Rules                       { $1 : $3 }

Rule :: { RuleDecl }
Rule : 'rule' STRING '[' Goals ']' '=>' '[' TermList ']' { RuleDecl (getString $2) $4 $8 (mkRange $1 $9) }

Goals :: { [PureTerm] }
Goals : {- empty -} { [] }
      | Goal { [$1] }
      | Goal ';' Goals { $1 : $3 }

TermList :: { [PureTerm] }
TermList : {- empty -}                       { [] }
         | Terms                             { $1 }

Terms :: { [PureTerm] }
Terms : Term                                 { [$1] }
      | Term ';' Terms                       { $1 : $3 }

-- Transition declaration: transition Type ~> Type  
TransitionDecl :: { Decl }
TransitionDecl : 'transition' IDENT '~>' IDENT { TransitionDecl "~>" (Sort (getIdent $2), rangeOf $2) (Sort (getIdent $4), rangeOf $4) (mkRange $1 $4) }

-- Rewrite rule: name(args) = body
RewriteRule :: { Decl }
RewriteRule : IDENT '(' TermArgs ')' '=' Term { Rewrite (RewriteDecl (getIdent $1) $3 $6 (mkRange $1 $6)) (mkRange $1 $6) }

TermArgs :: { [PureTerm] }
TermArgs : {- empty -}                       { [] }
         | TermArgList                       { $1 }

TermArgList :: { [PureTerm] }
TermArgList : Term                           { [$1] }
            | Term ',' TermArgList           { $1 : $3 }

-- Haskell block
HaskellBlock :: { Decl }
HaskellBlock : HASKBLOCK                     { HaskellDecl (getHaskellBlock $1) (rangeOf $1) }

-- Terms with precedence handling
Term :: { PureTerm }
Term : BasicTerm                             { $1 }
     | Term '=' Term                         { Eqq $1 $3 () (mkRange $1 $3) }
     | Term '/=' Term                        { Neq $1 $3 () (mkRange $1 $3) }
     | Term '~>' Term                        { Transition "~>" $1 $3 () (mkRange $1 $3) }

BasicTerm :: { PureTerm }
BasicTerm : IDENT                            { Atom (Identity (getIdent $1)) () (rangeOf $1) }
          | IDENT '(' TermArgs ')'           { Functor (getIdent $1) $3 () (mkRange $1 $4) }
          | HASKEXPR                         { AST.HaskellExpr (getHaskellExpr $1) () (rangeOf $1) }
          | INTLIT                           { TermValue (IntValue (getIntLit $1)) () (rangeOf $1) }
          | '(' Term ')'                     { $2 }

Goal :: { PureTerm }
Goal : Term { $1 }
     | IDENT 'in' Term { IncludedIn (getIdent $1) $3 (mkRange $1 $3) }

{

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

getHaskellExpr :: TokenWithRange -> String
getHaskellExpr (TokenWithRange tok _) = tokVal tok

getHaskellBlock :: TokenWithRange -> String
getHaskellBlock (TokenWithRange tok _) = tokVal tok

getComment :: TokenWithRange -> String
getComment (TokenWithRange tok _) = tokVal tok

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

-- | Parse a single rule from string
parseRule :: String -> Either Error RuleDecl
parseRule input = 
  case lex input of
    tokens -> 
      let (_, otherTokens) = partitionTokens tokens
      in parseRuleTokens otherTokens

}
