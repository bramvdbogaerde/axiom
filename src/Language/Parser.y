{
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

}

%name parseProgramTokens Program
%name parseTermTokens Term  
%name parseRuleTokens Rule
%name parseGoalTokens Goal
%tokentype { TokenWithRange }
%error { parseError }
%monad { Either Error } { >>= } { return }

%token
  '::='       { TokenWithRange IsDefinedAs _ }
  '~>'        { TokenWithRange LeadsTo _ }
  '=>'        { TokenWithRange Implies _ }
  '='         { TokenWithRange Equal _ }
  '/='        { TokenWithRange NotEqual _ }
  '⇓'         { TokenWithRange BigStep _ }
  'rules'     { TokenWithRange Rules _ }
  'rule'      { TokenWithRange Rule _ }
  BOOL        { TokenWithRange (Boo _) _ }
  'syntax'    { TokenWithRange (Ident "syntax") _ }
  'in'        { TokenWithRange (Ident "in") _ }
  'transition' { TokenWithRange (Ident "transition") _ }
  'import'    { TokenWithRange (Ident "import") _ }
  'alias'     { TokenWithRange (Ident "alias") _ }
  'as'        { TokenWithRange (Ident "as") _ }
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
  '|->'       { TokenWithRange MapsTo _ }
  '_'         { TokenWithRange Token.Wildcard _ }
  IDENT       { TokenWithRange (Ident _) _ }
  STRING      { TokenWithRange (Quo _) _ }
  INTLIT      { TokenWithRange (IntLit _) _ }
  HASKEXPR    { TokenWithRange (Token.HaskellExpr _) _ }
  HASKBLOCK   { TokenWithRange (Token.Hask _) _ }
  HASKPOST    { TokenWithRange (Token.HaskPost _) _ }

%right '~>'
%left '=' '/=' '⇓'

%%

Program :: { Program }
Program : ProgramElements                    { Program $1 [] }

ProgramElements :: { [Decl] }
ProgramElements : {- empty -}                { [] }
                | ProgramElement ProgramElements { $1 : $2 }

ProgramElement :: { Decl }
ProgramElement : Declaration ';'             { $1 }
               | HaskellBlock                { $1 }
               | HaskellPost                 { $1 }

Declaration :: { Decl }
Declaration : SyntaxBlock                    { $1 }
            | RulesBlock                     { $1 }
            | TransitionDecl                 { $1 }
            | RewriteRule                    { $1 }
            | ImportDecl                     { $1 }

-- Syntax block: syntax { ... } or syntax name { ... }
SyntaxBlock :: { Decl }
SyntaxBlock : 'syntax' '{' SyntaxDecls '}'   { Syntax Nothing $3 (mkRange $1 $4) }
            | 'syntax' IDENT '{' SyntaxDecls '}' { Syntax (Just (getIdent $2)) $4 (mkRange $1 $5) }

SyntaxDecls :: { [SyntaxDecl] }
SyntaxDecls : {- empty -}                    { [] }
            | SyntaxDecl ';' SyntaxDecls     { $1 : $3 }

SyntaxDecl :: { SyntaxDecl }
SyntaxDecl : 'alias' TypeRef 'as' IDENT { TypeAliasDecl (getIdent $4) $2 (mkRange $1 $4) }
           | TypeRef MaybeProductions { SyntaxDecl [] $1 (fromMaybe [] $2) (rangeOf $1) }
           | IdentList 'in' TypeRef MaybeProductions { SyntaxDecl $1 $3 (fromMaybe [] $4) (rangeOf $3) }

TypeRef :: { TypeCon }
TypeRef : IDENT { TypeApp (TypeTyp (getIdent $1) (rangeOf $1)) [] (rangeOf $1) }
        | IDENT '(' TypeRefs ')' { TypeApp (TypeTyp (getIdent $1) (rangeOf $1)) $3 (rangeOf $1) }
        | HASKEXPR { TypeHas (getHaskellExpr $1) (rangeOf $1) }
      
TypeRefs :: { [TypeCon] }
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

-- Rules block: rules { ... } or rules name { ... }
RulesBlock :: { Decl }
RulesBlock : 'rules' '{' Rules '}'           { RulesDecl Nothing $3 (mkRange $1 $4) }
           | 'rules' IDENT '{' Rules '}'     { RulesDecl (Just (getIdent $2)) $4 (mkRange $1 $5) }

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

-- Import declaration: import "filename"
ImportDecl :: { Decl }
ImportDecl : 'import' STRING                 { Import (getQuo $2) (mkRange $1 $2) }

TermArgs :: { [PureTerm] }
TermArgs : {- empty -}                       { [] }
         | TermArgList                       { $1 }

TermArgList :: { [PureTerm] }
TermArgList : Term                           { [$1] }
            | Term ','  TermArgList           { $1 : $3 }

-- Haskell block
HaskellBlock :: { Decl }
HaskellBlock : HASKBLOCK                     { HaskellDecl (getHaskellBlock $1) True (rangeOf $1) }

-- Haskell block to be inserted at the end
HaskellPost :: { Decl }
HaskellPost : HASKPOST { HaskellDecl (getHaskellBlock $1) False (rangeOf $1) }

-- Terms with precedence handling
Term :: { PureTerm }
Term : BasicTerm                             { $1 }
     | Term '~>' Term                        { Transition "~>" $1 $3 () (mkRange $1 $3) }
     | BigStepExpr                           { $1 }
     | '{' Elements '}'                      { SetOfTerms (Set.fromList $2) () (mkRange $1 $3) }

-- Big step evaluation relation with comma-separated terms on both sides
BigStepExpr :: { PureTerm }
BigStepExpr : '(' Elements ')' '⇓' '(' Elements ')' { Functor "⇓" ($2 ++ $6) () (mkRange $1 $7) }

-- Expressions
Expr :: { PureExpr }
Expr : '[' Bindings ']' { curryUpdateMap (TermExpr (EmptyMap () (mkRange $1 $3)) (mkRange $1 $3)) $2 () (mkRange $1 $3) }
     | BasicTerm '[' Bindings ']' { curryUpdateMap $1 $3 () (mkRange $1 $4) }

Bindings :: { [(PureTerm, PureTerm)] }
Bindings : {- empty -} { [] }
         | Binding { [$1] }
         | Binding ';' Bindings { $1 : $3 }
Binding :: { (PureTerm, PureTerm) }
Binding : Term '|->' Term { ($1, $3) }

-- Sequence of terms seperated by a ","
Elements :: { [PureTerm] }
Elements : {- empty -} { [] }
         | Term { [$1] }
         | Term ',' Elements { $1 : $3 }

BasicTerm :: { PureTerm }
BasicTerm : IDENT                            { Atom (Identity (getIdent $1)) () (rangeOf $1) }
          | IDENT '(' TermArgs ')'           { Functor (getIdent $1) $3 () (mkRange $1 $4) }
          | HASKEXPR                         { AST.HaskellExpr (getHaskellExpr $1) () (rangeOf $1) }
          | INTLIT                           { TermValue (IntValue (getIntLit $1)) () (rangeOf $1) }
          | BOOL                             { TermValue (BooValue (getBooLit $1))  () (rangeOf $1) }
          | '_'                              { AST.Wildcard () (rangeOf $1) }
          | STRING                           { TermValue (StrValue (getString $1)) () (rangeOf $1) }
          | Expr { TermExpr $1 (rangeOf $1) }

Goal :: { PureTerm }
Goal : Term { $1 }
     | IDENT 'in' Term { IncludedIn (getIdent $1) $3 (mkRange $1 $3) }
     | Term '=' Term                         { Eqq $1 $3 () (mkRange $1 $3) }
     | Term '/=' Term                        { Neq $1 $3 () (mkRange $1 $3) }


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

-- | Parse a program from string with the given filename information
parseProgramWithFilename :: String -- ^ the name of the file where the contents originate from
                         -> String -- ^ the program contents themselves
                         -> Either Error Program
parseProgramWithFilename filename input = 
  case lexWithFilename filename input of
    tokens -> 
      let (comments, otherTokens) = partitionTokens tokens
          commentsAST = map (\t -> AST.Comment (getComment t) (rangeOf t)) comments
      in case parseProgramTokens otherTokens of
           Right program -> Right (Program (getDecls program) (commentsAST ++ getComments program))
           Left err -> Left err

-- | Parse a program from a string
parseProgram :: String -> Either Error Program
parseProgram = parseProgramWithFilename "<unknown>"

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

}
