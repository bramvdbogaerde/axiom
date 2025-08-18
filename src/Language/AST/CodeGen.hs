{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Language.AST.CodeGen (
    astToCode
  ) where

import Language.Haskell.TH hiding (Range)
import Language.Haskell.TH.Syntax hiding (Range)
import Language.AST
import Language.Range
import Data.Functor.Identity

astToCode :: Program -> Q Exp
astToCode = astToCodeQ

astToCodeQ :: Program -> Q Exp
astToCodeQ prog = [| $(programToExp prog) |]

programToExp :: Program -> Q Exp
programToExp (Program decls comments) = 
  [| Program $(listE (map declToExp decls)) $(listE (map commentToExp comments)) |]

commentToExp :: Comment -> Q Exp
commentToExp (Comment str range) = 
  [| Comment $(lift str) $(rangeToExp range) |]

declToExp :: Decl -> Q Exp
declToExp = \case
  Syntax syntaxDecls range -> 
    [| Syntax $(listE (map syntaxDeclToExp syntaxDecls)) $(rangeToExp range) |]
  
  Rewrite rewriteDecl range -> 
    [| Rewrite $(rewriteDeclToExp rewriteDecl) $(rangeToExp range) |]
  
  RulesDecl ruleDecls range -> 
    [| RulesDecl $(listE (map ruleDeclToExp ruleDecls)) $(rangeToExp range) |]
  
  TransitionDecl name (tpy1, range1) (tpy2, range2) range -> 
    [| TransitionDecl $(lift name) ($(lift tpy1), $(rangeToExp range1)) ($(lift tpy2), $(rangeToExp range2)) $(rangeToExp range) |]

syntaxDeclToExp :: SyntaxDecl -> Q Exp
syntaxDeclToExp (SyntaxDecl vars tpy prods range) = 
  [| SyntaxDecl $(lift vars) $(lift tpy) $(listE (map pureTermToExp prods)) $(rangeToExp range) |]

rewriteDeclToExp :: RewriteDecl -> Q Exp
rewriteDeclToExp (RewriteDecl name args body range) = 
  [| RewriteDecl $(lift name) $(listE (map pureTermToExp args)) $(pureTermToExp body) $(rangeToExp range) |]

ruleDeclToExp :: RuleDecl -> Q Exp
ruleDeclToExp (RuleDecl name precedent consequent range) = 
  [| RuleDecl $(lift name) $(listE (map pureTermToExp precedent)) $(listE (map pureTermToExp consequent)) $(rangeToExp range) |]

pureTermToExp :: PureTerm -> Q Exp
pureTermToExp = \case
  Atom (Identity name) tpy range -> 
    [| Atom (Identity $(lift name)) $(lift tpy) $(rangeToExp range) |]
  
  Functor fname args tpy range -> 
    [| Functor $(lift fname) $(listE (map pureTermToExp args)) $(lift tpy) $(rangeToExp range) |]
  
  Eqq left right tpy range -> 
    [| Eqq $(pureTermToExp left) $(pureTermToExp right) $(lift tpy) $(rangeToExp range) |]
  
  Neq left right tpy range -> 
    [| Neq $(pureTermToExp left) $(pureTermToExp right) $(lift tpy)  $(rangeToExp range) |]
  
  Transition tname left right tpy range -> 
    [| Transition $(lift tname) $(pureTermToExp left) $(pureTermToExp right) $(lift tpy) $(rangeToExp range) |]

rangeToExp :: Range -> Q Exp
rangeToExp (Range (Position line1 col1 fname1) (Position line2 col2 fname2)) =
  [| Range (Position $(lift line1) $(lift col1) $(lift fname1)) (Position $(lift line2) $(lift col2) $(lift fname2)) |]
