{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
module Language.CodeGen (
    astToCode,
    codegen
  ) where

import Language.Haskell.TH hiding (Range)
import Language.Haskell.TH.Syntax hiding (Range)
import Language.Haskell.Meta.Parse (parseExp)
import Language.AST
import Language.Range
import Language.TypeCheck
import Language.Types
import Language.TemplateHaskell.SyntaxExtra (freeVars)
import Data.Functor.Identity
import Control.Monad.Reader hiding (lift)
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import NeatInterpolation
 
------------------------------------------------------------
-- Prelude & module creation
------------------------------------------------------------

makeModule :: String -> String
makeModule ast = T.unpack
  [text|
  -- AnalysisLang related imports
  import Language.AST.CodeGen.Prelude
  import qualified Language.AST
  import qualified Language.Range
  import qualified Language.Types

  -- Haskell imports
  import Data.Functor.Identity

  
  import GHC.Maybe
  ast :: CodeGenProgram
  ast = $ast'
  |]
  where ast' = T.pack ast


------------------------------------------------------------
-- Monad
------------------------------------------------------------

type CodeGenM = ReaderT CheckingContext Q

------------------------------------------------------------
-- Haskell expression embedding
------------------------------------------------------------

embedExpression :: String -> Typ -> CodeGenM Exp
embedExpression haskellCode expectedType = do
  ctx <- ask
  Reader.lift $ do
    -- Parse the Haskell expression string into a Template Haskell expression
    haskellExp <- either fail return (parseExp haskellCode)
    
    -- Extract free variables from the parsed expression
    let freeVarSet = freeVars haskellExp
        freeVarList = Set.toList freeVarSet
        typingCtx = _typingContext ctx
    
    -- Validate that all free variables exist in typing context during code gen
    mapM_ (validateVariable typingCtx) freeVarList
    
    -- Generate the HaskellHatch data structure
    [| HaskellHatch $(lift freeVarList) $(generateExecuteFunction haskellExp freeVarList typingCtx expectedType) |]

-- | Validate that a variable exists in the typing context (code gen phase)
validateVariable :: Gamma -> String -> Q ()
validateVariable typingCtx varName = do
  let baseVarName = variableName varName
  maybe (fail $ "Variable " ++ baseVarName ++ " not found in typing context") 
        (const $ return ()) 
        (lookupGamma baseVarName typingCtx)

-- | Generate the execute function for a Haskell expression  
generateExecuteFunction :: Exp -> [String] -> Gamma -> Typ -> Q Exp
generateExecuteFunction haskellExp freeVarList typingCtx expectedType = do
  -- Generate variable extraction expressions
  extractExprs <- mapM (generateExtraction typingCtx) freeVarList
  wrapExpr <- wrapResult haskellExp expectedType
  
  -- Generate the lambda function
  baseExpr <- [| Right $(return wrapExpr) |]
  resultExpr <- foldM (\acc (varName, extractExpr) -> buildLet (varName, extractExpr) acc)
                      baseExpr
                      (zip freeVarList extractExprs)
  [| \mapping -> $(return resultExpr) |]
  where
    buildLet :: (String, Exp) -> Exp -> Q Exp
    buildLet (varName, extractExpr) bodyExpr = do
      let varNameE = mkName varName
      [| $(return extractExpr) mapping >>= \ $(varP varNameE) -> $(return bodyExpr) |]

-- | Generate extraction code for a single variable
generateExtraction :: Gamma -> String -> Q Exp
generateExtraction typingCtx varName = do
  let baseVarName = variableName varName
      sortName = fromJust $ lookupGamma baseVarName typingCtx  -- Safe due to validateVariable
      typeName = getSortName sortName
      typ = fromSortName typeName
  
  [| \mapping -> 
       maybe (Left $ UserError $ "Variable " ++ $(lift varName) ++ " not found") 
             (\case 
               TermValue value _ _ -> maybe (Left $ InvalidTypePassed $(lift typ) (typeOf value))
                                           Right
                                           (asType $(typHaskEx typ) value)
               _ -> Left $ UserError $ "Expected TermValue for variable " ++ $(lift varName))
             (Map.lookup $(lift varName) mapping)
   |]

-- | Generate code to wrap the result of a Haskell expression into a PureTerm
wrapResult :: Exp -> Typ -> Q Exp
wrapResult haskellExp IntType = 
  [| TermValue (IntValue $(return haskellExp)) IntType dummyRange |]
wrapResult haskellExp StrType = 
  [| TermValue (StrValue $(return haskellExp)) StrType dummyRange |]
wrapResult _ typ = 
  fail $ "Unsupported result type: " ++ show typ

------------------------------------------------------------
-- AST lifting
------------------------------------------------------------

astToCode :: CheckingContext -> TypedProgram -> Q Exp
astToCode ctx prog = runReaderT (astToCodeQ prog) ctx

astToCodeQ :: TypedProgram -> CodeGenM Exp
astToCodeQ prog = do
  ctx <- ask
  Reader.lift [| $(programToExp ctx prog) |]

programToExp :: CheckingContext -> TypedProgram -> Q Exp
programToExp ctx (Program decls comments) = 
  [| Program $(listE (map (declToExp ctx) decls)) $(listE (map commentToExp comments)) |]

commentToExp :: Comment' p -> Q Exp
commentToExp (Comment str range) = 
  [| Comment $(lift str) $(rangeToExp range) |]

declToExp :: CheckingContext -> TypedDecl -> Q Exp
declToExp ctx = \case
  Syntax syntaxDecls range -> 
    [| Syntax $(listE (map (syntaxDeclToExp ctx) syntaxDecls)) $(rangeToExp range) |]
  
  Rewrite rewriteDecl range -> 
    [| Rewrite $(rewriteDeclToExp ctx rewriteDecl) $(rangeToExp range) |]
  
  RulesDecl ruleDecls range -> 
    [| RulesDecl $(listE (map (ruleDeclToExp ctx) ruleDecls)) $(rangeToExp range) |]
  
  TransitionDecl name (tpy1, range1) (tpy2, range2) range -> 
    [| TransitionDecl $(lift name) ($(lift tpy1), $(rangeToExp range1)) ($(lift tpy2), $(rangeToExp range2)) $(rangeToExp range) |]

syntaxDeclToExp :: CheckingContext -> TypedSyntaxDecl -> Q Exp
syntaxDeclToExp ctx (SyntaxDecl vars tpy prods range) = 
  [| SyntaxDecl $(lift vars) $(lift tpy) $(listE (map (pureTermToExp ctx) prods)) $(rangeToExp range) |]

rewriteDeclToExp :: CheckingContext -> TypedRewriteDecl -> Q Exp
rewriteDeclToExp ctx (RewriteDecl name args body range) = 
  [| RewriteDecl $(lift name) $(listE (map (pureTermToExp ctx) args)) $(pureTermToExp ctx body) $(rangeToExp range) |]

ruleDeclToExp :: CheckingContext -> TypedRuleDecl -> Q Exp
ruleDeclToExp ctx (RuleDecl name precedent consequent range) = 
  [| RuleDecl $(lift name) $(listE (map (pureTermToExp ctx) precedent)) $(listE (map (pureTermToExp ctx) consequent)) $(rangeToExp range) |]

pureTermToExp :: CheckingContext -> TypedTerm -> Q Exp  
pureTermToExp ctx = \case
  Atom (Identity name) tpy range -> 
    [| Atom (Identity $(lift name)) $(lift tpy) $(rangeToExp range) |]
  
  Functor fname args tpy range -> 
    [| Functor $(lift fname) $(listE (map (pureTermToExp ctx) args)) $(lift tpy) $(rangeToExp range) |]
  
  Eqq left right tpy range -> 
    [| Eqq $(pureTermToExp ctx left) $(pureTermToExp ctx right) $(lift tpy) $(rangeToExp range) |]
  
  Neq left right tpy range -> 
    [| Neq $(pureTermToExp ctx left) $(pureTermToExp ctx right) $(lift tpy) $(rangeToExp range) |]
  
  Transition tname left right tpy range -> 
    [| Transition $(lift tname) $(pureTermToExp ctx left) $(pureTermToExp ctx right) $(lift tpy) $(rangeToExp range) |]
  
  HaskellExpr expr tpy range -> do
    haskellHatch <- runReaderT (embedExpression expr tpy) ctx
    [| HaskellExpr $(return haskellHatch) $(lift tpy) $(rangeToExp range) |]
  
  TermValue value tpy range ->
    [| TermValue $(lift value) $(lift tpy) $(rangeToExp range) |]

rangeToExp :: Range -> Q Exp
rangeToExp (Range (Position line1 col1 fname1) (Position line2 col2 fname2)) =
  [| Range (Position $(lift line1) $(lift col1) $(lift fname1)) (Position $(lift line2) $(lift col2) $(lift fname2)) |]

------------------------------------------------------------
-- Entrypoints
------------------------------------------------------------

-- | Generate a Haskell program representing the Typed program with executable Haskell functions in it.
codegen :: CheckingContext -> TypedProgram -> IO String
codegen context = fmap (makeModule . pprint) . runQ . astToCode context

