{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
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
import Language.Parser (parseTerm)
import Language.TemplateHaskell.SyntaxExtra (freeVars)
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Reader hiding (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Applicative ((<|>))
import qualified Control.Monad.Reader as Reader
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromJust, catMaybes)
import Data.List (stripPrefix)
import Data.Bifunctor (first)
import NeatInterpolation
import qualified Control.Monad.Trans as Trans
import Control.Monad.Except (liftEither)

------------------------------------------------------------
-- Prelude & module creation
------------------------------------------------------------

makeModule :: String -> String -> String -> String
makeModule prelude ast testQueries = T.unpack
  [text|
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE LambdaCase #-}
  -- AnalysisLang related imports
  import Language.CodeGen.Prelude
  import qualified Language.AST
  import qualified Language.Range
  import qualified Language.Types
  import Language.Solver
  import qualified Language.Solver.BacktrackingST as ST

  -- Haskell imports
  import Data.Functor.Identity
  import qualified Data.Either
  import qualified Data.Maybe
  import qualified GHC.Base
  import qualified GHC.Types  
  import qualified Data.Map as Map
  import System.Exit
  import Data.List (stripPrefix)
  import Data.Maybe (catMaybes)
  import qualified Data.Map.Internal

  
  import GHC.Maybe

  $prelude'


  ast :: CodeGenProgram
  ast = $ast'

  -- Test queries parsed and type checked during code generation
  testQueries :: [(PureTerm' CodeGenPhase, Bool)]  -- (query, shouldPass)
  testQueries = $testQueries'

  main :: IO ()
  main = do
    putStrLn $ "Running " ++ show (length testQueries) ++ " test queries..."
    results <- mapM runTestQuery (zip [1..] testQueries)
    let passed = length $ filter id results
        total = length results
        failed = total - passed
    
    putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"
    
    if failed == 0
      then do
        putStrLn "All tests passed!"
        exitWith ExitSuccess
      else do
        putStrLn $ show failed ++ " tests failed"
        exitWith (ExitFailure 1)

  runTestQuery :: (Int, (PureTerm' CodeGenPhase, Bool)) -> IO Bool
  runTestQuery (idx, (query, shouldPass)) = do
    putStr $ "Testing query " ++ show idx ++ ": " ++ show query ++ 
             " (expected: " ++ (if shouldPass then "PASS" else "FAIL") ++ ") ... "
    let Program decls _ = ast
    let rules = [rule | RulesDecl rules _ <- decls, rule <- rules]
    let engineCtx = fromRules @[] rules
    let solverComputation = ST.runST $ runSolver engineCtx (solve @CodeGenPhase @[] query)
    
    let hasSolution = not $ null solverComputation
    let testPassed = hasSolution == shouldPass
    
    if testPassed
      then do
        putStrLn $ if shouldPass then "PASS" else "FAIL (as expected)"
        return True
      else do
        putStrLn $ if shouldPass then "FAIL (unexpected)" else "PASS (unexpected)"
        return False
  |]
  where
    ast' = T.pack ast
    testQueries' = T.pack testQueries
    prelude' = T.pack prelude


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

    -- Generate the HaskellHatch data structure
    [| HaskellHatch $(lift haskellCode) $(lift freeVarList) Map.empty $(generateExecuteFunction haskellExp freeVarList typingCtx expectedType) |]

-- | Generate the execute function for a Haskell expression  
generateExecuteFunction :: Exp -> [String] -> Gamma -> Typ -> Q Exp
generateExecuteFunction haskellExp freeVarList typingCtx expectedType = do
  -- Generate unique variable names
  proxName <- newName "prox"
  mappingName <- newName "mapping"

  -- Generate variable extraction expressions
  extractExprs <- mapM (generateExtraction typingCtx mappingName) freeVarList
  wrapExpr <- wrapResult haskellExp (primTyp expectedType) proxName

  -- Generate the lambda function
  baseExpr <- [| Right $(return wrapExpr) |]
  LamE [VarP proxName, VarP mappingName] <$>
    foldM (\acc (varName, extractExpr) -> buildLet (varName, extractExpr) acc)
          baseExpr
          (zip freeVarList extractExprs)
  where
    buildLet :: (String, Maybe Exp) -> Exp -> Q Exp
    buildLet (varName, Just extractExpr) bodyExpr =
      let varNameE = mkName varName
      in [| $(return extractExpr) >>= \ $(varP varNameE) -> $(return bodyExpr) |]
    buildLet (varName, Nothing) bodyExpr = [| $(return bodyExpr) |]


-- | Generate extraction code for a single variable
generateExtraction :: Gamma -> Name -> String -> Q (Maybe Exp)
generateExtraction typingCtx mappingName varName = do
  runMaybeT $ do
      baseVarName <- MaybeT $ return $ safeVariableName varName
      sortName <- MaybeT $ return $ lookupGamma baseVarName typingCtx
      let typ = sortName

      Trans.lift [|
           maybe (Left $ UserError $ "Variable " ++ $(lift varName) ++ " not found")
                 (\case
                   TermValue value _ _ -> maybe (Left $ InvalidTypePassed $(lift typ) (typeOf value))
                                               Right
                                               (asType $(typHaskEx typ) value)
                   _ -> Left $ UserError $ "Expected TermValue for variable " ++ $(lift varName))
                 (Map.lookup $(lift varName) $(varE mappingName))
       |]

-- | Generate code to wrap the result of a Haskell expression into a PureTerm
wrapResult :: Exp -> Typ -> Name -> Q Exp
wrapResult haskellExp IntType proxName =
  [| TermValue (IntValue $(return haskellExp)) (typeAnnot $(varE proxName) IntType) dummyRange |]
wrapResult haskellExp StrType proxName =
  [| TermValue (StrValue $(return haskellExp)) (typeAnnot $(varE proxName) StrType) dummyRange |]
wrapResult _ typ _ =
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

  HaskellDecl s range ->
    [| HaskellDecl $(lift s) $(rangeToExp range) |]

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

-- | Extract test queries from comments that start with "codegen_test: " or "codegen_fail_test: "
extractTestQueries :: [Comment' p] -> [(String, Bool)]  -- (query, shouldPass)
extractTestQueries = catMaybes . map extractQuery
  where
    extractQuery (Comment content _) =
      let prefixes = [(" codegen_test: ", True), ("codegen_test: ", True),
                      (" codegen_fail_test: ", False), ("codegen_fail_test: ", False)]
          tryPrefix (prefix, shouldPass) = ((,shouldPass) <$> stripPrefix prefix content)
      in foldr ((<|>) . tryPrefix) Nothing prefixes

-- | Parse and type check test queries during code generation
processTestQueries :: CheckingContext -> [(String, Bool)] -> Q [Exp]
processTestQueries ctx = mapM (fmap (either error id) . runExceptT . processQuery ctx)
  where
    processQuery :: CheckingContext -> (String, Bool) -> ExceptT String Q Exp
    processQuery ctx (queryStr, shouldPass) = do
      parsedQuery <- liftEither $ first ((("Failed to parse test query '" ++ queryStr ++ "': ") ++) . show) $ parseTerm queryStr
      typedQuery <- liftEither $ first ((("Failed to type check test query '" ++ queryStr ++ "': ") ++) . show) $
                    runCheckTerm ctx parsedQuery
      Trans.lift $ do
        queryExp <- pureTermToExp ctx typedQuery
        [| ($(return queryExp), $( if shouldPass then [| True |] else [| False |] )) |]

-- | Generate a Haskell program representing the Typed program with executable Haskell functions in it.
codegen :: CheckingContext -> TypedProgram -> IO String
codegen context prog@(Program _ comments) = runQ $ do
  let testQueryStrings = extractTestQueries comments
  (makeModule (concat (haskellBlocks prog)) . pprint <$> astToCode context prog)
    <*> (pprint <$> (processTestQueries context testQueryStrings >>= listE . map return))

