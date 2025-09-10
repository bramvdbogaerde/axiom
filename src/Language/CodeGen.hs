{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
module Language.CodeGen (
    astToCode,
    codegen,
  ) where

import Language.Haskell.TH hiding (Range)
import Language.Haskell.TH.Syntax hiding (Range)
import Language.Haskell.Meta.Parse (parseExp)
import Language.AST
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
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List (stripPrefix)
import qualified Data.List as List
import Data.Bifunctor (first, Bifunctor (second))
import Data.Void (absurd)
import NeatInterpolation
import qualified Control.Monad.Trans as Trans
import Control.Monad.Except (liftEither)
import Data.Either (fromRight)
import Language.Haskell.Meta (parseType)

------------------------------------------------------------
-- Prelude & module creation
------------------------------------------------------------

makeModule :: Bool -> String -> String -> String -> String -> String  
makeModule enableDebugger prelude ast testQueries termDecls = T.unpack
  [text|
  {-# LANGUAGE ScopedTypeVariables #-}
  {-# LANGUAGE TypeApplications #-}
  {-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE TypeFamilies #-}
  -- AnalysisLang related imports
  import Language.CodeGen.Prelude
  import Language.CodeGen.Phase (CodeGenPhase)
  import qualified Language.AST
  import Language.AST (XEmbeddedValue)
  import qualified Language.Range
  import qualified Language.Types
  import Language.Solver
  import qualified Language.Solver.BacktrackingST as ST
  $debuggerImports'

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
  -- GHC imports
  import GHC.Show  
  import GHC.Maybe

  -- User prelude

  $prelude'

  -- Type family instance for embedded values
  type instance XEmbeddedValue CodeGenPhase = HaskellValue

  -- Term declarations

  $termDecls'

  -- Parsed AST

  ast :: CodeGenProgram
  ast = $ast'

  -- Queries

  -- Test queries parsed and type checked during code generation
  testQueries :: [(PureTerm' CodeGenPhase, Bool)]  -- (query, shouldPass)
  testQueries = $testQueries'

  main :: IO ()
  main = do
    $debuggerMain'

  $debuggerREPL'

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
    termDecls' = T.pack termDecls
    debuggerImports' = T.pack $ if enableDebugger 
                                then unlines
                                  [ "import qualified Language.SolverDebugger"
                                  , "import qualified System.IO" 
                                  , "import qualified Text.Read"
                                  ]
                                else ""
    debuggerMain' = T.pack $ if enableDebugger
                            then debuggerMainCode
                            else testRunnerMainCode
    
    testRunnerMainCode = unlines
      [ "putStrLn $ \"Running \" ++ show (length testQueries) ++ \" test queries...\""
      , "results <- mapM runTestQuery (zip [1..] testQueries)"
      , "let passed = length $ filter id results"
      , "    total = length results"
      , "    failed = total - passed"
      , ""
      , "putStrLn $ \"Results: \" ++ show passed ++ \"/\" ++ show total ++ \" passed\""
      , ""
      , "if failed == 0"
      , "  then do"
      , "    putStrLn \"All tests passed!\""
      , "    exitWith ExitSuccess"  
      , "  else do"
      , "    putStrLn $ show failed ++ \" tests failed\""
      , "    exitWith (ExitFailure 1)"
      ]
    
    debuggerMainCode = unlines
      [ "putStrLn \"Generated code debugger\""
      , "putStrLn \"Available test queries:\""
      , "mapM_ (\\(i, (query, _)) -> putStrLn $ show i ++ \": \" ++ show query) (zip [1..] testQueries)"
      , "debuggerREPL"
      ]
    
    debuggerREPL' = T.pack $ if enableDebugger then debuggerREPLCode else ""
    
    debuggerREPLCode = unlines
      [ "debuggerREPL :: IO ()"
      , "debuggerREPL = do"
      , "  putStr \"debug> \""
      , "  System.IO.hFlush System.IO.stdout"
      , "  input <- getLine"
      , "  case input of"
      , "    \"quit\" -> putStrLn \"Goodbye!\""
      , "    \"help\" -> do"
      , "      putStrLn \"Commands:\""
      , "      putStrLn \"  help    - Show this help\""
      , "      putStrLn \"  list    - List available queries\""
      , "      putStrLn \"  run N   - Debug query number N\""
      , "      putStrLn \"  quit    - Exit debugger\""
      , "      debuggerREPL"
      , "    \"list\" -> do"
      , "      putStrLn \"Available test queries:\""
      , "      mapM_ (\\(i, (query, _)) -> putStrLn $ show i ++ \": \" ++ show query) (zip [1..] testQueries)"
      , "      debuggerREPL"
      , "    _ | Just queryNumStr <- Data.List.stripPrefix \"run \" input -> do"
      , "        case Text.Read.readMaybe queryNumStr of"
      , "          Just queryNum | queryNum >= 1 && queryNum <= length testQueries -> do"
      , "            let (query, _) = testQueries !! (queryNum - 1)"
      , "            debugQuery query"
      , "            debuggerREPL"
      , "          _ -> do"
      , "            putStrLn $ \"Invalid query number. Use 1-\" ++ show (length testQueries)"
      , "            debuggerREPL"
      , "    _ -> do"
      , "      putStrLn \"Unknown command. Type 'help' for available commands.\""
      , "      debuggerREPL"
      , ""
      , "debugQuery :: PureTerm' CodeGenPhase -> IO ()"
      , "debugQuery query = do"
      , "  putStrLn $ \"Debugging query: \" ++ show query"
      , "  let Program decls _ = ast"
      , "  let rules = [rule | RulesDecl rules _ <- decls, rule <- rules]"
      , "  let config = Language.SolverDebugger.defaultConfig"
      , "  (solutions, trace) <- Language.SolverDebugger.debugSolve config rules query"
      , "  putStrLn \"\\n=== TRACE ===\""
      , "  putStrLn $ Language.SolverDebugger.prettyTrace trace"
      , "  putStrLn \"=== RESULTS ===\""
      , "  if null solutions"
      , "    then putStrLn \"No solutions found.\""
      , "    else do"
      , "      putStrLn $ \"Found \" ++ show (length solutions) ++ \" solution(s):\""
      , "      mapM_ (putStrLn . (\"  \" ++) . show) solutions"
      ]


------------------------------------------------------------
-- Monad
------------------------------------------------------------

type CodeGenM = ReaderT CheckingContext Q

------------------------------------------------------------
-- Haskell expression embedding
------------------------------------------------------------

-- | Checks whether the given variable is a term variable
isTermVariable :: String -> Bool
isTermVariable = List.isPrefixOf "_'"

-- | Extract the term variable name from the term variable 
-- e.g.,extractTermVariable "_'a" = "a"
extractTermVariable :: String -> String
extractTermVariable v = fromMaybe (error $ "could not extract term variable from " ++ show v) (stripPrefix "_'" v)

embedExpression :: String -> Typ -> CodeGenM Exp
embedExpression haskellCode expectedType = do
  ctx <- ask
  Reader.lift $ do
    -- Parse the Haskell expression string into a Template Haskell expression
    haskellExp <- either fail return (parseExp haskellCode)

    -- Extract free variables from the parsed expression
    let freeVarSet = Set.filter isTermVariable $ freeVars haskellExp
        freeVarList = Set.toList freeVarSet
        typingCtx = _typingContext ctx

    -- Generate the HaskellHatch data structure
    [| HaskellHatch $(lift haskellCode) $(lift (map extractTermVariable freeVarList)) Map.empty $(generateExecuteFunction haskellExp freeVarList typingCtx expectedType) |]

-- | Generate the execute function for a Haskell expression  
generateExecuteFunction :: Exp -> [String] -> TypingContext -> Typ -> Q Exp
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
    buildLet (_varName, Nothing) bodyExpr = [| $(return bodyExpr) |]


-- | Generate extraction code for a single variable
generateExtraction :: TypingContext -> Name -> String -> Q (Maybe Exp)
generateExtraction typingCtx mappingName varName = do
  runMaybeT $ do
      baseVarName <- MaybeT $ return $ safeVariableName varName
      sortName <- MaybeT $ return $ Map.lookup baseVarName typingCtx
      let typ = sortName

      case typ of
        -- Handle Haskell-defined types via TermHask
        HaskType haskType -> do
          let constructorName = mkName (getTermConstructorName haskType)
          Trans.lift [|
               maybe (Left $ UserError $ "Variable " ++ $(lift varName) ++ " not found")
                     (\case
                       TermValue {} -> Left $ UserError $ "Expected TermHask for Haskell type " ++ $(lift haskType) ++ " but got TermValue for variable " ++ $(lift varName)
                       TermHask haskellValue _ _ -> 
                         case haskellValue of
                           $(conP constructorName [varP (mkName "v")]) -> Right v
                           _ -> Left $ UserError $ "Expected " ++ $(lift (getTermConstructorName haskType)) ++ " constructor for variable " ++ $(lift varName) ++ " of type " ++ $(lift haskType) ++ ", but got different HaskellValue constructor"
                       t -> Left $ UserError $ "Expected TermHask for Haskell type variable " ++ $(lift varName) ++ " but got " ++ show t)
                     (Map.lookup $(lift (extractTermVariable varName)) $(varE mappingName))
           |]
        -- Handle built-in types via existing TermValue/asType mechanism  
        _ -> Trans.lift [|
               maybe (Left $ UserError $ "Variable " ++ $(lift varName) ++ " not found")
                     (\case
                       TermValue value _ _ -> maybe (Left $ InvalidTypePassed $(lift typ) (typeOf value))
                                                   Right
                                                   (asType $(typHaskEx typ) value)
                       _ -> Left $ UserError $ "Expected TermValue for variable " ++ $(lift varName))
                     (Map.lookup $(lift (extractTermVariable varName)) $(varE mappingName))
         |]


------------------------------------------------------------
-- Shared helper functions for Haskell type handling
------------------------------------------------------------

-- | Extract all Haskell types defined in the context via ${Type} syntax
-- For example: if context contains HaskType "CP Bool" and HaskType "Map String Int", 
-- this returns ["CP Bool", "Map String Int"]
getHaskellTypes :: CheckingContext -> [String]
getHaskellTypes ctx = mapMaybe (\case HaskType h -> Just h ; _ -> Nothing) $ Set.toList $ Map.keysSet $ _definedSorts ctx

-- | Sanitize Haskell type names for use as constructor names
-- To do so, it converts spaces to underscores.
-- Example: "CP Bool" -> "CP_Bool", "Map String Int" -> "Map_String_Int"
sanitizeHaskellTypeName :: String -> String  
sanitizeHaskellTypeName = map (\case ' ' -> '_' ; c -> c)

-- | Generate the constructor name for a Haskell type in the HaskellValue sum type
-- Example: "CP Bool" -> "Term_CP_Bool", "Map String Int" -> "Term_Map_String_Int"  
getTermConstructorName :: String -> String
getTermConstructorName haskType = "Term_" ++ sanitizeHaskellTypeName haskType

-- | Generate a data type for the defined Haskell expression types so that they can be used as term values
-- Creates a sum type like: data HaskellValue = Term_CP_Bool (CP Bool) | Term_Map_String_Int (Map String Int) | ... deriving (Show, Ord, Eq)
generateTermTypes :: CheckingContext -> Q Dec
generateTermTypes ctx = return $ DataD [] (mkName "HaskellValue") [] Nothing termConts [DerivClause Nothing [ConT (mkName "Show"), ConT (mkName "Ord"), ConT (mkName "Eq")]]
  where haskellTermTypes = getHaskellTypes ctx
        termTypes  = map (fromRight (error "could not parse type") . parseType) haskellTermTypes
        termNames  = map (mkName . getTermConstructorName) haskellTermTypes
        termConts  = zipWith (curry (uncurry NormalC . second (fmap (Bang NoSourceUnpackedness NoSourceStrictness,) . List.singleton))) termNames termTypes


-- | Generate code to wrap the result of a Haskell expression into a PureTerm
-- For built-in types (IntType, StrType, etc.), wraps in TermValue
-- For Haskell-defined types (HaskType), wraps in TermHask with appropriate constructor
wrapResult :: Exp -> Typ -> Name -> Q Exp
wrapResult haskellExp IntType proxName =
  [| TermValue (IntValue $(return haskellExp)) (typeAnnot $(varE proxName) IntType) dummyRange |]
wrapResult haskellExp StrType proxName =
  [| TermValue (StrValue $(return haskellExp)) (typeAnnot $(varE proxName) StrType) dummyRange |]
wrapResult haskellExp BooType proxName =
  [| TermValue (BooValue $(return haskellExp)) (typeAnnot $(varE proxName) BooType) dummyRange |]
wrapResult haskellExp (HaskType haskType) proxName = do
  let constructorName = mkName (getTermConstructorName haskType)
  [| TermHask ($(conE constructorName) $(return haskellExp)) 
             (typeAnnot $(varE proxName) (HaskType $(lift haskType))) 
             dummyRange |]
wrapResult e typ _ =
  fail $ "Unsupported result type: " ++ show typ ++ " for " ++ show e 

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
  Import filename range ->
    [| Import $(lift filename) $(rangeToExp range) |]

-- | Convert TypeCtor to Template Haskell expression
typeCtorToExp :: TypeCon -> Q Exp
typeCtorToExp = \case
  TypeApp t1 ts r -> [| TypeApp $(typeCtorToExp t1) $(listE $ map typeCtorToExp ts) $(rangeToExp r) |]
  TypeTyp nam r -> [| TypeTyp $(lift nam) $(rangeToExp r) |]
  TypeVar nam r -> [| TypeVar $(lift nam) $(rangeToExp r) |]
  TypeHas contents r -> [| TypeVar $(lift contents) $(rangeToExp r) |]

syntaxDeclToExp :: CheckingContext -> TypedSyntaxDecl -> Q Exp
syntaxDeclToExp ctx (SyntaxDecl vars tpy prods range) =
  [| SyntaxDecl $(lift vars) $(typeCtorToExp tpy) $(listE (map (pureTermToExp ctx) prods)) $(rangeToExp range) |]

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

  IncludedIn var term range ->
    [| IncludedIn $(lift var) $(pureTermToExp ctx term) $(rangeToExp range) |]

  SetOfTerms terms tpy range ->
    [| SetOfTerms (Set.fromList $(listE (map (pureTermToExp ctx) (Set.toList terms)))) $(lift tpy) $(rangeToExp range) |]

  TermHask v _ _ -> absurd v

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
codegen :: Bool -> CheckingContext -> TypedProgram -> IO String
codegen enableDebugger context prog@(Program _ comments) = runQ $ do
  let testQueryStrings = extractTestQueries comments
  makeModule
        enableDebugger
        (concat (haskellBlocks prog))
    <$> (pprint <$> astToCode context prog)
    <*> (pprint <$> (processTestQueries context testQueryStrings >>= listE . map return))
    <*> (pprint <$> generateTermTypes context)

