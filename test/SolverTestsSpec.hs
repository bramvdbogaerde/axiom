{-# LANGUAGE TypeApplications #-}
module SolverTestsSpec (spec) where

import Test.Hspec
import Language.AST
import Language.Parser (parseTerm)
import Language.ImportResolver
import Language.Solver
import Language.TypeCheck (runChecker', CheckingContext(..))
import qualified Language.Solver.BacktrackingST as ST
import System.Directory
import System.FilePath
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Except
import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import System.Timeout (timeout)

-- | Find all .sem files in the tests directory
findSemFiles :: IO [FilePath]
findSemFiles = do
  let testsDir = "tests"
  ifM (doesDirectoryExist testsDir)
    (do files <- listDirectory testsDir
        return $ map (testsDir </>) $ filter (\f -> takeExtension f == ".sem") files)
    (return [])

-- | Check if a filename indicates it should fail (and thus not be tested for solver)
shouldSkipFile :: FilePath -> Bool
shouldSkipFile path = "_fail_" `isPrefixOf` takeBaseName path


-- | Extract test queries from comments that start with "test:" or "test_fail:"
extractTestQueries :: [Comment] -> [(String, Bool)]  -- (query, shouldPass)
extractTestQueries comments = catMaybes $ map extractQuery comments
  where
    extractQuery (Comment content _) =
      let prefixes = [(" test: ", True), ("test: ", True),
                      (" test_fail: ", False), ("test_fail: ", False)]
          tryPrefix (prefix, shouldPass) = ((,shouldPass) <$> stripPrefix prefix content)
      in foldr (<|>) Nothing $ map tryPrefix prefixes

-- | Parse and run a single test query against a program with timeout protection
runTestQuery :: Program -> String -> IO (Either String Bool)
runTestQuery (Program decls _) queryStr = do
  case parseTerm queryStr of
    Left parseError -> return $ Left $ "Parse error: " ++ show parseError
    Right query -> do
      -- First type check the program to get the subtyping graph
      case runChecker' (Program decls []) of
        Left typeError -> return $ Left $ "Type error: " ++ show typeError
        Right (checkingCtx, typedProgram) -> do
          let rules = [rule | RulesDecl rules _ <- getDecls typedProgram, rule <- rules]
          let rewrites = [rewrite | Rewrite rewrite _ <- getDecls typedProgram]
          let subtyping = _subtypingGraph checkingCtx
          let typedQuery = anyTyped query
          let engineCtx = fromRules subtyping rules rewrites :: EngineCtx TypingPhase [] s
          let solverComputation = ST.runST $ runSolver engineCtx (solve @TypingPhase typedQuery)
          
          -- Run with 5 second timeout to catch non-termination
          timeoutResult <- timeout 5000000 (return $! solverComputation) -- 5 seconds in microseconds
          case timeoutResult of
            Nothing -> return $ Left $ "Solver timeout: Query '" ++ queryStr ++ "' did not terminate within 5 seconds"
            Just solutions -> return $ Right (not $ null solutions)

-- | Load and parse a test file, returning either an error or (program, queries)
loadTestFile :: FilePath -> ExceptT String IO (Program, [(String, Bool)])
loadTestFile filePath = do
  importResult <- ExceptT $ first show <$> resolveImportsFromFile filePath
  let program@(Program _ comments) = importResult
  let queries = extractTestQueries comments
  return (program, queries)

-- | Create a test for a single file
createSolverTest :: FilePath -> Spec
createSolverTest filePath =
  describe ("Testing " ++ takeFileName filePath) $ do
    result <- runIO $ runExceptT $ loadTestFile filePath
    case result of
      Left err ->
        it "should load and parse successfully" $
          expectationFailure err
      Right (program, queries) -> do
        if null queries
          then it "has test queries" $ pendingWith $ "No test queries found"
          else mapM_ (createQueryTest program) queries

-- | Create a test for a single query
createQueryTest :: Program -> (String, Bool) -> Spec
createQueryTest program (queryStr, shouldPass) =
  it (testDescription ++ queryStr) $ do
    result <- runTestQuery program queryStr
    case result of
      Left err | "Solver timeout:" `isPrefixOf` err ->
        expectationFailure $ "SOLVER NON-TERMINATION DETECTED: " ++ err
      Left err ->
        expectationFailure $ "Error testing query: " ++ err
      Right hasSolution ->
        if shouldPass
          then hasSolution `shouldBe` True
          else hasSolution `shouldBe` False
  where
    testDescription = if shouldPass then "should solve: " else "should fail: "

spec :: Spec
spec = describe "Solver tests" $ do
  semFiles <- runIO findSemFiles
  let validFiles = filter (not . shouldSkipFile) semFiles

  if null validFiles
    then it "should find .sem files in tests directory" $ do
      expectationFailure "No valid .sem files found in tests directory"
    else describe "Test queries from .sem files" $ do
      mapM_ createSolverTest validFiles
