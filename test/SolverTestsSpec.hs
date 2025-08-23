{-# LANGUAGE TypeApplications #-}
module SolverTestsSpec (spec) where

import Test.Hspec
import Language.AST
import Language.Parser (parseProgram, parseTerm)
import Language.Solver
import qualified Language.Solver.BacktrackingST as ST
import System.Directory
import System.FilePath
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes)
import Control.Exception (catch, IOException)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Except
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

-- | Read file content safely
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
  catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either String String)
    handler e = return $ Left $ "Could not read file " ++ path ++ ": " ++ show e

-- | Extract test queries from comments that start with "test:"
extractTestQueries :: [Comment] -> [String]
extractTestQueries comments = catMaybes $ map extractQuery comments
  where
    extractQuery (Comment content _) = 
      stripPrefix "test: " content

-- | Parse and run a single test query against a program with timeout protection
runTestQuery :: Program -> String -> IO (Either String Bool)
runTestQuery program@(Program decls _) queryStr = do
  case parseTerm queryStr of
    Left parseError -> return $ Left $ "Parse error: " ++ show parseError
    Right query -> do
      let rules = [rule | RulesDecl rules _ <- decls, rule <- rules]
      let engineCtx = fromRules rules :: EngineCtx p [] s
      let solverComputation = ST.runST $ runSolver engineCtx (solve @ParsePhase query)
      
      -- Run with 5 second timeout to catch non-termination
      timeoutResult <- timeout 5000000 (return $! solverComputation) -- 5 seconds in microseconds
      case timeoutResult of
        Nothing -> return $ Left $ "Solver timeout: Query '" ++ queryStr ++ "' did not terminate within 5 seconds"
        Just solutions -> return $ Right (not $ null solutions)

-- | Load and parse a test file, returning either an error or (program, queries)
loadTestFile :: FilePath -> ExceptT String IO (Program, [String])
loadTestFile filePath = do
  content <- ExceptT $ readFileSafe filePath
  program@(Program _ comments) <- ExceptT $ return $ first show $ parseProgram content
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
createQueryTest :: Program -> String -> Spec
createQueryTest program queryStr = 
  it ("should solve: " ++ queryStr) $ do
    result <- runTestQuery program queryStr
    case result of
      Left err | "Solver timeout:" `isPrefixOf` err -> 
        expectationFailure $ "SOLVER NON-TERMINATION DETECTED: " ++ err
      Left err -> 
        expectationFailure $ "Error testing query: " ++ err
      Right hasSolution -> 
        hasSolution `shouldBe` True

spec :: Spec
spec = describe "Solver tests" $ do
  semFiles <- runIO findSemFiles
  let validFiles = filter (not . shouldSkipFile) semFiles
  
  if null validFiles
    then it "should find .sem files in tests directory" $ do
      expectationFailure "No valid .sem files found in tests directory"
    else describe "Test queries from .sem files" $ do
      mapM_ createSolverTest validFiles
