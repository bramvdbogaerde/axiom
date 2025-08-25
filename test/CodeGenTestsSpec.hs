{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module CodeGenTestsSpec (spec) where

import Test.Hspec
import Language.AST
import Language.Parser (parseProgram)
import Language.TypeCheck (runChecker')
import Language.CodeGen (codegen)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import System.Process
import System.Exit
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (catMaybes, listToMaybe)
import Control.Exception (catch, IOException, finally)
import Control.Monad.Extra (ifM)
import Control.Monad (filterM)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | 30 second timeout
defaultTimeout :: Int
defaultTimeout = 30000000

-- | Find all .sem files in the tests directory
findSemFiles :: IO [FilePath]
findSemFiles = do
  let testsDir = "tests"
  ifM (doesDirectoryExist testsDir)
    (do files <- listDirectory testsDir
        return $ map (testsDir </>) $ filter (\f -> takeExtension f == ".sem") files)
    (return [])

-- | Check if a filename indicates it should fail (and thus not be tested for codegen)
shouldSkipFile :: FilePath -> Bool
shouldSkipFile path = "_fail_" `isPrefixOf` takeBaseName path

-- | Read file content safely
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
  catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either String String)
    handler e = return $ Left $ "Could not read file " ++ path ++ ": " ++ show e

-- | Extract codegen test queries from comments that start with "codegen_test:" or "codegen_fail_test:"
extractCodegenTestQueries :: [Comment] -> [(String, Bool)]  -- (query, shouldPass)
extractCodegenTestQueries comments = catMaybes $ map extractQuery comments
  where
    extractQuery (Comment content _) = 
      let prefixes = [(" codegen_test: ", True), ("codegen_test: ", True), 
                      (" codegen_fail_test: ", False), ("codegen_fail_test: ", False)]
          tryPrefix (prefix, shouldPass) = fmap (,shouldPass) $ stripPrefix prefix content
      in foldr (<|>) Nothing $ map tryPrefix prefixes

-- | Parse the output from generated code execution
parseTestOutput :: String -> Either String (Int, Int, Int)  -- (passed, total, failed)
parseTestOutput output = do
  ls <- maybe (Left "Empty output") Right $ nonEmpty $ lines output
  let resultLines = filter ("Results: " `isPrefixOf`) ls
  line <- maybe (Left "No results line found in output") Right $ listToMaybe resultLines
  parseResultLine line
  where
    nonEmpty [] = Nothing
    nonEmpty xs = Just xs
    
    parseResultLine line =
      let parts = words line
      in case parts of
        ["Results:", passedStr, "passed"] -> 
          maybe (Left $ "Could not parse passed/total: " ++ passedStr)
                (\(passed, total) -> Right (passed, total, total - passed))
                (readPassedTotal passedStr)
        _ -> Left $ "Unexpected results format: " ++ line
    
    readPassedTotal str =
      case break (== '/') str of
        (passedStr, '/':totalStr) -> (,) <$> readMaybe passedStr <*> readMaybe totalStr
        _ -> Nothing

-- | Execute generated Haskell code and check results
executeGeneratedCode :: String -> IO (Either String (Int, Int, Int))
executeGeneratedCode generatedCode = do
  withSystemTempFile "codegen_test.hs" $ \tempFilePath tempHandle -> do
    -- Write generated code to temp file
    hPutStr tempHandle generatedCode
    hFlush tempHandle
    hClose tempHandle
    
    -- Execute with timeout (30 seconds)
    let cmd = "cabal"
    let args = ["exec", "--", "runghc", "--ghc-arg=-package", "--ghc-arg=analysislang", tempFilePath]
    
    timeoutResult <- timeout defaultTimeout $ readProcessWithExitCode cmd args "" 
    case timeoutResult of
      Nothing -> return $ Left "Execution timeout: Generated code did not complete within 30 seconds"
      Just (exitCode, stdout, stderr) -> 
        case exitCode of
          ExitSuccess -> return $ parseTestOutput stdout
          ExitFailure code -> return $ Left $ "Execution failed with code " ++ show code ++ 
                                             "\nStdout: " ++ stdout ++ 
                                             "\nStderr: " ++ stderr

-- | Load and parse a test file, returning either an error or (program, queries)
loadCodegenTestFile :: FilePath -> ExceptT String IO (Program, [(String, Bool)])
loadCodegenTestFile filePath = do
  content <- ExceptT $ readFileSafe filePath
  program@(Program _ comments) <- ExceptT $ return $ first show $ parseProgram content
  let queries = extractCodegenTestQueries comments
  return (program, queries)

-- | Generate and test code for a single file
testCodegenFile :: FilePath -> IO (Either String (Int, Int, Int))
testCodegenFile filePath = runExceptT $ do
  (program, queries) <- loadCodegenTestFile filePath
  
  if null queries
    then return (0, 0, 0) -- No tests to run
    else do
      -- Type check the program
      (context, typedProgram) <- ExceptT $ return $ first show $ runChecker' program
      
      -- Generate code and execute
      ExceptT $ codegen context typedProgram >>= executeGeneratedCode

-- | Create a test for a single file
createCodegenTest :: FilePath -> Spec
createCodegenTest filePath =
  describe ("Testing " ++ takeFileName filePath) $ do
    it "should generate and execute code successfully" $ do
      result <- testCodegenFile filePath
      case result of
        Left err -> expectationFailure $ "Error: " ++ err
        Right (passed, total, failed) -> do
          if failed == 0
            then passed `shouldBe` total
            else expectationFailure $ 
                   "Code generation tests failed: " ++ show failed ++ "/" ++ show total ++ 
                   " tests failed (passed: " ++ show passed ++ ")"

spec :: Spec
spec = describe "Code generation tests" $ do
  semFiles <- runIO findSemFiles
  let validFiles = filter (not . shouldSkipFile) semFiles
  
  -- Filter to only files that have codegen tests
  filesWithCodegenTests <- runIO $ do
    filesWithTests <- filterM hasCodegenTests validFiles
    return filesWithTests
  
  if null filesWithCodegenTests
    then it "should find .sem files with codegen tests" $ do
      expectationFailure "No .sem files with %codegen_test: comments found in tests directory"
    else describe "Code generation from .sem files" $ do
      mapM_ createCodegenTest filesWithCodegenTests

-- | Check if a file has codegen test queries
hasCodegenTests :: FilePath -> IO Bool
hasCodegenTests filePath = do
  either (const False) (not . null) <$> (runExceptT $ loadCodegenTestFile filePath)
