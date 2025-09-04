module CheckSmokeTestSpec where

import Test.Hspec
import Language.TypeCheck
import Language.ImportResolver
import System.Directory
import System.FilePath
import Data.List (isPrefixOf)
import Control.Monad (unless)

-- | Find all .sem files in the tests directory
findSemFiles :: IO [FilePath]
findSemFiles = do
  let testsDir = "tests"
  exists <- doesDirectoryExist testsDir
  if exists
    then do
      files <- listDirectory testsDir
      return $ map (testsDir </>) $ filter (\f -> takeExtension f == ".sem") files
    else return []

-- | Check if a filename indicates it should fail type checking
shouldFailTypeCheck :: FilePath -> Bool
shouldFailTypeCheck path = "_fail_" `isPrefixOf` takeBaseName path


-- | Test type checking a single file
testTypeCheckFile :: FilePath -> IO (FilePath, Bool, Bool)
testTypeCheckFile filePath = do
  importResult <- resolveImportsFromFile filePath
  case importResult of
    Left importErr -> do
      putStrLn $ "Import error in " ++ filePath ++ ": " ++ show importErr
      return (filePath, False, False) -- (file, parseSuccess, typeCheckSuccess)
    Right program -> do
      case runChecker program of
        Left typeErr -> do
          putStrLn $ "Type check error in " ++ filePath ++ ": " ++ show typeErr
          return (filePath, True, False)
        Right _ -> do
          putStrLn $ "Successfully type checked " ++ filePath
          return (filePath, True, True)

spec :: Spec
spec = describe "Type checking smoke tests" $ do
  semFiles <- runIO findSemFiles
  let nonFailingFiles = filter (not . shouldFailTypeCheck) semFiles
  let failingFiles = filter shouldFailTypeCheck semFiles

  if null semFiles
    then it "should find .sem files in tests directory" $ do
      expectationFailure "No .sem files found in tests directory"
    else do
      -- Test non-failing files
      unless (null nonFailingFiles) $ do
        describe "Non-failing files should type check successfully" $ do
          mapM_ createTypeCheckTest nonFailingFiles

      -- Test failing files  
      unless (null failingFiles) $ do
        describe "Files marked as _fail_ should fail type checking" $ do
          mapM_ createFailingTypeCheckTest failingFiles

createTypeCheckTest :: FilePath -> Spec
createTypeCheckTest filePath =
  it ("should type check " ++ takeFileName filePath) $ do
    importResult <- resolveImportsFromFile filePath
    case importResult of
      Left importErr -> expectationFailure $ "Import error: " ++ show importErr
      Right program -> do
        case runChecker program of
          Left typeErr -> expectationFailure $ "Type check error: " ++ show typeErr ++ " for " ++ show program
          Right _ -> return () -- Success

createFailingTypeCheckTest :: FilePath -> Spec
createFailingTypeCheckTest filePath =
  it ("should fail to type check " ++ takeFileName filePath) $ do
    importResult <- resolveImportsFromFile filePath
    case importResult of
      Left importErr -> expectationFailure $ "Import error (failing test files should import): " ++ show importErr
      Right program -> do
        case runChecker program of
          Left _ -> return () -- Expected failure - success!
          Right _ -> expectationFailure "Expected type checking to fail, but it succeeded"
