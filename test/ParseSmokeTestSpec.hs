module ParseSmokeTestSpec where

import Test.Hspec
import Language.ImportResolver
import System.Directory
import System.FilePath
import Control.Exception (catch, IOException)

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

-- | Read file content safely
readFileSafe :: FilePath -> IO (Either String String)
readFileSafe path = do
    catch (Right <$> readFile path) handler
  where
    handler :: IOException -> IO (Either String String)
    handler e = return $ Left $ "Could not read file " ++ path ++ ": " ++ show e

spec :: Spec
spec = describe "Parse smoke tests" $ do
  semFiles <- runIO findSemFiles
  if null semFiles
    then it "should find .sem files in tests directory" $ do
      expectationFailure "No .sem files found in tests directory"
    else mapM_ createParseTest semFiles

createParseTest :: FilePath -> Spec
createParseTest filePath = 
  it ("should parse " ++ takeFileName filePath) $ do
    importResult <- resolveImportsFromFile filePath
    case importResult of
      Left importErr -> expectationFailure $ "Parse error: " ++ show importErr
      Right _ -> return () -- Success

-- | Test parsing a single file
testParseFile :: FilePath -> IO (FilePath, Bool)
testParseFile filePath = do
  importResult <- resolveImportsFromFile filePath
  case importResult of
    Left importErr -> do
      putStrLn $ "Parse error in " ++ filePath ++ ": " ++ show importErr
      return (filePath, False)
    Right _ -> do
      putStrLn $ "Successfully parsed " ++ filePath
      return (filePath, True)
