{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}

module Language.ImportResolver (
    resolveImports,
    resolveImportsFromFile,
    ImportError(..),
    ModuleInfo(..)
  ) where

import Language.AST
import Language.Parser (parseProgram, Error(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph (Graph, empty, addEdge, addNode, topSort)
import Data.Maybe (mapMaybe)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Lens
import Control.Exception (try, SomeException)
import System.FilePath (takeDirectory, (</>), normalise)
import System.Environment (lookupEnv)
import System.Directory (getXdgDirectory, XdgDirectory(..))
import Data.List (stripPrefix)
import Prelude hiding (readFile)
import System.IO (readFile)


-- | Information about a parsed module
data ModuleInfo = ModuleInfo
  { moduleFilePath :: FilePath
  , moduleProgram :: Program
  } deriving (Show, Eq)

-- | Import resolution errors
data ImportError
  = CyclicImport [FilePath]
  | FileNotFound FilePath
  | ParseError FilePath Error
  | InvalidImportPath FilePath
  deriving (Show, Eq)

-- | State for import resolution
data ImportState = ImportState
  { _visitedFiles :: Set.Set FilePath
  , _moduleMap :: Map.Map FilePath ModuleInfo
  , _dependencyGraph :: Graph FilePath () 
  } deriving (Show)

-- Generate lenses
makeLenses ''ImportState

type ImportM = ExceptT ImportError (ReaderT FilePath (StateT ImportState IO))

-- | Resolve import paths, handling special $XXX prefixes
resolveImportPath :: FilePath -> FilePath -> ImportM FilePath
resolveImportPath baseDir importPath
  | Just stdPath <- stripPrefix "$std/" importPath = liftIO $ do
      -- Check for ANL_STD_LIB environment variable first
      maybeCustomStdLib <- lookupEnv "ANL_STD_LIB"
      case maybeCustomStdLib of
        Just customPath -> return $ normalise (customPath </> stdPath)
        Nothing -> do
          -- Use XDG_DATA_HOME/.analysislang/std/ as default
          xdgDataHome <- getXdgDirectory XdgData ".analysislang/std"
          return $ normalise (xdgDataHome </> stdPath)
  | otherwise =
      -- Regular relative path resolution
      return $ normalise (baseDir </> importPath)

-- | Main function to resolve all imports starting from a root module
resolveImports :: ModuleInfo -> IO (Either ImportError Program)
resolveImports rootModule@ModuleInfo{..} = do
  let initialState = ImportState
        Set.empty
        (Map.singleton moduleFilePath rootModule)  -- Include root in module map
        (addNode moduleFilePath empty)  -- Add root node to graph
      initialPath = takeDirectory moduleFilePath
  evalStateT (runReaderT (runExceptT (resolveImportsM rootModule)) initialPath) initialState

-- | Entrypoint that loads and resolves imports from a file path
-- Returns Either ImportError Program where the Program is the concatenated result
resolveImportsFromFile :: FilePath -> IO (Either ImportError Program)
resolveImportsFromFile filePath = do
  either (const $ return $ Left (FileNotFound filePath))
         (either (return . Left . ParseError filePath)
                            (createModuleAndResolve filePath) . parseProgram)
    =<< try @SomeException (readFile filePath)
  where
    createModuleAndResolve :: FilePath -> Program -> IO (Either ImportError Program)
    createModuleAndResolve path program = do
      resolveImports (ModuleInfo path program)

-- | Resolve imports starting from a root module
resolveImportsM :: ModuleInfo -> ImportM Program
resolveImportsM rootModule = do
  -- Collect all modules recursively
  collectModulesRecursively rootModule

  -- Perform topological sort
  graph <- use dependencyGraph
  modules <- use moduleMap
  sorted <- topologicalSortWithGraph graph modules

  -- Concatenate all programs in topological order
  return $ concatenatePrograms (reverse sorted)

-- | Extract import file paths from a program
extractImports :: Program -> ImportM [FilePath]
extractImports (Program decls _) = do
  currentDir <- ask
  mapM (resolveImportPath currentDir) (mapMaybe getImportFilename decls)
  where
    getImportFilename :: Decl -> Maybe FilePath
    getImportFilename = \case
      Import filename _ -> Just filename
      _ -> Nothing

-- | Recursively collect all modules and their dependencies
collectModulesRecursively :: ModuleInfo -> ImportM ()
collectModulesRecursively modInfo@ModuleInfo{..} = do
  -- Check if we've already visited this file
  visited <- use visitedFiles
  if Set.member moduleFilePath visited
    then return ()  -- Already processed
    else do
      -- Mark as visited
      visitedFiles %= Set.insert moduleFilePath

      -- Store this module
      moduleMap %= Map.insert moduleFilePath modInfo

      -- Add this module as a node to the dependency graph
      dependencyGraph %= addNode moduleFilePath

      -- Extract dependencies dynamically from the program
      dependencies <- extractImports moduleProgram

      -- Add edges for all dependencies to the graph
      mapM_ (addDependencyEdge moduleFilePath) dependencies

      -- Process each dependency
      mapM_ processDependency dependencies
  where
    -- Add a dependency edge to the graph
    addDependencyEdge :: FilePath -> FilePath -> ImportM ()
    addDependencyEdge from to =
      dependencyGraph %= addEdge () from to
    processDependency :: FilePath -> ImportM ()
    processDependency depPath = do
      -- Parse and process the dependency in the context of its directory
      depProgram <- loadAndParseFile depPath
      local (const $ takeDirectory depPath) $
        collectModulesRecursively (ModuleInfo depPath depProgram)

-- | Load and parse a file
loadAndParseFile :: FilePath -> ImportM Program
loadAndParseFile filePath = do
  result <- liftIO $ try @SomeException (readFile filePath)
  content <- either (const $ throwError (FileNotFound filePath)) return result
  either (throwError . ParseError filePath) return (parseProgram content)

-- | Perform topological sort using the incrementally built graph
topologicalSortWithGraph :: Graph FilePath () -> Map.Map FilePath ModuleInfo -> ImportM [ModuleInfo]
topologicalSortWithGraph graph moduleMap =
  maybe (throwError $ CyclicImport (Map.keys moduleMap))
        (return . mapMaybe (`Map.lookup` moduleMap))
        (topSort graph)

-- | Concatenate programs in dependency order, removing import statements
concatenatePrograms :: [ModuleInfo] -> Program
concatenatePrograms modules =
  let allDecls = concatMap (getDecls . moduleProgram) modules
      allComments = concatMap (getComments . moduleProgram) modules
  in Program allDecls allComments

