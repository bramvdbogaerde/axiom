module Latex.Entrypoint (runLatexCommand, latexOptionsParser) where

import System.FilePath ((</>), (<.>))
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map as Map
import Text.Printf
import Options.Applicative

import Language.ImportResolver (resolveImportsFromFile, concatModules)
import Latex.Generator (runGenerator)
import Latex.Output (getBlocks, LatexType(..))

-- | Options for the latex command
data LatexOptions = LatexOptions
  { latexInputFile :: String
  , latexOutputDir :: String
  } deriving Show

-- | Parser for latex command options
latexOptionsParser :: Parser LatexOptions
latexOptionsParser = LatexOptions
  <$> strArgument
      ( metavar "FILE"
     <> help "Input file to process" )
  <*> strArgument
      ( metavar "OUTPUT_DIR"
     <> help "Output directory for generated LaTeX files" )

-- | Generate the LaTeX preamble for a file
latexPreamble :: LatexType -> String
latexPreamble latexType = unlines
  [ "\\begin{" ++ env ++ "}"
  ]
  where
    env = case latexType of
      Syntax -> "align*"
      Rules -> "mathpar"
      Rewrites -> "mathpar"

-- | Generate the LaTeX postamble for a file
latexPostamble :: LatexType -> String
latexPostamble latexType = unlines
  [ "\\end{" ++ env ++ "}"  ]
  where
    env = case latexType of
      Syntax -> "align*"
      Rules -> "mathpar"
      Rewrites -> "mathpar"

-- | Write a single LaTeX file
writeLatexFile :: FilePath -> String -> LatexType -> String -> IO ()
writeLatexFile outDir name latexType content = do
  let filename = outDir </> name <.> "tex"
  let fullContent = latexPreamble latexType ++ content ++ "\n" ++ latexPostamble latexType
  writeFile filename fullContent
  putStrLn $ "Written: " ++ filename

-- | Execute the latex generation command
runLatexCommand :: LatexOptions -> IO ()
runLatexCommand (LatexOptions inputFile outputDir) = do
  -- Parse the input file
  (modules, result) <- resolveImportsFromFile inputFile
  prog <- case result of
    Left err -> Reporting.printImportError modules err 
    Right p -> pure p

  -- Generate LaTeX output
  let latexOutput = runGenerator (concatModules prog)

  -- Create output directory if it doesn't exist
  createDirectoryIfMissing True outputDir

  -- Write each block to a separate file
  let blocks = getBlocks latexOutput
  mapM_ (\((name, latexType), content) ->
          writeLatexFile outputDir name latexType content)
        (Map.toList blocks)

  putStrLn $ printf "Generated %d LaTeX files in %s" (Map.size blocks) outputDir
