{-# LANGUAGE OverloadedStrings #-}
-- | Module for extracting and providing symbol information from the type checker context
module LanguageServer.Symbols
  ( extractDocumentSymbols
  , findDefinition
  ) where

import Language.TypeCheck (Context(..), Sort(..), SortName(..))
import qualified Language.AST as AST
import Language.LSP.Protocol.Types
  ( DocumentSymbol(..)
  , SymbolKind(..)
  , Location(Location)
  , Range(..)
  , Position(..), Uri (Uri)
  )
import LanguageServer.Diagnostics (rangeToDiagnosticRange)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Applicative

-------------------------------------------------------------
-- Document Symbol Extraction
-------------------------------------------------------------

-- | Extract all document symbols from a type checker context
extractDocumentSymbols :: Context -> [DocumentSymbol]
extractDocumentSymbols ctx =
  let sortSymbols = extractSortSymbols ctx
      variableSymbols = extractVariableSymbols ctx
  in sortSymbols ++ variableSymbols

-- | Extract sort definitions as document symbols
extractSortSymbols :: Context -> [DocumentSymbol]
extractSortSymbols ctx = mapMaybe sortToSymbol (Map.toList (_sorts ctx))
  where
    sortToSymbol :: (String, Sort) -> Maybe DocumentSymbol
    sortToSymbol (sortName, sort@(Sort _ vars ctors)) = do
      -- Get the definition range from sortToDefSite
      maybeRange <- Map.lookup sortName (_sortToDefSite ctx)
      range <- maybeRange
      return DocumentSymbol
        { _name = T.pack sortName
        , _detail = Just $ T.pack $ formatSortDetail sort
        , _kind = SymbolKind_Class -- Use Class symbol kind for sorts
        , _tags = Nothing
        , _deprecated = Nothing
        , _range = rangeToDiagnosticRange range
        , _selectionRange = rangeToDiagnosticRange range
        , _children = Nothing
        }

    formatSortDetail :: Sort -> String
    formatSortDetail (Sort name vars ctors) =
      "Sort with " ++ show (Set.size vars) ++ " variables and " ++
      show (Set.size ctors) ++ " constructors"

-- | Extract variable definitions as document symbols
extractVariableSymbols :: Context -> [DocumentSymbol]
extractVariableSymbols ctx =
  map variableToSymbol (Map.toList (_atomToSorts ctx))
  where
    variableToSymbol :: (String, SortName) -> DocumentSymbol
    variableToSymbol (varName, sortName) = DocumentSymbol
      { _name = T.pack varName
      , _detail = Just $ "Variable of sort " <> T.pack (getSortName sortName)
      , _kind = SymbolKind_Variable
      , _deprecated = Nothing
      , _tags = Nothing
      , _range = defaultRange  -- Variables don't have explicit ranges in current implementation
      , _selectionRange = defaultRange
      , _children = Nothing
      }

    -- Default range when no specific range is available
    defaultRange = Range
      { _start = Position { _line = 0, _character = 0 }
      , _end = Position { _line = 0, _character = 0 }
      }

-------------------------------------------------------------
-- Go-to Definition Support
-------------------------------------------------------------

-- | Find the definition location of a symbol at the given position
findDefinition :: Context -> AST.Position -> String -> Maybe Location
findDefinition ctx _pos symbolName =
  findSortDefinition ctx symbolName <|> findVariableDefinition ctx symbolName

-- | Find definition of a sort by name
findSortDefinition :: Context -> String -> Maybe Location
findSortDefinition ctx sortName = do
  maybeRange <- Map.lookup sortName (_sortToDefSite ctx)
  range <- maybeRange
  let startPos = AST.rangeStart range
  let uri = case AST.filename startPos of
        Just fname -> "file://" <> T.pack fname
        Nothing -> "file:///unknown"
  return $ Location (Uri uri) (rangeToDiagnosticRange range)

-- | Find definition of a variable by name (returns sort definition)
findVariableDefinition :: Context -> String -> Maybe Location
findVariableDefinition ctx varName = do
  sortName <- Map.lookup varName (_atomToSorts ctx)
  findSortDefinition ctx (getSortName sortName)
