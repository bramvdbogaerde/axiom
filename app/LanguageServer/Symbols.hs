{-# LANGUAGE OverloadedStrings #-}
-- | Module for extracting and providing symbol information from the type checker context
module LanguageServer.Symbols
  ( extractDocumentSymbols
  , findDefinition
  ) where

import Language.TypeCheck (CheckingContext(..), getGamma)
import Language.Types (Typ(..), toSortName)
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
extractDocumentSymbols :: CheckingContext -> [DocumentSymbol]
extractDocumentSymbols ctx =
  let sortSymbols = extractSortSymbols ctx
      variableSymbols = extractVariableSymbols ctx
  in sortSymbols ++ variableSymbols

-- | Extract sort definitions as document symbols
extractSortSymbols :: CheckingContext -> [DocumentSymbol]
extractSortSymbols ctx = mapMaybe sortToSymbol (Set.toList (_definedSorts ctx))
  where
    sortToSymbol :: Typ -> Maybe DocumentSymbol
    sortToSymbol sortName = do
      -- Get the definition range from sortToDefSite
      let sortNameStr = toSortName sortName
      range <- Map.lookup sortNameStr (_sortToDefSite ctx)
      return DocumentSymbol
        { _name = T.pack sortNameStr
        , _detail = Just $ T.pack $ "Sort " ++ sortNameStr
        , _kind = SymbolKind_Class -- Use Class symbol kind for sorts
        , _tags = Nothing
        , _deprecated = Nothing
        , _range = rangeToDiagnosticRange range
        , _selectionRange = rangeToDiagnosticRange range
        , _children = Nothing
        }

-- | Extract variable definitions as document symbols
extractVariableSymbols :: CheckingContext -> [DocumentSymbol]
extractVariableSymbols ctx =
  map variableToSymbol (Map.toList (getGamma (_typingContext ctx)))
  where
    variableToSymbol :: (String, Typ) -> DocumentSymbol
    variableToSymbol (varName, sortName) = DocumentSymbol
      { _name = T.pack varName
      , _detail = Just $ "Variable of sort " <> T.pack (toSortName sortName)
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
findDefinition :: CheckingContext -> AST.Position -> String -> Maybe Location
findDefinition ctx _pos symbolName =
  findSortDefinition ctx symbolName <|> findVariableDefinition ctx symbolName

-- | Find definition of a sort by name
findSortDefinition :: CheckingContext -> String -> Maybe Location
findSortDefinition ctx sortName = do
  range <- Map.lookup sortName (_sortToDefSite ctx)
  let startPos = AST.rangeStart range
  let uri = case AST.filename startPos of
        Just fname -> "file://" <> T.pack fname
        Nothing -> "file:///unknown"
  return $ Location (Uri uri) (rangeToDiagnosticRange range)

-- | Find definition of a variable by name (returns sort definition)
findVariableDefinition :: CheckingContext -> String -> Maybe Location
findVariableDefinition ctx varName = do
  sortName <- Map.lookup varName (getGamma (_typingContext ctx))
  findSortDefinition ctx (toSortName sortName)
