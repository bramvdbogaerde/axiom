{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for converting type checker errors to LSP diagnostics
module LanguageServer.Diagnostics
  ( errorToDiagnostic
  , rangeToDiagnosticRange
  , positionToDiagnosticPosition
  ) where

import Language.TypeCheck (Error(..))
import qualified Language.AST as AST
import Language.LSP.Protocol.Types (Diagnostic(..), DiagnosticSeverity(..), Range(..), Position(..))
import Language.Axiom.Reporting (formatModelError)
import qualified Data.Text as T

-------------------------------------------------------------
-- Error to Diagnostic Conversion
-------------------------------------------------------------

-- | Convert a type checker Error to an LSP Diagnostic
errorToDiagnostic :: Error -> Diagnostic
errorToDiagnostic (Error modelErr maybeRange _ctx) = Diagnostic
  { _range = maybe defaultRange rangeToDiagnosticRange maybeRange
  , _severity = Just DiagnosticSeverity_Error
  , _code = Nothing
  , _codeDescription = Nothing
  , _source = Just "analysislang-typechecker"
  , _message = T.pack (formatModelError modelErr)  -- Reuse Reporting module function
  , _relatedInformation = Nothing
  , _tags = Nothing
  , _data_ = Nothing
  }
  where
    -- Default range for errors without location information
    defaultRange = Range 
      { _start = Position { _line = 0, _character = 0 }
      , _end = Position { _line = 0, _character = 0 }
      }

-------------------------------------------------------------
-- Range Conversion
-------------------------------------------------------------

-- | Convert an AST Range to an LSP DiagnosticRange
rangeToDiagnosticRange :: AST.Range -> Range
rangeToDiagnosticRange (AST.Range startPos endPos) = Range
  { _start = positionToDiagnosticPosition startPos
  , _end = positionToDiagnosticPosition endPos
  }

-- | Convert an AST Position to an LSP DiagnosticPosition
positionToDiagnosticPosition :: AST.Position -> Position
positionToDiagnosticPosition (AST.Position line col _filename) = Position
  { _line = fromIntegral $ line - 1      -- LSP uses 0-based line numbers
  , _character = fromIntegral $ col - 1  -- LSP uses 0-based column numbers
  }
