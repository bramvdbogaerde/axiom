{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Language Server Protocol implementation for analysislang
module LanguageServer
  ( runLanguageServer
  ) where

import Language.Parser (parseProgram)
import Language.TypeCheck (runChecker)
import qualified Language.AST
import LanguageServer.Diagnostics (errorToDiagnostic)
import LanguageServer.Symbols (extractDocumentSymbols, findDefinition)

import Language.LSP.Protocol.Message hiding (_code, _message)
import qualified Language.LSP.Protocol.Message as Messag
import Language.LSP.Protocol.Types (Position(..), Location, Uri(..), DocumentSymbol, Diagnostic (..), Range(..), DiagnosticSeverity (DiagnosticSeverity_Error), toNormalizedUri, fromNormalizedUri, type (|?) (InL))
import Language.LSP.Server
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.Either (either)
import Data.Maybe
import Control.Monad.Trans.Class
import Language.LSP.VFS (virtualFileText)
import Language.LSP.Protocol.Lens hiding (publishDiagnostics, options)
import Language.LSP.Diagnostics (partitionBySource)
import Control.Lens ((^.))

-------------------------------------------------------------
-- Language Server Entry Point
-------------------------------------------------------------

-- | Run the Language Server Protocol server
runLanguageServer :: IO Int
runLanguageServer = runServer $ ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "analysislang"
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = const handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }

-------------------------------------------------------------
-- LSP Handlers
-------------------------------------------------------------

-- | LSP message handlers
handlers :: Handlers (LspM ())
handlers = mconcat
  [ -- Handle document open/change events for diagnostics
    notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      let params' = msg ^. params
          uri' = params' ^. textDocument . uri
      handleDocumentChange uri'

  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
      let params' = msg ^. params
          uri' = params' ^. textDocument . uri
      handleDocumentChange uri'

    -- Handle document symbol requests
  -- , requestHandler SMethod_TextDocumentDocumentSymbol $ \req responder -> do
  --     let params' = req ^. params
  --         uri' = params' ^. textDocument . uri
  --     symbols <- handleDocumentSymbols uri'
  --     responder $ Right $ InL symbols

    -- Handle go-to-definition requests  
  -- , requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
  --     let params' = req ^. params
  --         uri' = params' ^. textDocument . uri
  --         pos = params' ^. position
  --     location <- handleGotoDefinition uri' pos
  --     responder $ Right $ InL $ maybeToList location
  ]

-------------------------------------------------------------
-- Document Change Handler
-------------------------------------------------------------

-- | Handle document open/change events and publish diagnostics
handleDocumentChange :: (MonadIO m, MonadLsp () m) => Uri -> m ()
handleDocumentChange uri = do
  vf <- fromJust <$> getVirtualFile (toNormalizedUri uri)
  let content = virtualFileText vf
  diagnostics <- liftIO $ case parseProgram (T.unpack content) of
    Left parseError -> return [parseErrorToDiagnostic parseError]
    Right ast -> case runChecker ast of
      Left typeError -> return [errorToDiagnostic typeError]
      Right _context -> return []

  publishDiagnostics 100 (toNormalizedUri uri) Nothing (partitionBySource diagnostics)

-- | Convert parse error to diagnostic
parseErrorToDiagnostic :: Show e => e -> Diagnostic
parseErrorToDiagnostic parseError = Diagnostic
  { _range = Range (Position 0 0) (Position 0 0)
  , _severity = Just DiagnosticSeverity_Error
  , _code = Nothing
  , _codeDescription = Nothing
  , _source = Just "analysislang-parser"
  , _message = T.pack (show parseError)
  , _tags = Nothing
  , _relatedInformation = Nothing
  , _data_ = Nothing
  }

-------------------------------------------------------------
-- Document Symbols Handler  
-------------------------------------------------------------

-- | Handle document symbol requests
handleDocumentSymbols :: (MonadIO m, MonadLsp () m) => Uri -> m [DocumentSymbol]
handleDocumentSymbols uri = do
  result <- runMaybeT $ do
    vf <- MaybeT $ getVirtualFile (toNormalizedUri uri)
    let content = virtualFileText vf
    program <- MaybeT $ return $ either (const Nothing) Just $ parseProgram (T.unpack content)
    context <- MaybeT $ return $ either (const Nothing) Just $ runChecker program
    return $ extractDocumentSymbols context
  return $ fromMaybe [] result

-------------------------------------------------------------
-- Go-to Definition Handler
-------------------------------------------------------------

-- | Handle go-to-definition requests
handleGotoDefinition :: (MonadIO m, MonadLsp () m) => Uri -> Position -> m (Maybe Location)
handleGotoDefinition uri pos = runMaybeT $ do
  vf <- MaybeT $ getVirtualFile (toNormalizedUri uri)
  let content = virtualFileText vf
  program <- MaybeT $ return $ either (const Nothing) Just $ parseProgram (T.unpack content)
  context <- MaybeT $ return $ either (const Nothing) Just $ runChecker program
  let symbolName = extractSymbolAtPosition content pos
  MaybeT $ return $ findDefinition context (lspPositionToASTPosition pos) symbolName

-------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------

-- | Extract symbol name at given position (simplified implementation)
extractSymbolAtPosition :: T.Text -> Position -> String
extractSymbolAtPosition _content _pos = "placeholder" -- TODO: Implement proper symbol extraction

-- | Convert LSP Position to AST Position
lspPositionToASTPosition :: Position -> Language.AST.Position
lspPositionToASTPosition (Position line char) =
  Language.AST.Position (fromIntegral line + 1) (fromIntegral char + 1) Nothing
