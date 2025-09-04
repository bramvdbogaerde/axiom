{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Language Server Protocol implementation for analysislang
module LanguageServer
  ( runLanguageServer
  ) where

import Language.Parser ( parseProgram, Error(parseErrorPosition) )
import Language.TypeCheck (runChecker)
import qualified Language.AST
import LanguageServer.Diagnostics (errorToDiagnostic, rangeToDiagnosticRange)
import LanguageServer.Symbols (extractDocumentSymbols, findDefinition)

import Language.LSP.Protocol.Message hiding (ResponseError(..), TResponseError(..))
import Language.LSP.Protocol.Types (Position(..), Location, Uri(..), DocumentSymbol, Diagnostic (..), DiagnosticSeverity (DiagnosticSeverity_Error), toNormalizedUri, TextDocumentSyncOptions (..), TextDocumentSyncKind (TextDocumentSyncKind_Full))
import Language.LSP.Server
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.Maybe
import Language.LSP.VFS (virtualFileText)
import Language.LSP.Protocol.Lens hiding (publishDiagnostics, options)
import Language.LSP.Diagnostics (partitionBySource)
import Control.Lens ((^.))
import System.IO
import qualified Language.AST as AST

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
  , options = defaultOptions { optTextDocumentSync = Just $ TextDocumentSyncOptions
                                                          {  _openClose = Nothing
                                                           , _change    = Just TextDocumentSyncKind_Full
                                                           , _willSave  = Nothing
                                                           , _willSaveWaitUntil = Nothing
                                                           , _save = Nothing } } }

-------------------------------------------------------------
-- LSP Handlers
-------------------------------------------------------------

-- | LSP message handlers
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized $ \_msg -> return (),

    -- Handle document open/change events for diagnostics
    notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      liftIO (hPutStrLn stderr "documented opened")
      let params' = msg ^. params
          uri' = params' ^. textDocument . uri
      handleDocumentChange uri'

  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
      let params' = msg ^. params
          uri' = params' ^. textDocument . uri
      liftIO (hPutStrLn stderr "documented changed")
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

  liftIO (hPutStrLn stderr $ "uri: " ++ show uri)
  -- Clear diagnostics for both parser and typechecker sources
  flushDiagnosticsBySource 100 (Just "analysislang-parser")
  flushDiagnosticsBySource 100 (Just "analysislang-typechecker")
  publishDiagnostics 100 (toNormalizedUri uri) Nothing (partitionBySource diagnostics)

-- | Convert parse error to diagnostic
parseErrorToDiagnostic :: Error -> Diagnostic
parseErrorToDiagnostic parseError = Diagnostic
  { _range = rangeToDiagnosticRange (AST.Range (parseErrorPosition parseError) (parseErrorPosition parseError))
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
_handleDocumentSymbols :: (MonadIO m, MonadLsp () m) => Uri -> m [DocumentSymbol]
_handleDocumentSymbols uri = do
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
_handleGotoDefinition :: (MonadIO m, MonadLsp () m) => Uri -> Position -> m (Maybe Location)
_handleGotoDefinition uri pos = runMaybeT $ do
  vf <- MaybeT $ getVirtualFile (toNormalizedUri uri)
  let content = virtualFileText vf
  program <- MaybeT $ return $ either (const Nothing) Just $ parseProgram (T.unpack content)
  context <- MaybeT $ return $ either (const Nothing) Just $ runChecker program
  let symbolName = _extractSymbolAtPosition content pos
  MaybeT $ return $ findDefinition context (_lspPositionToASTPosition pos) symbolName

-------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------

-- | Extract symbol name at given position (simplified implementation)
_extractSymbolAtPosition :: T.Text -> Position -> String
_extractSymbolAtPosition _content _pos = "placeholder" -- TODO: Implement proper symbol extraction

-- | Convert LSP Position to AST Position
_lspPositionToASTPosition :: Position -> Language.AST.Position
_lspPositionToASTPosition (Position line char) =
  Language.AST.Position (fromIntegral line + 1) (fromIntegral char + 1) Nothing
