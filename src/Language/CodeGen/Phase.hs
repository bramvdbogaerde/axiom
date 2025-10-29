{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Language.CodeGen.Phase(CodeGenPhase, CodeGenProgram, nocodegen) where

import Language.AST
import Language.CodeGen.HaskellHatch
import Language.Types (Typ)
import Data.Data
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Void (absurd)
import Control.Monad.Error.Class (MonadError, throwError)

-- | Phase after code generation. Haskell expressions are turned in pure functions
-- that have unified terms are their argument. The solver makes sure that all terms
-- are ground before passing them to the function and tries to unwrap the data
-- into pure Haskell types wherever possible.
data CodeGenPhase
type instance XHaskellExpr CodeGenPhase  = HaskellHatch CodeGenPhase
type instance XTypeAnnot CodeGenPhase  = Typ

type CodeGenProgram = Program' CodeGenPhase

-- | CodeGen phase instance: Haskell expressions are executable via HaskellHatch
instance HaskellExprExecutor CodeGenPhase where
  executeHaskellExpr hatch mapping =
    let mapping' = Map.mapKeys (\k -> fromMaybe k  $ Map.lookup k (renamedVariables hatch)) mapping
    in case execute hatch (Proxy @CodeGenPhase) mapping' of
      Left (InvalidTypePassed expected actual) ->
        Left $ "Type error: expected " ++ show expected ++ ", got " ++ show actual
      Left (UserError msg) ->
        Left $ "Execution error: " ++ msg
      Right result ->
        Right result

instance AnnotateType CodeGenPhase where
  typeAnnot _ = id
  -- For codegen phase, the XTypeAnnot is just Typ, so return it directly
  getTermType typ = typ

instance HaskellExprRename CodeGenPhase where
  haskellExprRename mapping hatch = hatch {  renamedVariables = mapping }
  haskellExprFreeVars = Set.fromList . freeVars

-- | Convert a TypingPhase expression to a CodeGenPhase expression
-- This function is used to convert typed terms that don't contain any code generation
-- constructs (like HaskellExpr) into the CodeGenPhase representation.
-- Terms with HaskellExpr will fail since they require proper code generation.
nocodegenExpr :: (MonadError String m, Ord (a String), Ord (XEmbeddedValue CodeGenPhase))
              => Expr TypingPhase a -> m (Expr CodeGenPhase a)
nocodegenExpr = \case
  LookupMap mapping key tpy r ->
    LookupMap <$> nocodegen mapping <*> nocodegen key <*> pure tpy <*> pure r
  UpdateMap mapping key val tpy r ->
    UpdateMap <$> nocodegenExpr mapping <*> nocodegen key <*> nocodegen val <*> pure tpy <*> pure r
  EmptyMap tpy r -> pure $ EmptyMap tpy r
  RewriteApp nam ags tpy r ->
    RewriteApp nam <$> traverse nocodegen ags <*> pure tpy <*> pure r
  GroundTerm t tpy r ->
    GroundTerm <$> nocodegen t <*> pure tpy <*> pure r
  SetUnion t1 t2 tpy r ->
    SetUnion <$> nocodegen t1 <*> nocodegen t2 <*> pure tpy <*> pure r

-- | Convert a TypingPhase term to CodeGenPhase by leaving out code generation
-- This function does not allow for terms that are supposed to be generated after
-- the code generation phase (like HaskellExpr or TermHask with non-Void values).
-- If you try to convert such terms, it will throw an error in the MonadError context.
nocodegen :: (MonadError String m, Ord (a String), Ord (XEmbeddedValue CodeGenPhase))
          => Term' TypingPhase a -> m (Term' CodeGenPhase a)
nocodegen = \case
  Atom atomId tpy range -> pure $ Atom atomId tpy range
  Functor name terms tpy range ->
    Functor name <$> traverse nocodegen terms <*> pure tpy <*> pure range
  TermValue value tpy range -> pure $ TermValue value tpy range
  Eqq left right tpy range ->
    Eqq <$> nocodegen left <*> nocodegen right <*> pure tpy <*> pure range
  Neq left right tpy range ->
    Neq <$> nocodegen left <*> nocodegen right <*> pure tpy <*> pure range
  Transition tname from to tpy range ->
    Transition tname <$> nocodegen from <*> nocodegen to <*> pure tpy <*> pure range
  SetOfTerms terms tpy range -> do
    terms' <- traverse nocodegen (Set.toList terms)
    pure $ SetOfTerms (Set.fromList terms') tpy range
  HaskellExpr _ _ _ -> throwError "nocodegen: HaskellExpr requires code generation and cannot be converted directly"
  IncludedIn var term range ->
    IncludedIn var <$> nocodegen term <*> pure range
  TermHask value _ _ -> absurd value  -- TypingPhase has Void for XEmbeddedValue, so this is unreachable
  TermExpr expr range ->
    TermExpr <$> nocodegenExpr expr <*> pure range
  TermMap mapping tpy range -> do
    mappingList <- traverse (\(k, v) -> (,) <$> nocodegen k <*> nocodegen v) (Map.toList mapping)
    pure $ TermMap (Map.fromList mappingList) tpy range
  Wildcard t range -> pure $ Wildcard t range

