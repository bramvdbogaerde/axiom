{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
module Language.CodeGen.Phase(CodeGenPhase, CodeGenProgram) where

import Language.AST
import Language.CodeGen.HaskellHatch
import Language.Types (Typ)
import Data.Data
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

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

