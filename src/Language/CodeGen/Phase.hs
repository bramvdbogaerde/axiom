{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
module Language.CodeGen.Phase(CodeGenPhase, CodeGenProgram) where

import Language.AST
import Language.CodeGen.HaskellHatch

-- | Phase after code generation. Haskell expressions are turned in pure functions
-- that have unified terms are their argument. The solver makes sure that all terms
-- are ground before passing them to the function and tries to unwrap the data
-- into pure Haskell types wherever possible.
data CodeGenPhase
type instance XHaskellExpr CodeGenPhase  = HaskellHatch

type CodeGenProgram = Program' CodeGenPhase

-- | CodeGen phase instance: Haskell expressions are executable via HaskellHatch
-- Note: This assumes XHaskellExpr CodeGenPhase ~ HaskellHatch
instance HaskellExprExecutor CodeGenPhase where
  executeHaskellExpr hatch mapping = 
    case execute hatch mapping of
      Left (InvalidTypePassed expected actual) -> 
        Left $ "Type error: expected " ++ show expected ++ ", got " ++ show actual
      Left (UserError msg) -> 
        Left $ "Execution error: " ++ msg
      Right result -> 
        Right result
