{-# LANGUAGE RankNTypes #-}
module Language.Axiom(
    module Language.CodeGen.Prelude,
    solverFromProgram,
    solve,
    typeQuery
  ) where 

import Language.CodeGen.Prelude
import qualified Language.Solver as Solver
import qualified Language.Solver.BacktrackingST as ST
import Data.Map
import Language.TypeCheck

-- | Create a new solver based on the given rules and subtyping information
solverFromProgram :: (ForAllPhases Ord p) => Subtyping -> Program' p -> Solver.EngineCtx p [] s
solverFromProgram = Solver.fromProgram

-- | Type an untyped term using the given type checker
typeQuery :: CheckingContext -> PureTerm -> Either Error TypedTerm
typeQuery = runCheckTerm

-- | Solve 'query' against 'solver' and return the out-cache of the given query
solve :: (HaskellExprExecutor p, ForAllPhases Show p, AnnotateType p, ForAllPhases Ord p, HaskellExprRename p)
      => (forall s . Solver.EngineCtx p [] s)
      -> PureTerm' p
      -> [Map String (PureTerm' p)]
solve solver query =
  ST.runST $ Solver.runSolver solver (Solver.solve query)
  
