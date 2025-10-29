{-# LANGUAGE RankNTypes #-}
module Language.Axiom(
    module Language.CodeGen.Prelude,
    solverFromRules,
    solve,
    typeQuery
  ) where 

import Language.CodeGen.Prelude
import qualified Language.Solver as Solver
import qualified Language.Solver.BacktrackingST as ST
import Data.Map
import Language.TypeCheck

-- | Create a new solver based on the given rules and subtyping information
solverFromRules :: (ForAllPhases Ord p) => Subtyping -> [RuleDecl' p] -> [PureRewriteDecl p] -> Solver.EngineCtx p [] s
solverFromRules = Solver.fromRules

-- | Type an untyped term using the given type checker
typeQuery :: CheckingContext -> PureTerm -> Either Error TypedTerm
typeQuery = runCheckTerm

-- | Solve 'query' against 'solver' and return the out-cache of the given query
solve :: (HaskellExprExecutor p, AnnotateType p, ForAllPhases Ord p, HaskellExprRename p)
      => (forall s . Solver.EngineCtx p [] s)
      -> PureTerm' p
      -> [Map String (PureTerm' p)]
solve solver query =
  ST.runST $ Solver.runSolver solver (Solver.solve query)
  
