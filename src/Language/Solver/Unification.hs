{-# LANGUAGE LambdaCase #-}
module Language.Solver.Unification where

import Data.Map (Map)
import qualified Data.Map as Map
import Language.AST
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Except
import Control.Monad.Extra (ifM)
import Control.Monad ((>=>), zipWithM_)
import Data.STRef
import Data.Functor.Identity

-------------------------------------------------------------
-- Core data types
-------------------------------------------------------------

-- | A mapping between original variable names and their references
type VariableMapping s = Map String (Cell s String)

-- | The value within the cell of variables
data CellValue s a = Value (Term (Cell s))
                   | Ptr (Cell s a)
                   | Uninitialized String

newtype Cell s a = Ref (STRef s (CellValue s a))

-------------------------------------------------------------
-- Union find
-------------------------------------------------------------

-- | Get the final cell value with path compression
setOf :: STRef s (CellValue s String) -> ST s (CellValue s String)
setOf ref = readSTRef ref >>= \case
  Ptr (Ref nextRef) -> do
    finalValue <- setOf nextRef
    writeSTRef ref finalValue  -- Path compression
    return finalValue
  v -> return v

-- | Flatten indirect references in the variable mapping to speed up conversion
flattenMapping :: VariableMapping s -> ST s ()
flattenMapping = mapM_ flattenCell
  where
    flattenCell :: Cell s String -> ST s ()
    flattenCell (Ref cellRef) = setOf cellRef >>= writeSTRef cellRef

-------------------------------------------------------------
-- Conversions between reference cells
-------------------------------------------------------------

-- | Transform a term to use IORefs instead of variables, returns
-- the transformed term, together with a mapping from variable names
-- to their references.
refTerm ::  PureTerm -> VariableMapping s -> ST s (Term (Cell s), VariableMapping s)
refTerm term = runStateT (transformTerm term)
  where
    transformTerm :: PureTerm -> StateT (VariableMapping s) (ST s) (Term (Cell s))
    transformTerm (Atom (Identity varName) range) =
      maybe createNewCell (return . flip Atom range) =<< gets (Map.lookup varName)
      where
        createNewCell = do
          cellRef <- lift $ newSTRef (Uninitialized varName)
          let cell = Ref cellRef
          modify (Map.insert varName cell)
          return $ Atom cell range

    transformTerm (Functor name subterms range) =
      Functor name <$> mapM transformTerm subterms <*> pure range

    transformTerm (Eqq left right range) =
      Eqq <$> transformTerm left <*> transformTerm right <*> pure range

    transformTerm (Transition transName left right range) =
      Transition transName <$> transformTerm left <*> transformTerm right <*> pure range

-- | Visited list abstraction for cycle detection
type VisitedList s = [STRef s (CellValue s String)]

-- | Check if a reference is in the visited list
isVisited :: STRef s (CellValue s String) -> VisitedList s -> ST s Bool
isVisited ref = return . (ref `elem`)

-- | Add a reference to the visited list
addVisited :: STRef s (CellValue s String) -> VisitedList s -> VisitedList s
addVisited ref visited = ref : visited

-- | Convert a pointer-based term back to a pure term with cycle detection
pureTerm' :: Term (Cell s) -> VariableMapping s -> ST s (Either String PureTerm)
pureTerm' term mapping = do
  flattenMapping mapping
  runExceptT $ evalStateT (convertTerm term) []
  where
    convertTerm :: Term (Cell s) -> StateT (VisitedList s) (ExceptT String (ST s)) PureTerm
    convertTerm (Atom (Ref cellRef) range) =
      lift (lift $ readSTRef cellRef) >>= \case
        Value term ->
          ifM (get >>= lift . lift . isVisited cellRef)
              (lift $ throwError "Cycle detected during term conversion")
              (modify (addVisited cellRef) >> convertTerm term)
        Uninitialized varName -> return $ Atom (Identity varName) range
        Ptr _ -> error "Unreachable: flattenMapping should have eliminated all Ptr cases"

    convertTerm (Functor name subterms range) =
      Functor name <$> mapM convertTerm subterms <*> pure range

    convertTerm (Eqq left right range) =
      Eqq <$> convertTerm left <*> convertTerm right <*> pure range

    convertTerm (Transition transName left right range) =
      Transition transName <$> convertTerm left <*> convertTerm right <*> pure range

-- | Same as pureTerm' but raises an error if the term could not be converted
pureTerm :: Term (Cell s) -> VariableMapping s -> ST s PureTerm
pureTerm term = pureTerm' term >=> either error return

-------------------------------------------------------------
-- Unification 
-------------------------------------------------------------

-- | Unify two reference-based terms
unifyTerms :: Term (Cell s) -> Term (Cell s) -> ExceptT String (ST s) ()
unifyTerms = unifyTermsImpl
  where
    unifyTermsImpl :: Term (Cell s) -> Term (Cell s) -> ExceptT String (ST s) ()
    unifyTermsImpl (Atom cell1 _) (Atom cell2 _) = unifyAtoms cell1 cell2
    unifyTermsImpl (Functor name1 args1 _) (Functor name2 args2 _) = unifyFunctors name1 args1 name2 args2
    unifyTermsImpl (Atom cell _) functor@(Functor {}) = unifyAtomWithTerm cell functor
    unifyTermsImpl functor@(Functor {}) (Atom cell _) = unifyAtomWithTerm cell functor
    unifyTermsImpl (Eqq l1 r1 _) (Eqq l2 r2 _) = unifyTermsImpl l1 l2 >> unifyTermsImpl r1 r2
    unifyTermsImpl (Transition n1 l1 r1 _) (Transition n2 l2 r2 _) =
      if n1 == n2 then unifyTermsImpl l1 l2 >> unifyTermsImpl r1 r2
      else throwError $ "Transition names don't match: " ++ n1 ++ " vs " ++ n2
    unifyTermsImpl t1 t2 = throwError "Cannot unify different term structures"

    unifyAtoms :: Cell s String -> Cell s String -> ExceptT String (ST s) ()
    unifyAtoms (Ref ref1) (Ref ref2) = do
      finalVal1 <- lift $ setOf ref1
      finalVal2 <- lift $ setOf ref2
      case (finalVal1, finalVal2) of
        (Uninitialized name1, Uninitialized name2) -> do
          -- Check for self-unification (same variable name)
          if name1 == name2
            then return () -- Self-unification succeeds
            else do
              -- Check for cycle: if ref2's final parent would point back to ref1's parent
              if ref1 == ref2
                then throwError "Cycle detected in unification"
                else lift $ writeSTRef ref1 (Ptr (Ref ref2))
        (Uninitialized _, Value term) ->
          -- Left is variable, right has value - unify variable with term
          lift $ writeSTRef ref1 (Value term)
        (Value term, Uninitialized _) ->
          -- Right is variable, left has value - unify variable with term
          lift $ writeSTRef ref2 (Value term)
        (Value term1, Value term2) ->
          -- Both have values - must unify the terms
          unifyTermsImpl term1 term2
        _ -> throwError "Unexpected cell value configuration"

    unifyFunctors :: String -> [Term (Cell s)] -> String -> [Term (Cell s)] -> ExceptT String (ST s) ()
    unifyFunctors name1 args1 name2 args2 =
      if name1 == name2 && length args1 == length args2
        then zipWithM_ unifyTermsImpl args1 args2
        else throwError $ "Functors don't match: " ++ name1 ++ " vs " ++ name2

    unifyAtomWithTerm :: Cell s String -> Term (Cell s) -> ExceptT String (ST s) ()
    unifyAtomWithTerm (Ref ref) term =
      lift (readSTRef ref) >>= \case
        Uninitialized _ -> lift $ writeSTRef ref (Value term)
        Value existingTerm -> unifyTermsImpl existingTerm term
        _ -> throwError "Unexpected cell value in atom-term unification"

-- | Unify two pure terms and return a substitution mapping
unify :: PureTerm -> PureTerm -> ExceptT String (ST s) (Map String PureTerm)
unify term1 term2 = do
  -- Convert first term to ref term with empty mapping
  (refTerm1, mapping1) <- lift $ refTerm term1 Map.empty
  -- Convert second term to ref term, sharing the mapping from the first
  (refTerm2, sharedMapping) <- lift $ refTerm term2 mapping1
  -- Attempt unification
  unifyTerms refTerm1 refTerm2
  -- Convert mapping back to pure terms
  results <- lift $ mapM (\cell -> pureTerm' (Atom cell dummyRange) sharedMapping) sharedMapping
  either throwError return $ sequenceA results

-- | Run unification in the ST monad and return the result
runUnification :: PureTerm -> PureTerm -> Either String (Map String PureTerm)
runUnification term1 term2 = runST $ runExceptT $ unify term1 term2
