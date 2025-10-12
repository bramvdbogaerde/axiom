{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Language.Solver.Unification where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Language.AST
import Language.Types
import Control.Lens
import Control.Monad.State
import qualified Language.Solver.BacktrackingST as BST
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Extra (ifM)
import Control.Monad ( (>=>), zipWithM_, unless )
import GHC.Exts (sortWith)
import qualified Data.Set as Set
import Data.Kind
import Data.Maybe (fromMaybe)
import qualified Language.Solver.Renamer as Renamer

-------------------------------------------------------------
-- Core data types
-------------------------------------------------------------

type RefTerm p s = Term' p (Cell p s)
type RefExpr p s = Expr p (Cell p s)
type RefRewriteRule p s = RewriteDecl' p (Cell p s)

-- | The value within the cell of variables
data CellValue p s a = Value (RefTerm p s)
                   | Ptr (Cell p s a)
                   | Uninitialized String Typ
deriving instance (ForAllPhases Eq p) => Eq (CellValue p s a)

newtype Cell p s a = Ref (BST.STRef s (CellValue p s a))
                 deriving (Eq, Ord)

-- | A mapping between original variable names and their references
type VariableMapping p s = Map String (Cell p s String)
type Rewrites p = Map String [PureRewriteDecl p]

-- | Information recursively passed around in the unification engine
data UnificationInfo p s = UnificationInfo {
    -- | Tracks the subtyping graph, necessary for widening/narrowing during unification
    _subtyping          :: !Subtyping,
    -- | Tracks rewrite rules as a mapping from rule names to their alternatives
    _rewriteRules       :: !(Rewrites p)
    }

data UnificationState p s = UnificationState {
    -- | Tracks a mapping from variable names to their reference cells
    _variableMapping :: VariableMapping p s,
    -- | A counter that tracks the number of unique variables in the current context
    _numUniqueVariables :: !Int
  }

$(makeLenses ''UnificationInfo)
$(makeLenses ''UnificationState)

-- | Create a 'UnificationInfo' using the given subtyping graph and rewrite rules
emptyUnificationInfo :: Subtyping -> Map String [PureRewriteDecl p] -> UnificationInfo p s
emptyUnificationInfo = UnificationInfo

-- | Create an empty 'UnificationState'
emptyUnificationState :: UnificationState p s
emptyUnificationState = UnificationState Map.empty 0

-------------------------------------------------------------
-- Monad
-------------------------------------------------------------

-- | Unification monad with access to variable mapping and subtyping graph
type UnificationM p s = ReaderT (UnificationInfo p s) (StateT (UnificationState p s) (ExceptT String (BST.ST s)))

-- | Get the current variable mapping
getVariableMapping :: UnificationM p s (VariableMapping p s)
getVariableMapping = use variableMapping

updateVariableMapping :: VariableMapping p s -> UnificationM p s ()
updateVariableMapping = modify . over variableMapping . const

-- | Get the current subtyping graph
getSubtyping :: UnificationM p s Subtyping
getSubtyping = view subtyping

-- | Find the rewrite rules matching the given name
findRewriteRules :: String -> UnificationM p s [PureRewriteDecl p]
findRewriteRules nam = asks (fromMaybe [] . Map.lookup nam . _rewriteRules)

-- | Retrieve the counter of unique variables
getUniqueVariables :: UnificationM p s Int
getUniqueVariables = use numUniqueVariables

-- | Update the counter of unique variables
updateUniqueVariables :: Int -> UnificationM p s ()
updateUniqueVariables = modify . over numUniqueVariables . const

pureTerm'' :: (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> UnificationM p s (PureTerm' p)
pureTerm'' t = do
  getVariableMapping >>= lift . lift . ExceptT . pureTerm' t

-- | Lift an ST operation into the unification monad
liftST :: BST.ST s a -> UnificationM p s a
liftST = lift . lift . lift

-------------------------------------------------------------
-- Union find
-------------------------------------------------------------

readCellRef :: Cell p s a -> BST.ST s (CellValue p s a)
readCellRef (Ref ref) = BST.readSTRef ref

writeCellRef :: Cell p s a -> CellValue p s a -> BST.ST s ()
writeCellRef (Ref ref) = BST.writeSTRef ref

-- | Get the final cell value with path compression
setOf :: Cell p s String -> BST.ST s (Cell p s String)
setOf cell = readCellRef cell >>= \case
  Ptr nextCell -> do
    finalPtr <- setOf nextCell
    writeCellRef cell (Ptr finalPtr) -- Path compression
    return finalPtr
  _ -> return cell

-- | Read the value of the parent
parentValue :: Cell p s String -> BST.ST s (CellValue p s String)
parentValue = setOf >=> readCellRef

-- | Flatten indirect references in the variable mapping to speed up conversion
flattenMapping :: VariableMapping p s -> BST.ST s ()
flattenMapping = mapM_ flattenCell
  where
    flattenCell :: Cell p s String -> BST.ST s ()
    flattenCell cell = parentValue cell >>= writeCellRef cell

-- | Returns the unified value of the term if the term is an atom
-- otherwise returns the term value itself.
termValue :: RefTerm p s -> BST.ST s (Maybe (RefTerm p s))
termValue (Atom cell _ _) = parentValue cell <&> ensureInitialized
  where ensureInitialized (Value t) = Just t
        ensureInitialized _ = Nothing
termValue t = return $ Just t

-------------------------------------------------------------
-- Conversions between reference cells
-------------------------------------------------------------

-- | Transform a term to use IORefs instead of variables, returns
-- the transformed term, together with a mapping from variable names
-- to their references.
refTerm :: forall p s . (ForAllPhases Ord p, AnnotateType p) => PureTerm' p -> VariableMapping p s -> BST.ST s (RefTerm p s, VariableMapping p s)
refTerm term = runStateT (transformTerm term)
  where
    transformTerm :: PureTerm' p -> StateT (VariableMapping p s) (BST.ST s) (RefTerm p s)
    transformTerm (Atom (Identity varName) tpy range) =
      maybe createNewCell (return . (\c -> Atom c tpy range)) =<< gets (Map.lookup varName)
      where
        createNewCell = do
          cellRef <- lift $ BST.newSTRef (Uninitialized varName (getTermType @p tpy))
          let cell = Ref cellRef
          modify (Map.insert varName cell)
          return $ Atom cell tpy range

    transformTerm (Functor name subterms tpy range) =
      Functor name <$> mapM transformTerm subterms <*> pure tpy <*> pure range

    transformTerm (Eqq left right tpy range) =
      Eqq <$> transformTerm left <*> transformTerm right <*> pure tpy <*> pure range

    transformTerm (Neq left right tpy range) =
      Neq <$> transformTerm left <*> transformTerm right <*> pure tpy <*> pure range

    transformTerm (Transition transName left right tpy range) =
      Transition transName <$> transformTerm left <*> transformTerm right <*> pure tpy <*> pure range

    transformTerm (TermValue value tpy range) =
      return $ TermValue value tpy range

    transformTerm (HaskellExpr expr tpy range) =
      return $ HaskellExpr expr tpy range

    transformTerm (SetOfTerms ts annot r) =
      SetOfTerms <$> fmap Set.fromList (mapM transformTerm (Set.toList ts)) <*> pure annot <*> pure r

    transformTerm (IncludedIn vrr set range) =
      IncludedIn vrr <$> transformTerm set <*> pure range

    transformTerm (TermHask value tpy range) =
      return $ TermHask value tpy range

    transformTerm (TermExpr expr range) = flip TermExpr range <$> transformExpr expr

    transformTerm (TermMap mapping tpy range) =
      return $ TermMap mapping tpy range

    transformTerm (Wildcard t range) = return (Wildcard t range)

    transformExpr = \case
            LookupMap t1 t2 tpy range ->
                LookupMap <$> transformTerm t1 <*> transformTerm t2 <*> pure tpy <*> pure range
            UpdateMap t1 t2 t3 tpy range ->
                UpdateMap <$> transformExpr t1 <*> transformTerm t2 <*> transformTerm t3 <*> pure tpy <*> pure range
            RewriteApp nam ags tpy range ->
                RewriteApp nam <$> mapM transformTerm ags <*> pure tpy <*> pure range
            EmptyMap tpy r -> return $ EmptyMap tpy r
            GroundTerm t tpy range -> GroundTerm <$> transformTerm t <*> pure tpy <*> pure range

-- | Transform a term to a reference term within the unification monad and use
-- the internal variable mapping while doing so.
refTerm' :: (ForAllPhases Ord p, AnnotateType p) => PureTerm' p -> UnificationM p s (RefTerm p s)
refTerm' t = do
  variableMapping <- getVariableMapping
  (t', variableMapping') <- liftST  $ refTerm t variableMapping
  updateVariableMapping variableMapping'
  return t'

-- | Visited list abstraction for cycle detection
type VisitedList p s = [BST.STRef s (CellValue p s String)]

-- | Check if a reference is in the visited list
isVisited :: BST.STRef s (CellValue p s String) -> VisitedList p s -> BST.ST s Bool
isVisited ref = return . (ref `elem`)

-- | Add a reference to the visited list
addVisited :: BST.STRef s (CellValue p s String) -> VisitedList p s -> VisitedList p s
addVisited ref visited = ref : visited

-- | Convert a pointer-based term back to a pure term with cycle detection
pureTerm' :: (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> VariableMapping p s -> BST.ST s (Either String (PureTerm' p))
pureTerm' term _mapping = do
  -- TODO: should we still flatten the mapping or not?
  -- flattenMapping mapping
  runExceptT $ evalStateT (convertTerm term) []
  where
    convertTerm :: forall p s .  (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> StateT (VisitedList p s) (ExceptT String (BST.ST s)) (PureTerm' p)
    convertTerm (Atom (Ref cellRef) _tpy range) =
        lift (lift $ BST.readSTRef cellRef) >>= convertCell
      where convertCell = \case
              Value term ->
                ifM (get >>= lift . lift . isVisited cellRef)
                    (lift $ throwError "Cycle detected during term conversion")
                    (modify (addVisited cellRef) >> convertTerm term)
              Uninitialized varName cellType -> return $ Atom (Identity varName) (typeAnnot (Proxy @p) cellType) range
              Ptr cell -> lift (lift $ parentValue cell) >>= convertCell

    convertTerm (Functor name subterms tpy range) =
      Functor name <$> mapM convertTerm subterms <*> pure tpy <*> pure range

    convertTerm (Eqq left right tpy range) =
      Eqq <$> convertTerm left <*> convertTerm right <*> pure tpy <*> pure range

    convertTerm (Neq left right tpy range) =
      Neq <$> convertTerm left <*> convertTerm right <*> pure tpy <*> pure range

    convertTerm (Transition transName left right tpy range) =
      Transition transName <$> convertTerm left <*> convertTerm right <*> pure tpy <*> pure range

    convertTerm (TermValue value tpy range) =
      return $ TermValue value tpy range

    convertTerm (HaskellExpr expr tpy range) =
      return $ HaskellExpr expr tpy range

    convertTerm (IncludedIn vrr set range) =
      IncludedIn vrr <$> convertTerm set <*> pure range

    convertTerm (SetOfTerms ts tpy r)  =
      SetOfTerms <$> fmap Set.fromList (mapM convertTerm (Set.toList ts)) <*> pure tpy <*> pure r

    convertTerm (TermHask value tpy range) =
      return $ TermHask value tpy range

    convertTerm (TermExpr expr range) = flip TermExpr range <$> convertExpr expr

    convertTerm (TermMap mapping tpy range) =
      -- TODO: the bimapM here is redundant
      return $ TermMap mapping tpy range

    convertTerm (Wildcard t range) =
      return $ Wildcard t range

    convertExpr :: forall p (s :: Type) .  (ForAllPhases Ord p, AnnotateType p) => RefExpr p s -> StateT (VisitedList p s) (ExceptT String (BST.ST s)) (Expr p Identity)
    convertExpr = \case
        LookupMap t1 t2 tpy range ->
            LookupMap <$> convertTerm t1 <*> convertTerm t2 <*> pure tpy <*> pure range
        UpdateMap t1 t2 t3 tpy range ->
            UpdateMap <$> convertExpr t1 <*> convertTerm t2 <*> convertTerm t3 <*> pure tpy <*> pure range
        RewriteApp nam ags tpy range ->
            RewriteApp nam <$> mapM convertTerm ags <*> pure tpy <*> pure range
        EmptyMap tpy r -> return $ EmptyMap tpy r
        GroundTerm t tpy r -> GroundTerm <$> convertTerm t <*> pure tpy <*> pure r


-- | Same as pureTerm' but raises an error if the term could not be converted
pureTerm :: (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> VariableMapping p s -> BST.ST s (PureTerm' p)
pureTerm term = pureTerm' term >=> either error return


-- | Same as pureTerm but ensures that all elements of the term are ground
pureTermGround :: (ForAllPhases Ord p, AnnotateType p) => RefTerm p s -> VariableMapping p s -> BST.ST s (PureTerm' p)
pureTermGround term mapping = do
  term' <- pureTerm term mapping
  if isTermGround term'
    then return term'
    else error $ "Term is not ground " ++ show term'

-------------------------------------------------------------
-- Expression evaluation
-------------------------------------------------------------

-- | Normalize a term, meaning that nested terms of "TermExpr" and "GroundTerm" are collapsed into the inner "Term".
normalizeTerm :: (ForAllPhases Ord p, HaskellExprRename p, HaskellExprExecutor p, AnnotateType p) => RefTerm p s -> UnificationM p s (RefTerm p s)
normalizeTerm (TermExpr expr _) = do
  evaluateExpr expr >>= normalizeTerm
normalizeTerm t = return t

-- | Evaluate an element of the expression language
evaluateExpr :: (ForAllPhases Ord p, HaskellExprRename p, HaskellExprExecutor p, AnnotateType p) => RefExpr p s  -> UnificationM p s (RefTerm p s)
evaluateExpr (EmptyMap tpy r) =
  return (TermMap Map.empty tpy r)
evaluateExpr (LookupMap mapping key _ _) = do
  mapping' <- pureTerm'' =<< normalizeTerm mapping
  key' <- pureTerm'' =<< normalizeTerm key
  case mapping' of
    TermMap mapping'' _ _ ->
      -- TODO: add show isntance for the mapping
      refTerm' =<< maybe (error $ "Key " ++ show key' ++ " not found") return (Map.lookup (removeRange key') mapping'')
    -- The term not being a 'TermMap' would be an error in the type checker, so we can just crash here
    _ -> error $ "LookupMap: expected a map, got " ++ show (termTypeOf mapping') -- TODO: add show instance
evaluateExpr (UpdateMap mapping key value _ range) = do
  mapping' <- pureTerm'' =<< evaluateExpr mapping
  key' <- pureTerm'' =<< normalizeTerm key
  value' <- pureTerm'' =<< normalizeTerm value
  let mappingTpy = termTypeAnnot mapping'
  case mapping' of
    TermMap mapping'' _ _ ->
      return $ TermMap (Map.insert (removeRange key') value' mapping'') mappingTpy range
    -- The term not being a 'TermMap' would be an error in the type checker, so we can just crash here
    _ -> error $ "UpdateMap: expected a map, got " ++ show (termTypeOf mapping') -- TODO: add show instance of mapping
evaluateExpr (GroundTerm t _ _) = normalizeTerm t
evaluateExpr (RewriteApp nam args _ range) = tryRules =<< findRewriteRules nam
  where tryRules [] = throwError $ "No matching rewrite rules for application " ++ nam ++ " at " ++ show range
        tryRules (rewriteRule : remaining) = do
          _snapshot <- liftST BST.snapshot
          (do
            -- Rename the arguments of the functions so that subsequent
            -- invocations of the same rewrite rule does not unify
            -- with the same parameters.
            (RewriteDecl _ prs bdy _) <- uniqueRewrite rewriteRule
            -- Then transform the atoms to cells ... 
            refPars <- mapM refTerm' prs
            refBdy <- refTerm' bdy
            -- ... and unify them with the arguments
            zipWithM_ unifyTerms refPars args
            return refBdy) `catchError` const (tryRules remaining)
        uniqueRewrite  = zoom numUniqueVariables . lift . Renamer.renameState . Renamer.renameRewrite
-------------------------------------------------------------
-- Unification 
-------------------------------------------------------------

-- | Puts the terms in a fixed order based on their data type,
-- this is to elliminate the need for symmetric cases in the unification
-- engine. The order is as follows:
--
-- Atom < Eqq < Neq < Transition < HaskellExpr < Value
data TermType = AtomTp | WildcardTp |  EqqTp | NeqTp | TransitionTp | ValueTp | FunctorTp | HaskellExprTp | IncludedInTp | TermHaskTp | TermExprTp | MapTermTp | SetOfTermsTp
              deriving (Ord, Eq, Show, Enum)
termTypeOf :: Term' p x -> TermType
termTypeOf Atom{} = AtomTp
termTypeOf Eqq{}  = EqqTp
termTypeOf Neq{}  = NeqTp
termTypeOf Transition{} = TransitionTp
termTypeOf HaskellExpr{} = HaskellExprTp
termTypeOf TermValue{} = ValueTp
termTypeOf Functor{} = FunctorTp
termTypeOf IncludedIn{} = IncludedInTp
termTypeOf SetOfTerms{} = SetOfTermsTp
termTypeOf TermHask{} = TermHaskTp
termTypeOf TermExpr{} = TermExprTp
termTypeOf TermMap {} = MapTermTp
termTypeOf Wildcard {} = WildcardTp

-- | Puts two terms into a predictable order, therefore elliminating some symmetric cases in the unification algorithm.
termOrder :: Term' p x -> Term' p x -> (Term' p x, Term' p x)
termOrder a b = case sortWith (fromEnum . termTypeOf) [a, b] of
  [a', b'] -> (a', b')
  _ -> error "termOrder: impossible case - sortWith should always return exactly 2 elements"

-- | Unify two reference-based terms
unifyTerms :: forall p s . (ForAllPhases Ord p, AnnotateType p, HaskellExprRename p,  HaskellExprExecutor p) => RefTerm p s -> RefTerm p s -> UnificationM p s ()
unifyTerms = unifyTermsImpl
  where
    unifyTermsImpl :: (AnnotateType p, HaskellExprRename p, HaskellExprExecutor p) => RefTerm p s  -> RefTerm p s -> UnificationM p s ()
    --
    -- Same term types
    -- 
    unifyTermsImpl (Atom cell1 _ _) (Atom cell2 _ _) = unifyAtoms cell1 cell2
    unifyTermsImpl (Functor name1 args1 _ _) (Functor name2 args2 _ _) =
      unifyFunctors name1 args1 name2 args2
    unifyTermsImpl (Eqq l1 r1 _ _) (Eqq l2 r2 _ _) =
      unifyTermsImpl l1 l2 >> unifyTermsImpl r1 r2
    unifyTermsImpl (Neq l1 r1 _ _) (Neq l2 r2 _ _) =
      unifyTermsImpl l1 l2 >> unifyTermsImpl r1 r2
    unifyTermsImpl (Transition n1 l1 r1 _ _) (Transition n2 l2 r2 _ _) =
      if n1 == n2 then unifyTermsImpl l1 l2 >> unifyTermsImpl r1 r2
      else throwError $ "Transition names don't match: " ++ n1 ++ " vs " ++ n2
    unifyTermsImpl (TermValue v1 _ _) (TermValue v2 _ _) =
      if v1 == v2 then return ()
      else throwError $ "Values don't match: " ++ show v1 ++ " vs " ++ show v2
    unifyTermsImpl (SetOfTerms {}) (SetOfTerms {}) =  error "set unification not yet supported"
    unifyTermsImpl (TermHask v1 _ _) (TermHask v2 _ _) =
      if v1 == v2 then return ()
      else throwError "Embedded Haskell values don't match"

    -- 
    -- Wildcards
    -- 
    unifyTermsImpl (Wildcard _ _) _ =
      -- wildcard always matches so return nothing
      return ()
    unifyTermsImpl t w@Wildcard {} =
      unifyTermsImpl w t


    -- 
    -- AnalysisLang expressions
    -- 

    unifyTermsImpl otherTerm t@(TermExpr expr _) = do
      t'  <- pureTerm'' t
      case t' of
        TermExpr pureExpr _ -> do
          unless (isGround pureExpr) $
            throwError "The expression is not ground"
          result <- evaluateExpr expr
          pureResult <- pureTerm'' result
          if isGround pureResult
            then unifyTermsImpl result otherTerm
            else error "The result should be ground" -- TODO: should this be a throwError?
        -- INVARIANT: pureTerm t always returns the same type of term, but with a different
        -- cell representation.
        _ -> error "expected a TermExpr term but got "

    unifyTermsImpl t@(TermExpr {}) otherTerm =
      unifyTermsImpl otherTerm t

    -- 
    -- Haskell expressions
    -- 
    unifyTermsImpl otherTerm (HaskellExpr hatch _ _) = do
      -- Try to execute the Haskell expression and unify with the result
      varMapping <- getVariableMapping
      pureMapping <- liftST $ buildPureMapping varMapping
      case executeHaskellExpr hatch pureMapping of
        Left err -> throwError $ "Failed to execute Haskell expression: " ++ err
        Right result -> do
          (resultRef, _) <- liftST $ refTerm result varMapping
          -- TODO: The resultRef should not contain any variables and this should be enforced somehow in the future
          unifyTermsImpl otherTerm resultRef
    unifyTermsImpl hh@(HaskellExpr {}) otherTerm = unifyTermsImpl otherTerm hh
    --
    -- Unification with atoms
    -- 
    unifyTermsImpl (Atom cell _ _) t =
      unifyAtomWithTerm cell t
    unifyTermsImpl t (Atom cell _ _) =
      unifyAtomWithTerm cell t

    unifyTermsImpl _t1 _t2 = throwError "Cannot unify different term structures"

    unifyAtoms :: Cell p s String -> Cell p s String -> UnificationM p s ()
    unifyAtoms cell1@(Ref ref1) cell2@(Ref ref2) = do
      finalVal1 <- liftST $ parentValue cell1
      finalVal2 <- liftST $ parentValue cell2
      subtyping <- getSubtyping
      case (finalVal1, finalVal2) of
        (Uninitialized name1 typ1, Uninitialized name2 typ2) -> do
          -- Check type compatibility between the two variables
          case narrowType typ1 typ2 subtyping of
            Nothing -> throwError $ "Type mismatch: incompatible types " ++ show typ1 ++ " and " ++ show typ2
            Just narrowedType -> do
              -- Check for self-unification (same variable name)
              if name1 == name2
                then return () -- Self-unification succeeds
                else do
                  -- Check for cycle: if ref2's final parent would point back to ref1's parent
                  if ref1 == ref2
                    then throwError "Cycle detected in unification"
                    else do
                      -- Update both cells with the narrowed type constraint
                      liftST $ BST.writeSTRef ref1 (Ptr (Ref ref2))
                      liftST $ BST.writeSTRef ref2 (Uninitialized name2 narrowedType)
        (Uninitialized _ cellType, Value term) -> do
          -- Left is variable, right has value - check type compatibility first
          let termType = getTermType @p (termTypeAnnot @p term)
          case narrowType termType cellType subtyping of
            Nothing -> throwError $ "Type mismatch: cannot unify variable of type " ++ show cellType ++ " with term of type " ++ show termType
            Just _ -> liftST $ BST.writeSTRef ref1 (Value term)
        (Value term, Uninitialized _ cellType) -> do
          -- Right is variable, left has value - check type compatibility first
          let termType = getTermType @p (termTypeAnnot @p term)
          case narrowType termType cellType subtyping of
            Nothing -> throwError $ "Type mismatch: cannot unify variable of type " ++ show cellType ++ " with term of type " ++ show termType
            Just _ -> liftST $ BST.writeSTRef ref2 (Value term)
        (Value term1, Value term2) ->
          -- Both have values - must unify the terms
          unifyTermsImpl term1 term2
        _ -> throwError "Unexpected cell value configuration"

    unifyFunctors :: String -> [RefTerm p s] -> String -> [RefTerm p s] -> UnificationM p s ()
    unifyFunctors name1 args1 name2 args2 =
      if name1 == name2 && length args1 == length args2
        then zipWithM_ unifyTermsImpl args1 args2
        else throwError $ "Functors don't match: " ++ name1 ++ " vs " ++ name2

    unifyAtomWithTerm :: Cell p s String -> RefTerm p s -> UnificationM p s ()
    unifyAtomWithTerm cell term = do
      ref <- liftST $ setOf cell
      subtyping <- getSubtyping
      liftST (readCellRef ref) >>= \case
        Uninitialized _ cellType -> do
          -- Check type compatibility before unifying
          let termType = getTermType @p (termTypeAnnot @p term)
          case narrowType termType cellType subtyping of
            Nothing -> throwError $ "Type mismatch: cannot unify variable of type " ++ show cellType ++ " with term of type " ++ show termType
            Just _ -> liftST $ writeCellRef ref (Value term)
        Value existingTerm -> unifyTermsImpl existingTerm term
        _ -> error "Unexpected cell value in atom-term unification"

-- | Convert variable mapping to pure term mapping for Haskell expression execution
buildPureMapping :: forall p s . (ForAllPhases Ord p, AnnotateType p) => VariableMapping p s -> BST.ST s (Map String (PureTerm' p))
buildPureMapping varMapping = do
  results <- mapM (\cell -> pureTerm' (Atom cell (typeAnnot (Proxy @p) AnyType) dummyRange) varMapping) varMapping
  case sequenceA results of
    Left err -> error $ "Failed to build pure mapping: " ++ err
    Right pureMapping -> return pureMapping

-- | Unify two pure terms and return a substitution mapping
unify :: forall p s .
         (ForAllPhases Ord p, AnnotateType p, HaskellExprRename p, HaskellExprExecutor p)
      => PureTerm' p
      -> PureTerm' p
      -> UnificationM p s (Map String (PureTerm' p))
unify term1 term2 = do
  -- Convert first term to ref term with empty mapping
  refTerm1 <- refTerm' term1
  -- Convert second term to ref term, sharing the mapping from the first
  refTerm2 <- refTerm' term2
  -- Attempt unification using UnificationM
  unifyTerms refTerm1 refTerm2
  -- Convert mapping back to pure terms
  sharedMapping <- getVariableMapping
  results <- mapM (\cell -> liftST $ pureTerm' (Atom cell (typeAnnot (Proxy @p) AnyType) dummyRange) sharedMapping) sharedMapping
  either throwError return $ sequenceA results

-- | Construct the mapping resulting from the unification process
buildMapping :: forall p s . (AnnotateType p, ForAllPhases Ord p) => VariableMapping p s -> BST.ST s (Map String (PureTerm' p))
buildMapping mapping = mapM (\cell -> pureTerm (Atom cell (typeAnnot (Proxy @p) AnyType) dummyRange) mapping) mapping

-- | Run unification in the ST monad and return the result
runUnificationM :: forall p s a .
                  (ForAllPhases Ord p, HaskellExprExecutor p, AnnotateType p)
               => Subtyping
               -> Rewrites p
               -> UnificationM p s a
               -> BST.ST s (Either String a)
runUnificationM subtyping rewrites m =
      runExceptT
    $ flip evalStateT emptyUnificationState
    $ runReaderT m (emptyUnificationInfo subtyping rewrites)

-- | Run unification in the ST monad and return the result
runUnification :: forall p .
                  (ForAllPhases Ord p, HaskellExprExecutor p, HaskellExprRename p, AnnotateType p)
               => Subtyping
               -> Rewrites p
               -> PureTerm' p
               -> PureTerm' p
               -> Either String (Map String (PureTerm' p))
runUnification subtyping rewrites term1 term2 =
    BST.runST $ runUnificationM subtyping rewrites $ unify @p term1 term2
