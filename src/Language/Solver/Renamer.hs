{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Language.Solver.Renamer(
        renameRule
      , renameRule'
      , renameRewrite
      , renameTerm
      , unrenameTerm
      , runRenamer
      , renameRuleState
      , renameTermState
      , renameState
      , baseName
    ) where

import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Language.AST
import Data.Bifunctor
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)


-------------------------------------------------------------
-- Monadic infrastructure
-------------------------------------------------------------

data RenamerCtx = RenamerCtx {
                    -- | Mapping of existing variable names to their fresh name
                    _variableMapping :: Map String Int
                    -- | Counter for fresh variables
                  , _freshCtr :: Int
                }

$(makeLenses ''RenamerCtx)

-- | Creates an empty context
emptyCtx :: RenamerCtx
emptyCtx = RenamerCtx Map.empty 0

-- | Monadic context in which the rename functions are executed
type MonadRename m = (MonadState RenamerCtx m)

-- | Create a fresh variable counter based on the given existing name 
fresh :: MonadRename m => m Int
fresh = gets _freshCtr <* modify (over freshCtr (+1))

-- | Obtain the fresh name of a variable or return a new one if none exists
freshName :: MonadRename m => String -> m String
freshName nam =
    gets (Map.lookup nam . _variableMapping) >>= maybe storeFreshName (return . newName)
  where
    newName = ((nam ++ "_") ++) . show
    storeFreshName = fresh >>= \i -> modify (over variableMapping (Map.insert nam i)) >> return (newName i)

-- | Run the renamer monad and obtain its result
runRenamer :: Int -> StateT RenamerCtx Identity a -> (a, Int)
runRenamer freshCtr' m = runIdentity (second _freshCtr <$> runStateT m (emptyCtx { _freshCtr = freshCtr' }))

-- | Same as "runRenamer" but does not force Identity
runRenamer' :: Functor m => Int -> StateT RenamerCtx m a -> m (a, Int)
runRenamer' freshCtr' m = second _freshCtr <$> runStateT m (emptyCtx { _freshCtr = freshCtr' })

-------------------------------------------------------------
-- Renaming functions
-------------------------------------------------------------

-- | Rename all variables in a term using fresh names
renameTerm :: forall p m . (HaskellExprRename p, ForAllPhases Ord p, MonadRename m) => PureTerm' p -> m (PureTerm' p)
renameTerm (Atom (Identity varName) tpy range) = Atom . Identity <$> freshName varName <*> pure tpy <*> pure range
renameTerm (Functor name subterms tpy range) = Functor name <$> mapM renameTerm subterms <*> pure tpy <*> pure range
renameTerm (Neq left right tpy range) = Neq <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (Eqq left right tpy range) = Eqq <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (Transition transName left right tpy range) =
    Transition transName <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (HaskellExpr expr tpy range) =
            HaskellExpr
        <$> (haskellExprRename @p <$> mapping' <*> pure expr)
        <*> pure tpy
        <*> pure range
    where freeVars = Set.toList (haskellExprFreeVars @p expr)
          vars = mapM freshName freeVars
          mapping' = Map.fromList <$> liftA2 zip vars (return freeVars)

renameTerm (TermValue value tpy range) = pure $ TermValue value tpy range
renameTerm (IncludedIn var term range) = IncludedIn <$> freshName var <*> renameTerm term <*> pure range
renameTerm (SetOfTerms terms tpy range) = do
  renamedTermsList <- mapM renameTerm (Set.toList terms)
  return $ SetOfTerms (Set.fromList renamedTermsList) tpy range
renameTerm (TermHask value tpy range) = pure $ TermHask value tpy range
renameTerm (TermExpr expr range) = flip TermExpr range <$> renameExpr expr
renameTerm (TermMap mapping tpy range) =
    TermMap . Map.fromList <$> mapM (bimapM renameTerm renameTerm) (Map.toList mapping) <*> pure tpy <*> pure range
  where bimapM f g (x,y) = liftA2 (,) (f x) (g y)
renameTerm w@Wildcard {} = return w

renameExpr :: forall p m . (HaskellExprRename p, ForAllPhases Ord p, MonadRename m) => Expr p Identity -> m (Expr p Identity)
renameExpr = \case
    LookupMap t1 t2 tpy range ->
        LookupMap <$> renameTerm t1 <*> renameTerm t2 <*> pure tpy <*> pure range
    UpdateMap t1 t2 t3 tpy range ->
        UpdateMap <$> renameExpr t1 <*> renameTerm t2 <*> renameTerm t3 <*> pure tpy <*> pure range
    RewriteApp nam ags tpy range ->
        RewriteApp nam <$> mapM renameTerm ags <*> pure tpy <*> pure range
    e@(EmptyMap {}) -> return e
    GroundTerm t tpy range -> GroundTerm <$> renameTerm t <*> pure tpy <*> pure range
    SetUnion t1 t2 tpy range -> SetUnion <$> renameTerm t1 <*> renameTerm t2 <*> pure tpy <*> pure range

-- | Rename all variables in a list of terms
renameTerms :: (HaskellExprRename p, ForAllPhases Ord p, MonadRename m) => [PureTerm' p] -> m [PureTerm' p]
renameTerms = mapM renameTerm

-- | Rename the variable in the given rwrite rule, ensuring that they are unique
renameRewrite :: (HaskellExprRename p, ForAllPhases Ord p, MonadRename m) => PureRewriteDecl p -> m (PureRewriteDecl p)
renameRewrite (RewriteDecl nam prs bdy r) =
    RewriteDecl nam <$> renameTerms prs <*> renameTerm bdy <*> pure r

-------------------------------------------------------------
-- Public API
-------------------------------------------------------------



-- | Rename the variables in the given rule, ensuring that they are unique
renameRule' :: (HaskellExprRename p, ForAllPhases Ord p)
            => Int         -- ^ the number of currently allocated fresh variables
            -> RuleDecl' p  -- ^ the rule declaration to translate
            -> (RuleDecl' p, Int)
renameRule' freshCtr' (RuleDecl ruleName precedent consequent range) =
    runRenamer freshCtr' $
        RuleDecl ruleName <$> renameTerms precedent <*> renameTerms consequent <*> pure range
renameRule' freshCtr' (OnRuleDecl ruleName precedent consequent range) =
    runRenamer freshCtr' $
        OnRuleDecl ruleName <$> renameTerms precedent <*> renameTerms consequent <*> pure range

renameRuleState :: (HaskellExprRename p, ForAllPhases Ord p, Monad m) => RuleDecl' p -> StateT Int m (RuleDecl' p)
renameRuleState rule = StateT $ return . flip renameRule' rule

renameTermState :: (HaskellExprRename p, ForAllPhases Ord p, Monad m) => PureTerm' p -> StateT Int m (PureTerm' p)
renameTermState term = renameState (renameTerm term)

-- | Transform the renamer monad to a state monad with a unique variable counter
renameState :: Functor m => StateT RenamerCtx m a -> StateT Int m a
renameState m = StateT $ flip runRenamer' m

-- | Same as renameRule' but does not return the new unique count
renameRule :: (HaskellExprRename p, ForAllPhases Ord p) => Int -> RuleDecl' p -> RuleDecl' p
renameRule k = fst . renameRule' k

-------------------------------------------------------------
-- Base name functions
-------------------------------------------------------------

-- | Inverse of "renameTerm"
unrenameTerm :: (ForAllPhases Ord p) => PureTerm' p -> PureTerm' p
unrenameTerm (Atom (Identity varName) tpy range) = Atom (Identity (baseName varName)) tpy range
unrenameTerm (Functor name subterms tpy range) = Functor name (map unrenameTerm subterms) tpy range
unrenameTerm (Neq left right tpy range) = Neq (unrenameTerm left) (unrenameTerm right) tpy range
unrenameTerm (Eqq left right tpy range) = Eqq (unrenameTerm left) (unrenameTerm right) tpy range
unrenameTerm (Transition transName left right tpy range) =
    Transition transName (unrenameTerm left) (unrenameTerm right) tpy range
unrenameTerm (HaskellExpr expr tpy range) = HaskellExpr expr tpy range
unrenameTerm (TermValue value tpy range) = TermValue value tpy range
unrenameTerm (IncludedIn var term range) = IncludedIn (baseName var) (unrenameTerm term) range
unrenameTerm (SetOfTerms terms tpy range) = SetOfTerms (Set.fromList $ map unrenameTerm $ Set.toList terms) tpy range
unrenameTerm (TermHask value tpy range) = TermHask value tpy range
unrenameTerm (TermExpr expr range) = TermExpr (unrenameExpr expr) range
unrenameTerm (TermMap mapping tpy range) =
    TermMap (Map.fromList $ map (bimap unrenameTerm unrenameTerm) $ Map.toList mapping) tpy range
unrenameTerm w@(Wildcard {}) = w

unrenameExpr :: (ForAllPhases Ord p) => Expr p Identity -> Expr p Identity
unrenameExpr = \case
    LookupMap t1 t2 tpy range ->
        LookupMap (unrenameTerm t1) (unrenameTerm t2) tpy range
    UpdateMap t1 t2 t3 tpy range ->
        UpdateMap (unrenameExpr t1) (unrenameTerm t2) (unrenameTerm t3) tpy range
    RewriteApp nam ags tpy range ->
        RewriteApp nam (map unrenameTerm ags) tpy range
    e@(EmptyMap {}) -> e
    GroundTerm t tpy range -> GroundTerm (unrenameTerm t) tpy range
    SetUnion t1 t2 tpy range -> SetUnion (unrenameTerm t1) (unrenameTerm t2) tpy range

-- | Removes the part of the variable name that makes it unique
baseName :: String -> String
baseName s = fromMaybe s (safeVariableName s)

