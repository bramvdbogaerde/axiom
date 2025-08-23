{-# LANGUAGE ConstraintKinds #-}
module Language.Solver.Renamer(renameRule, renameRule', renameRuleState) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Functor.Identity
import Language.AST
import Data.Bifunctor


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
emptyCtx = RenamerCtx Map.empty 0

-- | Monadic context in which the rename functions are executed
type MonadRename m = (MonadState RenamerCtx m)

-- | Create a fresh variable counter based on the given existing name 
fresh :: MonadRename m => String -> m Int
fresh nam = gets _freshCtr <* modify (over freshCtr (+1))

-- | Obtain the fresh name of a variable or return a new one if none exists
freshName :: MonadRename m => String -> m String
freshName nam =
    gets (Map.lookup nam . _variableMapping) >>= maybe storeFreshName (return . newName)
  where
    newName = (nam ++) . show
    storeFreshName = fresh nam >>= \i -> modify (over variableMapping (Map.insert nam i)) >> return (newName i)

-- | Run the renamer monad and obtain its result
runRenamer :: Int -> StateT RenamerCtx Identity a -> (a, Int)
runRenamer freshCtr' m = runIdentity (second _freshCtr <$> runStateT m (emptyCtx { _freshCtr = freshCtr' }))

-------------------------------------------------------------
-- Renaming functions
-------------------------------------------------------------

-- | Rename all variables in a term using fresh names
renameTerm :: MonadRename m => PureTerm' p -> m (PureTerm' p)
renameTerm (Atom (Identity varName) tpy range) = Atom . Identity <$> freshName varName <*> pure tpy <*> pure range
renameTerm (Functor name subterms tpy range) = Functor name <$> mapM renameTerm subterms <*> pure tpy <*> pure range
renameTerm (Neq left right tpy range) = Neq <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (Eqq left right tpy range) = Eqq <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (Transition transName left right tpy range) =
    Transition transName <$> renameTerm left <*> renameTerm right <*> pure tpy <*> pure range
renameTerm (HaskellExpr expr tpy range) = pure $ HaskellExpr expr tpy range
renameTerm (TermValue value tpy range) = pure $ TermValue value tpy range

-- | Rename all variables in a list of terms
renameTerms :: MonadRename m => [PureTerm' p] -> m [PureTerm' p]
renameTerms = mapM renameTerm

-------------------------------------------------------------
-- Public API
-------------------------------------------------------------

-- | Rename the variables in the given rule, ensuring that they are unique
renameRule' :: Int         -- ^ the number of currently allocated fresh variables
           -> RuleDecl' p  -- ^ the rule declaration to translate
           -> (RuleDecl' p, Int)
renameRule' freshCtr' (RuleDecl ruleName precedent consequent range) =
    runRenamer freshCtr' $
        RuleDecl ruleName <$> renameTerms precedent <*> renameTerms consequent <*> pure range

renameRuleState :: Monad m => RuleDecl' p -> StateT Int m (RuleDecl' p)
renameRuleState rule = StateT $ return . flip renameRule' rule


-- | Same as renameRule' but does not return the new unique count
renameRule :: Int -> RuleDecl' p -> RuleDecl' p
renameRule k = fst . renameRule' k
