{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
module Language.Model where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State
import Control.Lens hiding (Context)
import Language.AST
import Control.Monad.Except hiding (throwError)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader

-----------------------------------------
-- Model datatypes
-----------------------------------------


-- | Names to refer to sorts
newtype SortName = SortName { getSortName :: String } deriving (Ord, Eq, Show)

-- | Variables
type Var = String

-- | A data constructor is either an atom or a tag paired with a list of sorts
data DataCtor = DataAtom SortName | DataTagged String [SortName]
              deriving (Ord, Eq, Show)

-- | A sort consists of a set of data constructors, a name of the sort and
-- the variables associated with that sort.
data Sort = Sort String (Set Var) (Set DataCtor)
          deriving (Ord, Eq, Show)

-----------------------------------------
-- Errors
-----------------------------------------

data ModelError = DuplicateVariable String SortName
                | DuplicateSort SortName
                | NoNestingAt SortName
                | NoSuchSort Var
                deriving (Ord, Eq, Show)

data Error = Error { err :: ModelError, ctx :: Context }
           deriving (Ord, Eq, Show)

-----------------------------------------
-- Context typing
-----------------------------------------

-- | A typing context used when parsing the syntax rules, and for checking
-- rewrite and transition rules.
data Context = Context { _atomToSorts      :: Map String SortName, -- ^ mapping from data atoms (in variables or datactors) to their sorts
                         _functorToCtor    :: Map String DataCtor,
                         _sorts            :: Map String Sort      -- ^ a mapping from a sort name to their sort data structure
                       } deriving (Ord, Eq, Show)


$(makeLenses ''Context)

-- | Create an empty context
emptyContext :: Context
emptyContext = Context Map.empty Map.empty Map.empty

-- | Monad for modifying and tracking the typing context
type MonadTy m = (MonadState Context m, MonadError Error m)

-- | Throw an error by adding the current context to it
throwError :: MonadTy m => ModelError -> m a
throwError err = get >>= Except.throwError . Error err

-- | Associate a single variable to the given type or return
-- an error if there is already a type associated with the given variable.
assocAtomToSort :: MonadTy m => SortName -> Var -> m ()
assocAtomToSort sortName var = do
  alreadyDefined <- gets (Map.member var . _atomToSorts)
  if alreadyDefined
    then throwError (DuplicateVariable var sortName)
    else modify (over atomToSorts (Map.insert var sortName))

-- | Founds the sort associated with the given variable
resolveSort :: MonadTy m => String -> m SortName
resolveSort varName = gets (Map.lookup varName . _atomToSorts)
                  >>= maybe (throwError (NoSuchSort varName)) return

-- | Produces and associated the data constructors in the given sort
makeCtor :: MonadTy m => SortName -> Term -> m DataCtor
makeCtor sortName = \case (Atom nam) -> DataAtom <$> resolveSort nam  -- TODO: perhaps we also want to list non-functor items in a data constructor that do not refer to a sort but define their thing or are elements of another sort? 
                          (Functor nam ts) -> do
                            assocAtomToSort sortName nam
                            sorts <- mapM (collectAtoms >=> resolveSort) ts
                            return $ DataTagged nam sorts
  where collectAtoms (Atom nam) = return nam
        collectAtoms _ = throwError $ NoNestingAt sortName

-- | Add the contents of a single syntax rule Context to the typing context 
addSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
addSyntaxRule (SyntaxDecl vars tpy ctors) = do
  -- construct the data constructors from ctors
  ctors <- mapM (makeCtor (SortName tpy)) ctors
  let sort = Sort tpy (Set.fromList vars) (Set.fromList ctors)
  -- finally, insert the sort in the sort map and mark it as resolved
  alreadyDefined <- gets (Map.member tpy . _sorts)
  if alreadyDefined
    then throwError (DuplicateSort (SortName tpy))
    else modify (over sorts (Map.insert tpy sort)) 

-- | Associates the sorts with the variables in the syntax rule or raises an error
-- if the variables was already defined in another rule.
assocSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
assocSyntaxRule (SyntaxDecl vars tpy ctors) = do
  mapM_ (assocAtomToSort (SortName tpy)) vars 

-- | Traverse the program's declarations for constructing a context
traverseDecls :: MonadTy m => [Decl] -> m ()
traverseDecls = mapM_ traverseDecl
  where traverseDecl (Syntax decls) = mapM_ addSyntaxRule decls
        traverseDecl _ = return  ()

-- | Derive the typing context from the given program, this process proceeds in two passes:
-- - The first pass traverses the entire tree and associates variables with sorts
-- - The next pass traverses the entire tree again and constructs the data constructors provided in the syntax  
deriveCtx :: MonadTy m => [Decl] -> m ()
deriveCtx decls = mapM_ traverseDecl decls >> traverseDecls decls
  where traverseDecl (Syntax decls) = mapM_ assocSyntaxRule decls
        traverseDecl _ = return ()

-----------------------------------------
-- Type checking
-----------------------------------------

type MonadCheck m = (MonadReader Context m)


checkProgram :: MonadCheck m => [Decl] -> m ()
checkProgram = undefined

-----------------------------------------
-- The model itself
-----------------------------------------

data Model = Model { tpyingContext :: Context } deriving (Ord, Eq, Show)

-----------------------------------------
-- Entrypoint
-----------------------------------------

-- Test for debugging
testContext :: Program -> Either Error Context
testContext (Program decls) =
  execStateT (deriveCtx decls) emptyContext  

fromProgram :: Program -> Model
fromProgram = undefined
