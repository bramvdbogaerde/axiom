{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Language.TypeCheck where

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
import Control.Monad
import Data.Kind
import Data.Maybe (fromMaybe)

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

-- | Returns the sorts beloning to the given data constructor
ctorSorts :: DataCtor -> [SortName]
ctorSorts = \case DataAtom s -> [s]
                  DataTagged _ ss -> ss

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
                | SortNotDefined String
                | IncompatibleTypes [SortName] [SortName]
                deriving (Ord, Eq, Show)

data Error = Error { err :: ModelError, raisedAt :: Maybe Range,  ctx :: Context }
           deriving (Ord, Eq, Show)


-----------------------------------------
-- Context typing
-----------------------------------------

-- | A typing context used when parsing the syntax rules, and for checking
-- rewrite and transition rules.
data Context = Context { _atomToSorts      :: Map String SortName, -- ^ mapping from data atoms (in variables or datactors) to their sorts
                         _functorToCtor    :: Map String DataCtor,
                         _sorts            :: Map String Sort,     -- ^ a mapping from a sort name to their sort data structure
                         _supertypes       :: Map SortName (Set SortName),
                        -- | A mapping from sort names to the location of their definition (for debugging and error messaging)
                         _sortToDefSite    :: Map String (Maybe Range)
                       } deriving (Ord, Eq, Show)


$(makeLenses ''Context)

-- | Create an empty context
emptyContext :: Context
emptyContext = Context Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Monad for modifying and tracking the typing context
type MonadTy m = (MonadState Context m, MonadError Error m)

-- | Throw an error by adding the current context to it
throwError :: MonadTy m => ModelError -> m a
throwError err = get >>= Except.throwError . Error err Nothing

-- | Throw an error at the given location in the sourcez by adding the current context to it 
throwErrorAt :: MonadTy m => Range -> ModelError -> m a
throwErrorAt range err = get >>= Except.throwError . Error err (Just range)

-- | Associate a single variable to the given type or return
-- an error if there is already a type associated with the given variable.
assocAtomToSort :: MonadTy m => SortName -> Var -> m ()
assocAtomToSort sortName var = do
  alreadyDefined <- gets (Map.member var . _atomToSorts)
  if alreadyDefined
    then throwError (DuplicateVariable var sortName)
    else modify (over atomToSorts (Map.insert var sortName))
    
-- | Check that the given sort exists
assertSortDefinedAt :: MonadTy m => Range -> String -> m ()
assertSortDefinedAt range sortName =
      gets (Map.member sortName . _sorts)
  >>= (\b -> if b then return () else throwErrorAt range (SortNotDefined sortName))

-- | Finds the sort associated with the given variable
resolveSort :: (MonadState Context m, MonadError Error m) => String -> m SortName
resolveSort varName = gets (Map.lookup varName . _atomToSorts)
                  >>= maybe (throwError (NoSuchSort varName)) return

resolveSortAt :: (MonadState Context m, MonadError Error m) => Range -> String -> m SortName
resolveSortAt range varName = gets (Map.lookup varName . _atomToSorts)
                  >>= maybe (throwErrorAt range (NoSuchSort varName)) return

-- | Return a list of sorts associated with the functor
sortsInFunctor :: (MonadState Context m, MonadError Error m) => String -> m [SortName]
sortsInFunctor functorName = do
  functor <- gets (Map.lookup functorName . _functorToCtor) >>= maybe (throwError $ NoSuchSort functorName) return
  case functor of
    DataAtom sortName   -> return [sortName]
    DataTagged _ sorts  -> return sorts

-- | Registers the first argument as a subtype of the second
registerSubtype :: MonadTy m => SortName -> SortName -> m ()
registerSubtype subtype parenttype = modify (over supertypes (Map.insertWith Set.union subtype (Set.singleton parenttype)))

-- | Produces and associated the data constructors in the given sort
makeCtor :: MonadTy m => SortName -> Term -> m DataCtor
makeCtor sortName = \case (Atom nam range) -> do
                              sort <- resolveSortAt range nam
                              registerSubtype sort sortName
                              return $ DataAtom sort
                          (Functor nam ts range) -> do
                            assocAtomToSort sortName nam
                            sorts <- mapM (\t -> collectAtoms t >>=resolveSortAt (rangeOf t)) ts
                            let ctor = DataTagged nam sorts
                            modify (over functorToCtor (Map.insert nam ctor))
                            return ctor
  where collectAtoms (Atom nam _) = return nam
        collectAtoms t = throwErrorAt (rangeOf t) $ NoNestingAt sortName


-- | Add a new sort to the type system 
addSort :: MonadTy m
        => String      -- ^ name of the sort
        -> Maybe Range -- ^ the definition site of the sort (if available)
        -> Sort        -- ^ the sort to define
        -> m ()
addSort tpy range sort = do
  modify (over sorts (Map.insert tpy sort))
  modify (over sortToDefSite (Map.insert tpy range))

-- | Add the contents of a single syntax rule Context to the typing context 
addSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
addSyntaxRule (SyntaxDecl vars tpy ctors range) = do
  -- construct the data constructors from ctors
  ctors <- mapM (makeCtor (SortName tpy)) ctors
  let sort = Sort tpy (Set.fromList vars) (Set.fromList ctors)
  -- finally, insert the sort in the sort map and mark it as resolved
  alreadyDefined <- gets (Map.member tpy . _sorts)
  if alreadyDefined
    then throwError (DuplicateSort (SortName tpy))
    else addSort tpy (Just range) sort

-- | Associates the sorts with the variables in the syntax rule or raises an error
-- if the variables was already defined in another rule.
assocSyntaxRule :: MonadTy m => SyntaxDecl -> m ()
assocSyntaxRule (SyntaxDecl vars tpy ctors _) = do
  mapM_ (assocAtomToSort (SortName tpy)) vars

-- | Traverse the program's declarations for constructing a context
traverseDecls :: MonadTy m => [Decl] -> m ()
traverseDecls = mapM_ traverseDecl
  where traverseDecl (Syntax decls _) = mapM_ addSyntaxRule decls
        traverseDecl (TransitionDecl nam from to range) = do
          -- Effectively:
          -- transition State ~> State
          -- is equivalent to the syntax rule:
          -- ~> ::= ~>(state, state)
          -- from a typing perspective
          let ctor = DataTagged nam [SortName (fst from), SortName (fst to)]
          let sort = Sort nam Set.empty (Set.singleton ctor)
          addSort nam (Just range) sort
          assocAtomToSort (SortName nam) nam
          modify (over functorToCtor (Map.insert nam ctor))
        traverseDecl _ = return  ()

-- | Derive the typing context from the given program, this process proceeds in two passes:
-- - The first pass traverses the entire tree and associates variables with sorts
-- - The next pass traverses the entire tree again and constructs the data constructors provided in the syntax  
deriveCtx :: MonadTy m => [Decl] -> m ()
deriveCtx decls = mapM_ traverseDecl decls >> traverseDecls decls
  where traverseDecl (Syntax decls _) = mapM_ assocSyntaxRule decls
        traverseDecl _ = return ()

-----------------------------------------
-- Subtyping
-----------------------------------------

class SubtypeOf s where
  subtypeOf :: MonadTy m => s -> s -> m Bool

instance SubtypeOf DataCtor where
  subtypeOf (DataAtom nam1) (DataAtom nam2) = nam1 `subtypeOf` nam2
  subtypeOf (DataTagged nam1 s1) (DataTagged nam2 s2) =
    (nam1 == nam2 &&) <$> s1 `subtypeOf` s2

instance SubtypeOf s => SubtypeOf [s] where
  subtypeOf l1 l2 = and <$> zipWithM subtypeOf l1 l2

instance SubtypeOf SortName where
  subtypeOf s1 s2 =
    gets _supertypes >>= runReaderT (evalStateT (findPath s1 s2) Map.empty)

-- | Checks if there is a path between the subtype and the supertype in the typing graph
findPath :: (MonadReader (Map SortName (Set SortName)) m,
             MonadState (Map SortName Bool) m)
         => SortName -> SortName -> m Bool
findPath from to =
  if from == to then return True
  else do
    alreadyVisited <- gets (fromMaybe False . Map.lookup from)
    if alreadyVisited
      then return False
      else asks (fromMaybe Set.empty . Map.lookup from) >>= (fmap or .  mapM (`findPath` to) . Set.toList)

-----------------------------------------
-- Type inference reduction rules
-----------------------------------------

-- TODO: check whether all variables are in the head of the rewrite rule
-- | Infer the types of the arguments in reduction rules
inferProgram :: MonadTy m => [Decl] -> m ()
inferProgram  = mapM_ visitDecl
  where visitDecl (Rewrite (RewriteDecl nam args bdy range) _) = do
          sorts <- mapM checkTerm args
          bdySort <- checkTerm bdy
          let ctor = DataTagged nam sorts
          -- track the rewrite rule, either by updating its data constructors,
          -- or checking whether existing data constructors are the same as the
          -- current one.
          existingSort <- gets (Map.lookup nam . _sorts)
          winningCtor <- maybe (createSort nam ctor) (checkSame range ctor) existingSort
          modify (over functorToCtor (Map.insert nam winningCtor))
          -- type the rewrite functor as the sort of its body,
          -- unless it was already typed before.
          alreadyTyped <- gets (Map.lookup nam . _atomToSorts)
          -- check whether the existing type matches
          case alreadyTyped of
            -- TODO: widening
            Just existingType -> subtypeOf bdySort existingType
                             >>= (\r -> if r then return () else throwErrorAt (rangeOf bdy) $ IncompatibleTypes [existingType] [bdySort])
            Nothing -> assocAtomToSort bdySort nam
        visitDecl _ = return ()
        createSort nam ctor  = do
          modify (over sorts (Map.insert nam $ Sort nam Set.empty (Set.singleton ctor)))
          return ctor
        checkSame range ctor (Sort nam vrs ctors)  = do
          -- ASSUMPTION: the set of ctors is a singleton (by construction)
          let ctor' = head $ Set.toList ctors
          -- is the new rule a subtype of the old rule
          isSubset <- subtypeOf ctor ctor'
          -- is the old rule a subtype of the new rule 
          isSubsetOp <- subtypeOf ctor' ctor
          -- if neither isSubset or isSubsetOp is true then
          -- the types are incompatible
          if not (isSubset || isSubsetOp)
            then throwErrorAt range (IncompatibleTypes (ctorSorts ctor') (ctorSorts ctor))
            else if isSubsetOp
                    -- widen if the new type is broader than the old one
                    then modify (over sorts (Map.insert nam $ Sort nam Set.empty (Set.singleton ctor))) >> return ctor
                    else return ctor'


-----------------------------------------
-- Type checking
-----------------------------------------

-- | Check an individual term and returns its sort
checkTerm :: MonadTy m => Term -> m SortName
checkTerm (Atom nam range)       = resolveSortAt range $ variableName nam
checkTerm (Functor tpy ts range) = do
    ts' <- mapM checkTerm ts
    expected <- sortsInFunctor tpy
    functorSort <- resolveSortAt range tpy
    ok <- and <$> zipWithM subtypeOf ts' expected
    if ok
      then return functorSort
      else throwErrorAt range $ IncompatibleTypes expected ts' 
checkTerm (Eqq t1 t2 range)      = do
    t1' <- checkTerm t1
    t2' <- checkTerm t2
    if t1' /= t2'
      then throwErrorAt range (IncompatibleTypes [t1'] [t2'])
      else return t1'
checkTerm (Transition transitionName t1 t2 range) =
  --  transitions are registered in the typing context
  -- as sorts with a single data constructor and no
  -- variables associated with it (so that they cannot be used as atoms), hence we can check them as if they were functors
  checkTerm (Functor transitionName [t1, t2] range)

-- | Check a rule in the given context and add it to the model
checkRule :: MonadTy m => RuleDecl -> m ()
checkRule rule@(RuleDecl nam precedent consequent _) = do
    mapM_ checkTerm (precedent ++ consequent)

-- | Check all declarations in a program
checkProgram :: MonadTy m => [Decl] -> m ()
checkProgram = mapM_ visitDecl
  where visitDecl (RulesDecl rules _) = mapM_ checkRule rules
        visitDecl (TransitionDecl _ (fromSort, r1) (toSort, r2) range) = do
          assertSortDefinedAt r1 fromSort
          assertSortDefinedAt r2 toSort 
        visitDecl _ = return ()

-----------------------------------------
-- Entrypoint
-----------------------------------------

-- | Run the type checker in its entirety
runChecker :: Program -> Either Error Context
runChecker (Program decls) =
   execStateT (deriveCtx decls >> inferProgram decls >> checkProgram decls) emptyContext

