module Language.TemplateHaskell.SyntaxExtra (
    freeVars,
    mapSuffixedTypes
) where

import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import Data.Set (Set)
import Data.List (isSuffixOf)
import Control.Monad

-- | Extract free variables from a Template Haskell expression
freeVars :: Exp -> Set String
freeVars expr = case expr of
  VarE name -> Set.singleton (nameBase name)
  ConE _ -> Set.empty
  LitE _ -> Set.empty
  AppE e1 e2 -> freeVars e1 `Set.union` freeVars e2
  InfixE me1 e me2 ->
    maybe Set.empty freeVars me1 `Set.union`
    freeVars e `Set.union`
    maybe Set.empty freeVars me2
  UInfixE e1 e2 e3 -> freeVars e1 `Set.union` freeVars e2 `Set.union` freeVars e3
  ParensE e -> freeVars e
  LamE pats e -> freeVars e `Set.difference` Set.fromList (concatMap patVars pats)
  TupE mexps -> Set.unions (map (maybe Set.empty freeVars) mexps)
  UnboxedTupE mexps -> Set.unions (map (maybe Set.empty freeVars) mexps)
  CondE e1 e2 e3 -> freeVars e1 `Set.union` freeVars e2 `Set.union` freeVars e3
  ListE exps -> Set.unions (map freeVars exps)
  SigE e _ -> freeVars e
  RecConE _ fields -> Set.unions (map (freeVars . snd) fields)
  RecUpdE e fields -> freeVars e `Set.union` Set.unions (map (freeVars . snd) fields)
  StaticE e -> freeVars e
  UnboundVarE name -> Set.singleton (nameBase name)
  GetFieldE e _ -> freeVars e
  ProjectionE _ -> Set.empty
  -- For other constructors, return empty set (conservative approach)
  _ -> Set.empty

-- | Extract variable names from patterns
patVars :: Pat -> [String]
patVars pat = case pat of
  LitP _ -> []
  VarP name -> [nameBase name]
  TupP pats -> concatMap patVars pats
  UnboxedTupP pats -> concatMap patVars pats
  ConP _ _ pats -> concatMap patVars pats
  InfixP p1 _ p2 -> patVars p1 ++ patVars p2
  UInfixP p1 _ p2 -> patVars p1 ++ patVars p2
  ParensP p -> patVars p
  TildeP p -> patVars p
  BangP p -> patVars p
  AsP name p -> nameBase name : patVars p
  WildP -> []
  RecP _ fields -> concatMap (patVars . snd) fields
  ListP pats -> concatMap patVars pats
  SigP p _ -> patVars p
  ViewP _ p -> patVars p
  -- For other constructors, return empty list (conservative approach)
  _ -> []

-- | Map over type names ending with `suffix` from the given type constructor
mapSuffixedTypes :: Monad m => String -> (Type -> m Type) -> Type -> m Type
mapSuffixedTypes suffix f typ = case typ of
  ConT name -> if suffix `isSuffixOf` nameBase name then f typ else return typ
  VarT name -> return (VarT name)
  AppT t1 t2 -> liftM2 AppT (mapSuffixedTypes suffix f t1) (mapSuffixedTypes suffix f t2)
  SigT t k -> fmap (`SigT` k) (mapSuffixedTypes suffix f t)
  InfixT t1 name t2 -> liftM2 (`InfixT` name) (mapSuffixedTypes suffix f t1) (mapSuffixedTypes suffix f t2)
  UInfixT t1 name t2 -> liftM2 (`UInfixT` name) (mapSuffixedTypes suffix f t1) (mapSuffixedTypes suffix f t2)
  ParensT t -> fmap ParensT (mapSuffixedTypes suffix f t)
  TupleT n -> return (TupleT n)
  UnboxedTupleT n -> return (UnboxedTupleT n)
  ArrowT -> return ArrowT
  EqualityT -> return EqualityT
  ListT -> return ListT
  PromotedTupleT n -> return (PromotedTupleT n)
  PromotedNilT -> return PromotedNilT
  PromotedConsT -> return PromotedConsT
  StarT -> return StarT
  ConstraintT -> return ConstraintT
  LitT lit -> return (LitT lit)
  WildCardT -> return WildCardT
  PromotedT name -> return (PromotedT name)
  ImplicitParamT str t -> fmap (ImplicitParamT str) (mapSuffixedTypes suffix f t)
  ForallT tvbs cxt t -> liftM2 (ForallT tvbs) (mapM (mapSuffixedTypes suffix f) cxt) (mapSuffixedTypes suffix f t)
  ForallVisT tvbs t -> fmap (ForallVisT tvbs) (mapSuffixedTypes suffix f t)
  AppKindT t k -> fmap (`AppKindT` k) (mapSuffixedTypes suffix f t)
  PromotedInfixT t1 name t2 -> liftM2 (`PromotedInfixT` name) (mapSuffixedTypes suffix f t1) (mapSuffixedTypes suffix f t2)
  PromotedUInfixT t1 name t2 -> liftM2 (`PromotedUInfixT` name) (mapSuffixedTypes suffix f t1) (mapSuffixedTypes suffix f t2)
  UnboxedSumT n -> return (UnboxedSumT n)
  MulArrowT -> return MulArrowT
