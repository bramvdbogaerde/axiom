module Language.TemplateHaskell.SyntaxExtra (
    freeVars
) where

import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import Data.Set (Set)

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