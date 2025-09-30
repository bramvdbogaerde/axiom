{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
module Latex.Generator where

import Language.AST
import Text.Printf
import Data.Kind
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Functor.Identity


type MonadRender (m :: Type -> Type) = Applicative m

-- | Renders an identifier properly for LaTeX math mode
-- Single letters are rendered as-is, multi-letter identifiers use \mathit
renderIdentifier :: String -> String
renderIdentifier name
  | length name == 1 = name
  | otherwise = printf "\\mathit{%s}" name

-- | Renders a type constructor
renderTpy :: MonadRender m => TypeCon -> m String
renderTpy (TypeTyp name _) = pure $ renderIdentifier name
renderTpy (TypeVar var _) = pure $ renderIdentifier var
renderTpy (TypeHas hask _) = pure $ printf "\\texttt{%s}" hask
renderTpy (TypeApp ctor args _) = 
  printf "%s(%s)" <$> renderTpy ctor <*> (intercalate ", " <$> traverse renderTpy args)

-- | Renders a term for LaTeX output
renderTerm :: MonadRender m => PureTerm' p -> m String
renderTerm (Atom (Identity name) _ _) = pure $ renderIdentifier name
renderTerm (Functor fname [] _ _) = pure $ printf "%s()" (renderIdentifier fname)
renderTerm (Functor fname args _ _) = 
  printf "%s(%s)" (renderIdentifier fname) . intercalate ", " <$> traverse renderTerm args
renderTerm (Eqq left right _ _) = 
  printf "%s = %s" <$> renderTerm left <*> renderTerm right
renderTerm (Neq left right _ _) = 
  printf "%s \\neq %s" <$> renderTerm left <*> renderTerm right
renderTerm (Transition tname left right _ _) = 
  printf "%s \\xrightarrow{%s} %s" <$> renderTerm left <*> pure (renderIdentifier tname) <*> renderTerm right
renderTerm (TermValue value _ _) = pure $ show value
renderTerm (SetOfTerms terms _ _) = 
  printf "\\{%s\\}" . intercalate ", " <$> traverse renderTerm (Set.toList terms)
renderTerm _ = pure "\\text{complex term}"

-- | Renders a single syntax definition
renderSyntax :: MonadRender m => SyntaxDecl -> m String
renderSyntax (SyntaxDecl vrs tpy productions _) = 
  printf "%s \\in %s ::= %s" varsStr <$> renderTpy tpy <*> productionStrs
  where
    varsStr = intercalate ", " (map renderIdentifier vrs)
    productionStrs = intercalate " \\mid " <$> traverse renderTerm productions

