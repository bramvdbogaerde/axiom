{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE TemplateHaskell #-}
module Latex.Generator where

import Language.AST
import Text.Printf
import Data.Kind
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Functor.Identity
import Control.Monad.State
import Control.Lens
import qualified Latex.Output 
import Latex.Output hiding (Syntax)


-- | State for rendering with a counter for generating unique names
data RenderState = RenderState
  { _output :: LatexOutput
  , _counter :: Int
  }

$(makeLenses ''RenderState)

type MonadRender (m :: Type -> Type) = (MonadState RenderState m)

-- | Get a fresh unique name for an unnamed block
freshName :: MonadRender m => LatexType -> m String
freshName latexType = do
  n <- use counter
  counter += 1
  pure $ prefix ++ show n
  where
    prefix = case latexType of
      Latex.Output.Syntax -> "syntax"
      Rules -> "rules"
      Rewrites -> "rewrites"


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
  if null productions
    then printf "%s \\in %s" varsStr <$> renderTpy tpy
    else printf "%s \\in %s ::= %s" varsStr <$> renderTpy tpy <*> productionStrs
  where
    varsStr = intercalate ", " (map renderIdentifier vrs)
    productionStrs = intercalate " \\mid " <$> traverse renderTerm productions
renderSyntax (TypeAliasDecl name tpy _) =
  printf "\\textbf{alias } %s = %s" (renderIdentifier name) <$> renderTpy tpy
    
-- | Render a single rule using mathpartir
renderRule :: MonadRender m => RuleDecl -> m String
renderRule (RuleDecl name precedents consequents _) =
  printf "\\inferrule{%s}{%s}%s"
    <$> (intercalate " \\\\ " <$> traverse renderTerm precedents)
    <*> (intercalate " \\\\ " <$> traverse renderTerm consequents)
    <*> pure ruleName
  where
    ruleName = if null name then "" else printf "{\\scriptsize \\textsc{%s}}" name


-- | Renders a program to the latex output
renderProgram :: MonadRender m => Program -> m ()
renderProgram (Program decls _) = mapM_ renderDecl decls

-- | Renders a single declaration
renderDecl :: MonadRender m => Decl -> m ()
renderDecl (Syntax name syntaxDecls _) = do
  rendered <- intercalate "\n\n" <$> traverse renderSyntax syntaxDecls
  blockName <- maybe (freshName Latex.Output.Syntax) pure name
  output %= addBlock blockName Latex.Output.Syntax rendered
renderDecl (RulesDecl name ruleDecls _) = do
  rendered <- intercalate "\n\n" <$> traverse renderRule ruleDecls
  blockName <- maybe (freshName Rules) pure name
  output %= addBlock blockName Rules rendered
renderDecl _ = pure ()

-- | Run the generator on a program to produce LaTeX output
runGenerator :: Program -> LatexOutput
runGenerator prog = _output $ execState (renderProgram prog) initialState
  where
    initialState = RenderState (LatexOutput mempty) 0


