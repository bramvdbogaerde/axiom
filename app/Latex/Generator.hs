{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Latex.Generator where

import Data.Proxy
import Language.AST
import Language.Types
import Text.Printf
import Data.Kind
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Functor.Identity
import Control.Monad.State
import Control.Lens
import qualified Latex.Output
import Latex.Output hiding (Syntax)
import Latex.Unicode (replaceUnicodeInString)
import qualified Data.Map.Strict as Map


-- | Key for looking up custom LaTeX rendering rules
-- Either an atom name or a functor name
type RenderKey = String

-- | A rendering rule with both pattern and template
data RenderRule p = RenderRule (PureTerm' p) [LatexTemplateElement]

-- | Mapping from atoms/functors to their custom LaTeX rendering rules
type RenderRules p = Map.Map RenderKey (RenderRule p)

-- | State for rendering with a counter for generating unique names
data RenderState p = RenderState
  { _output :: LatexOutput
  , _counter :: Int
  , _renderRules :: RenderRules p
  }

$(makeLenses ''RenderState)

type MonadRender p (m :: Type -> Type) = (MonadState (RenderState p) m, AnnotateType p)

-- | Get a fresh unique name for an unnamed block
freshName :: MonadRender p m => LatexType -> m String
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
-- Unicode symbols are converted to their LaTeX equivalents
renderIdentifier :: String -> String
renderIdentifier name
  | length name == 1 = replaceUnicodeInString name
  | otherwise = printf "\\mathit{%s}" (replaceUnicodeInString name)


renderFunctorName  :: String -> String
renderFunctorName name
  | length name == 1 = replaceUnicodeInString name
  | otherwise = printf "\\mathsf{%s}" (replaceUnicodeInString name)

-- | Renders a type constructor
renderTpy :: MonadRender p m => TypeCon -> m String
renderTpy (TypeTyp name _) = pure $ renderIdentifier name
renderTpy (TypeVar var _) = pure $ renderIdentifier var
renderTpy (TypeHas hask _) = pure $ printf "\\texttt{%s}" hask
renderTpy (TypeApp ctor args _) =
  printf "%s(%s)" <$> renderTpy ctor <*> (intercalate ", " <$> traverse renderTpy args)

-- | Extract custom rendering rules from the program
extractRenderRules :: [Decl' p] -> RenderRules p
extractRenderRules decls = Map.fromList $ concatMap extractFromDecl decls
  where
    extractFromDecl (LatexRenderDecl rules _) = map extractFromRule rules
    extractFromDecl _ = []

    extractFromRule (LatexRenderRule pattern template _) =
      case pattern of
        Atom (Identity name) _ _ -> (name, RenderRule pattern template)
        Functor fname _ _ _ -> (fname, RenderRule pattern template)
        _ -> error "Invalid LaTeX render pattern"

-- | Match a term against a pattern and extract variable bindings
matchPattern :: PureTerm' p -> PureTerm' p -> Maybe (Map.Map String (PureTerm' p))
matchPattern (Atom (Identity patVar) _ _) term = Just $ Map.singleton patVar term
matchPattern (Functor patName patArgs _ _) (Functor termName termArgs _ _)
  | patName == termName && length patArgs == length termArgs =
      -- Match each argument and merge the bindings
      fmap Map.unions $ sequence $ zipWith matchPattern patArgs termArgs
  | otherwise = Nothing
matchPattern _ _ = Nothing

-- | Render a term using a custom template with variable bindings
renderWithTemplate :: MonadRender p m => [LatexTemplateElement] -> Map.Map String (PureTerm' p) -> m String
renderWithTemplate template bindings = do
  parts <- mapM renderTemplateElement template
  return $ concat parts
  where
    renderTemplateElement (LatexString str _) = pure str
    renderTemplateElement (LatexArg argName _) =
      case Map.lookup argName bindings of
        Just boundTerm -> renderTerm boundTerm  -- Recursively render the bound term
        Nothing -> error $ "Internal error: argument '" ++ argName ++ "' not found in bindings. Type checker should have caught this."

-- | Renders a term for LaTeX output
renderTerm :: MonadRender p m => PureTerm' p -> m String
renderTerm term@(Atom (Identity name) _ _) = do
  rules <- use renderRules
  let (baseName, numName) = variableNameSplit name
  let maybeRendered = do
        RenderRule pattern template <- Map.lookup baseName rules
        bindings <- matchPattern pattern term
        return (template, bindings)
  maybe (pure $ renderIdentifier $ suffixNumPart  baseName numName) (uncurry renderWithTemplate) maybeRendered


renderTerm term@(Functor fname args _ _) = do
  rules <- use renderRules
  let maybeRendered = do
        RenderRule pattern template <- Map.lookup fname rules
        bindings <- matchPattern pattern term
        return (template, bindings)
  maybe defaultRender (uncurry renderWithTemplate) maybeRendered
  where
    defaultRender = if null args
      then pure $ printf "%s()" (renderFunctorName fname)
      else printf "%s(%s)" (renderFunctorName fname) . intercalate ", " <$> traverse renderTerm args

renderTerm (Eqq left right _ _) =
  printf "%s = %s" <$> renderTerm left <*> renderTerm right
renderTerm (Neq left right _ _) =
  printf "%s \\neq %s" <$> renderTerm left <*> renderTerm right
renderTerm (Transition tname left right _ _) =
  printf "%s \\xrightarrow{%s} %s" <$> renderTerm left <*> pure (renderIdentifier tname) <*> renderTerm right
renderTerm (TermValue value _ _) = pure $ show value
renderTerm (SetOfTerms terms _ _) =
  printf "\\{%s\\}" . intercalate ", " <$> traverse renderTerm (Set.toList terms)
renderTerm (IncludedIn nam t _) =
  printf "\\mathit{%s} \\in %s" nam <$> renderTerm t
renderTerm (TermExpr expr _) =
  renderExpr expr
renderTerm (Wildcard _ _) = return "_"
renderTerm (HaskellExpr _ _ _) =
  error "rendering of haskell expressions is not supported"
renderTerm (TermHask _ _ _) =
  error "terms representing haskell values should not arise during rendering"
renderTerm (TermMap _ _ _) =
  error "mappings should not arise during rendering"

renderExpr :: MonadRender p m => Expr p Identity -> m String
renderExpr (LookupMap key mapping _ _) =
  printf "%s[%s]" <$> renderTerm mapping <*> renderTerm key
renderExpr (UpdateMap mapping key value _ _) =
  printf "%s[%s \\mapsto %s]" <$> renderExpr mapping <*> renderTerm key <*> renderTerm value
renderExpr (EmptyMap _ _) =
  pure "\\emptyset"
renderExpr (RewriteApp fname args _ _) =
  if null args
    then pure $ printf "%s()" (renderFunctorName fname)
    else printf "%s(%s)" (renderFunctorName fname) . intercalate ", " <$> traverse renderTerm args
renderExpr (GroundTerm term _ _) =
  renderTerm term
renderExpr (SetUnion set1 set2 _ _) =
  printf "%s \\cup %s" <$> renderTerm set1 <*> renderTerm set2


suffixNumPart :: String -> String -> String
suffixNumPart base numeric = base ++ (if numeric == "" then "" else "_" ++ numeric)

-- | Renders a single syntax definition
renderSyntax :: forall p m . MonadRender p m => SyntaxDecl' p -> m String
renderSyntax (SyntaxDecl vrs tpy productions _) =
  if null productions
    then printf "%s \\in %s&" <$> varsStr <*> renderTpy tpy
    else printf "%s \\in %s ::=&~ %s" <$> varsStr <*> renderTpy tpy <*> productionStrs
  where
    varsStr = intercalate ", " <$> (mapM (\vr -> renderTerm $ Atom (Identity vr) (typeAnnot (Proxy @p) AnyType) dummyRange)  vrs)
    productionStrs = intercalate "\\\\&\\mid " <$> traverse renderTerm productions
renderSyntax (TypeAliasDecl name tpy _) =
  printf "\\textbf{alias } %s = %s" (renderIdentifier name) <$> renderTpy tpy

-- | Render a single rule using mathpartir
renderRule :: MonadRender p m => RuleDecl' p -> m String
renderRule (RuleDecl name precedents consequents _) =
  printf "\\inferrule{%s}{%s}%s"
    <$> (intercalate " \\\\ " <$> traverse renderTerm precedents)
    <*> (intercalate " \\\\ " <$> traverse renderTerm consequents)
    <*> pure ruleName
  where
    ruleName = if null name then "" else printf "{\\scriptsize \\textsc{%s}}" (replaceUnicodeInString name)
renderRule (OnRuleDecl name precedents consequents _) =
  printf "\\inferrule{%s}{%s}%s"
    <$> (intercalate " \\\\ " <$> traverse renderTerm precedents)
    <*> (intercalate " \\\\ " <$> traverse renderTerm consequents)
    <*> pure ruleName
  where
    ruleName = if null name then "" else printf "{\\scriptsize \\textsc{%s}}" (replaceUnicodeInString name)

-- | Renders a program to the latex output
renderProgram :: MonadRender p m => Program' p -> m ()
renderProgram (Program decls _) = mapM_ renderDecl decls

-- | Renders a single declaration
renderDecl :: MonadRender p m => Decl' p -> m ()
renderDecl (Syntax name syntaxDecls _) = do
  rendered <- intercalate "\\\\\n" <$> traverse renderSyntax syntaxDecls
  blockName <- maybe (freshName Latex.Output.Syntax) pure name
  output %= addBlock blockName Latex.Output.Syntax rendered
renderDecl (RulesDecl name ruleDecls _) = do
  rendered <- intercalate "\\\\\n" <$> traverse renderRule ruleDecls
  blockName <- maybe (freshName Rules) pure name
  output %= addBlock blockName Rules rendered
renderDecl _ = pure ()

-- | Run the generator on a program to produce LaTeX output
runGenerator :: AnnotateType p => Program' p -> LatexOutput
runGenerator prog@(Program decls _) = _output $ execState (renderProgram prog) initialState
  where
    initialState = RenderState (LatexOutput mempty) 0 (extractRenderRules decls)


