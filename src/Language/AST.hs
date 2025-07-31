module Language.AST(
    Tpy,
    Program(..),
    Decl(..),
    SyntaxDecl(..),
    RewriteDecl(..),
    RuleDecl(..),
    Term(..),
    variableName
  ) where
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (fromJust, fromMaybe)

-- | A program is a sequence of declarations
newtype Program = Program [Decl] deriving (Ord, Eq, Show)

-- | Names of types
type Tpy = String

-- | A declaration is either a syntax section, rules section, transition
-- declaration or or a rewrite rule.
data Decl = Syntax [SyntaxDecl]
          | Rewrite RewriteDecl
          | RulesDecl [RuleDecl]
          | TransitionDecl String Tpy Tpy
          deriving (Ord, Eq, Show)

-- | var in Tpy ::= term0 | term1 | ...
data SyntaxDecl = SyntaxDecl {
    syntaxVars :: [String],
    syntaxType :: String,
    productions :: [Term]
  } deriving (Ord, Eq, Show)

-- | head(term0, term1, ...) = term;
data RewriteDecl = RewriteDecl String -- ^ name
                               [Term] -- ^ argument
                               Term -- ^ body 
                              deriving (Ord, Eq, Show)

-- | rule NAME [ PRECEDENT ] => [ CONSEQUENT ];
data RuleDecl = RuleDecl String -- ^ rule name
                         [Term] -- ^ the precedent
                         [Term] -- ^ the consequent
                        deriving (Ord, Eq, Show)

-- | term \in Term ::= atom
--                  | atom(term0, term1, ...)
--                  | term0 = term1
--                  | term0 ~> term1
data Term = Atom String
          | Functor String [Term]
          | Eqq Term Term
          | Transition String Term Term
          deriving (Ord, Eq, Show)

-- | Extract the name of the variable from variables suffixed with numbers
variableName :: String -> String
variableName s = head $ fromMaybe (error $ "could not get variable name of " ++ s) $ matchRegex r s
  where r = mkRegex "([a-zA-Z]+)\\d*"
