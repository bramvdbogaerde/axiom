{-# LANGUAGE EmptyDataDecls #-}
module Language.AST where

data Decl = Syntax [SyntaxDecl] | Rewrite RewriteDecl | RulesDecl [RuleDecl]
          deriving (Ord, Eq, Show)

newtype Program = Program [Decl] deriving (Ord, Eq, Show)


data SyntaxDecl = SyntaxDecl {
    syntaxVars :: [String],
    syntaxType :: String,
    productions :: [Production]
  } deriving (Ord, Eq, Show)

data Production = Type String | Functor String [Production]
                deriving (Ord, Eq, Show)
      
data RewriteDecl = RewriteDecl String -- ^ name
                               [Production] -- ^ argument
                               Production -- ^ body 
                              deriving (Ord, Eq, Show)

data RuleDecl = RuleDecl deriving (Ord, Eq, Show)

