module TestInfrastructure where

import Test.HUnit (assertFailure)
import Language.AST
import Language.Parser

-- | Helper function to parse a term directly, failing the test if parsing fails
parseTermHelper :: String -> IO PureTerm
parseTermHelper input = case parseTerm input of
  Right term -> return term
  Left err -> assertFailure $ "Parse error: " ++ show err

-- | Helper function to parse a rule directly, failing the test if parsing fails
parseRuleHelper :: String -> IO RuleDecl
parseRuleHelper input = case parseRule input of
  Right rule -> return rule
  Left err -> assertFailure $ "Parse error: " ++ show err