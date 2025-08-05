{-# LANGUAGE TupleSections #-}
module UnificationTestsSpec where

import Test.Hspec
import Test.HUnit (assertFailure)
import Language.AST
import Language.Solver.Unification
import Language.Parser
import Control.Monad.ST
import qualified Data.Map as Map

-- Helper function to parse a term directly
parseTermHelper :: String -> IO PureTerm
parseTermHelper input = either (assertFailure . ("Parse error: " ++) . show) return (parseTerm input)

-- Test that refTerm and pureTerm are inverses for a given term
testInverse :: String -> Expectation
testInverse termStr = do
  original <- parseTermHelper termStr
  let result = runST $ refTerm original Map.empty >>= uncurry pureTerm
  result `shouldBe` original

-- Helper to create a test with the term in the name
testInverseFor :: String -> Spec
testInverseFor termStr = it ("refTerm and pureTerm are inverses for '" ++ termStr ++ "'") $ testInverse termStr

-- Helper to test successful unification
testUnificationSuccess :: String -> String -> String -> [(String, String)] -> Spec
testUnificationSuccess desc term1Str term2Str expectedMappings = 
  it (desc ++ ": " ++ term1Str ++ " = " ++ term2Str) $ do
    term1 <- parseTermHelper term1Str
    term2 <- parseTermHelper term2Str
    expectedPairs <- mapM (\(var, termStr) -> (var,) <$> parseTermHelper termStr) expectedMappings
    case runUnification term1 term2 of
      Right mapping -> 
        mapM_ (\(var, expectedTerm) -> 
          maybe (assertFailure $ "Variable " ++ var ++ " not found in mapping")
                (`shouldSatisfy` termEqIgnoreRange expectedTerm)
                (Map.lookup var mapping)) expectedPairs
      Left err -> assertFailure $ "Unification failed: " ++ err

-- Helper to test unification failure
testUnificationFailure :: String -> String -> String -> Spec
testUnificationFailure desc term1Str term2Str =
  it (desc ++ ": " ++ term1Str ++ " = " ++ term2Str) $ do
    term1 <- parseTermHelper term1Str
    term2 <- parseTermHelper term2Str
    case runUnification term1 term2 of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Expected unification to fail"

-- Helper to test partial unification (variable mappings exist)
testPartialUnification :: String -> String -> String -> [String] -> Spec
testPartialUnification desc term1Str term2Str varNames =
  it (desc ++ ": " ++ term1Str ++ " = " ++ term2Str) $ do
    term1 <- parseTermHelper term1Str
    term2 <- parseTermHelper term2Str
    case runUnification term1 term2 of
      Right mapping -> do
        let hasMapping = any (`Map.member` mapping) varNames
        hasMapping `shouldBe` True
      Left err -> assertFailure $ "Unification failed: " ++ err

spec :: Spec
spec = do
  describe "Unification roundtrip" $ do
    testInverseFor "x"
    testInverseFor "f(x, y)"
    testInverseFor "x = f(y)"
    testInverseFor "x ~> g(x)"
    testInverseFor "cons(x, cons(y, nil())) = append(x, y)"
    testInverseFor "x = x"

  describe "Unification tests" $ do
    describe "Successful unification" $ do
      testUnificationSuccess "unifies variable with atom" "x" "atom()" [("x", "atom()")]
      testUnificationSuccess "unifies variable with functor" "x" "f(a(), b())" [("x", "f(a(), b())")]
      testUnificationSuccess "unifies functors with same name" "f(x, y)" "f(a(), b())" [("x", "a()"), ("y", "b()")]
      testUnificationSuccess "unifies complex nested terms" "cons(x, cons(y, nil()))" "cons(a(), cons(b(), nil()))" [("x", "a()"), ("y", "b()")]
      testUnificationSuccess "unifies fully ground terms" "cons(a(), nil())" "cons(a(), nil())" []

    describe "Unification failures" $ do
      testUnificationFailure "fails to unify different atoms" "atom1()" "atom2()"
      testUnificationFailure "fails to unify functors with different names" "f(x)" "g(x)"
      testUnificationFailure "fails to unify functors with different arities" "f(x)" "f(x, y)"
      testUnificationFailure "fails to unify different ground terms" "cons(a(), nil())" "cons(b(), nil())"

    describe "Partial unification" $ do
      testPartialUnification "handles variable-to-variable unification" "x" "y" ["x", "y"]
      testPartialUnification "handles self-unification of variables" "x" "x" ["x"]
      -- testPartialUnification "handles circular variable references" "f(x, y)" "f(y, x)" ["x", "y"]
    
    describe "Cycle detection" $ do
      testUnificationFailure "detects simple cycles in nested terms" "f(x)" "f(f(x))"
      testUnificationFailure "detects cycles in deeper nesting" "g(x, y)" "g(f(x), x)"
      testUnificationFailure "detects cycles with multiple variables" "h(x, y)" "h(y, f(x))"
      testUnificationFailure "detects indirect cycles" "cons(x, y)" "cons(head(y), cons(x, nil()))"
