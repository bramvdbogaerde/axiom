{-# LANGUAGE TupleSections #-}
module UnificationTestsSpec where

import Test.Hspec
import Test.HUnit (assertFailure)
import Language.AST
import Language.Solver.Unification
import Language.Solver.BacktrackingST
import Language.Parser
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import TestInfrastructure

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

    describe "Path compression bug" $ do
      testUnificationSuccess "handles path compression with uninitialized parent" "f(x, y)" "f(z, y)" [("x", "z")]
      it "exposes path compression bug with pointer chains" $ do
        -- This test creates a scenario where path compression would fail
        -- We create: cell1 -> Ptr(cell2) where cell2 is Uninitialized
        -- Then we call setOf on cell1, which should preserve the pointer to cell2
        let testResult = runST $ runExceptT $ do
              -- Create three cells: cell1 points to cell2, cell2 is uninitialized
              ref1 <- lift $ newSTRef (Uninitialized "x")
              ref2 <- lift $ newSTRef (Uninitialized "y")
              ref3 <- lift $ newSTRef (Uninitialized "z")

              let cell1 = Ref ref1
              let cell2 = Ref ref2
              let cell3 = Ref ref3

              -- Create pointer chain: cell1 -> cell2 -> cell3
              lift $ writeSTRef ref1 (Ptr cell2)
              lift $ writeSTRef ref2 (Ptr cell3)

              -- Call setOf on cell1 - this should do path compression
              _  <- lift $ setOf cell1
              finalCell <- lift $ readSTRef ref1

              when (finalCell /= Ptr cell3) $
                throwError "Expected paths to be compressed"

              -- Now modify cell3 to have a value
              lift $ writeSTRef ref3 (Value (Functor "atom" [] dummyRange))

              -- Try to retrieve through cell1 - this should still work if path compression preserved pointers correctly
              finalCell2 <- lift $ setOf cell1

              -- Check if we can still access the updated value through cell1
              finalValue <- lift $ readCellRef finalCell2
              case finalValue of
                Value _ -> return () -- Success: path compression preserved the reference chain
                _ -> throwError "Bug: path compression broke final check in the reference chain"

        testResult `shouldBe` Right ()

    describe "Cycle detection" $ do
      testUnificationFailure "detects simple cycles in nested terms" "f(x)" "f(f(x))"
      testUnificationFailure "detects cycles in deeper nesting" "g(x, y)" "g(f(x), x)"
      testUnificationFailure "detects cycles with multiple variables" "h(x, y)" "h(y, f(x))"
      testUnificationFailure "detects indirect cycles" "cons(x, y)" "cons(head(y), cons(x, nil()))"
    
    describe "Error handling" $ do
      testUnificationFailure "fails on mixed term structures" "f(x)" "x = y"
      -- TODO: Add test for different transition names when parser supports named transitions
      -- testUnificationFailure "fails on different transition names" "x ~> y" "x step y"
