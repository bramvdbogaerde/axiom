module RenamerTestsSpec where

import Test.Hspec
import Test.HUnit (assertFailure)
import Language.AST
import Language.Solver.Renamer
import Data.Functor.Identity
import TestInfrastructure

spec :: Spec
spec = do
  describe "Alpha renaming" $ do
    it "renames variable 'x' consistently" $ do
      rule <- parseRuleHelper "rule \"test\" [ ] => [ x ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [] [Atom (Identity renamedVar) _ _] _ -> do
          renamedVar `shouldBe` "x0"
        _ -> expectationFailure "Unexpected rule structure after renaming"
    
    it "renames variables 'x' and 'y' with different names in 'x => y'" $ do
      rule <- parseRuleHelper "rule \"test\" [ x ] => [ y ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [Atom (Identity renamedX) _ _] [Atom (Identity renamedY) _ _] _ -> do
          renamedX `shouldBe` "x0"
          renamedY `shouldBe` "y1"
        _ -> expectationFailure "Unexpected rule structure after renaming"
    
    it "handles functor term 'f(x, y)' and variable 'x' consistently" $ do
      rule <- parseRuleHelper "rule \"test\" [ f(x, y) ] => [ x ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [Functor "f" [Atom (Identity renamedX1) _ _, Atom (Identity renamedY) _ _] _ _] [Atom (Identity renamedX2) _ _] _ -> do
          renamedX1 `shouldBe` "x0"
          renamedX2 `shouldBe` "x0"  -- Same variable should be consistently renamed
          renamedY `shouldBe` "y1"
        _ -> expectationFailure "Unexpected rule structure after renaming"
    
    it "handles equality term 'x = y'" $ do
      rule <- parseRuleHelper "rule \"test\" [ x = y ] => [ ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [Eqq (Atom (Identity renamedX) _ _) (Atom (Identity renamedY) _ _) _ _] [] _ -> do
          renamedX `shouldBe` "x0"
          renamedY `shouldBe` "y1"
        _ -> expectationFailure "Unexpected rule structure after renaming"
    
    it "handles transition term 'x ~> y'" $ do
      rule <- parseRuleHelper "rule \"test\" [ x ~> y ] => [ ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [Transition _ (Atom (Identity renamedX) _ _) (Atom (Identity renamedY) _ _) _ _] [] _ -> do
          renamedX `shouldBe` "x0"
          renamedY `shouldBe` "y1"
        _ -> expectationFailure "Unexpected rule structure after renaming"
    
    it "maintains variable consistency across multiple terms 'f(x), g(x)'" $ do
      rule <- parseRuleHelper "rule \"test\" [ f(x); g(x) ] => [ ]"
      let renamedRule = renameRule 0 rule
      
      case renamedRule of
        RuleDecl _ [Functor "f" [Atom (Identity renamedX1) _ _] _ _, Functor "g" [Atom (Identity renamedX2) _ _] _ _] [] _ -> do
          renamedX1 `shouldBe` "x0"
          renamedX2 `shouldBe` "x0"  -- Same variable should be consistently renamed
        _ -> expectationFailure "Unexpected rule structure after renaming"
