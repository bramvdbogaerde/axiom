module ParseTestsSpec where

import Test.Hspec
import Language.AST
import Language.Range
import Language.Types (Value(..))
import Control.Applicative (liftA2)
import Data.Functor.Identity
import qualified Data.Set as Set
import TestInfrastructure

spec :: Spec
spec = do
  describe "Parser functionality" $ do
    describe "atomNames extraction" $ do
      it "extracts functor names from Atom" $ do
        term <- parseTermHelper "x"
        case term of
          Atom (Identity name) _ _ -> atomNames term `shouldBe` Set.singleton name
          _ -> expectationFailure "Expected Atom term"
      
      it "extracts functor names from Functor" $ do
        term <- parseTermHelper "f(x, y)"
        atomNames term `shouldBe` Set.fromList ["x", "y"]
      
      it "extracts functor names from Eqq" $ do
        term <- parseTermHelper "x = y"
        atomNames term `shouldBe` Set.fromList ["x", "y"]
      
      it "extracts functor names from Transition" $ do
        term <- parseTermHelper "x ~> y"
        atomNames term `shouldBe` Set.fromList ["x", "y"]
    
    describe "termEqIgnoreRange comparison" $ do
      it "compares Eqq terms ignoring range" $ do
        liftA2 termEqIgnoreRange (parseTermHelper "x = y") (parseTermHelper "x = y") `shouldReturn` True
      
      it "detects different Eqq terms" $ do
        liftA2 termEqIgnoreRange (parseTermHelper "x = y") (parseTermHelper "x = z") `shouldReturn` False
      
      it "compares Transition terms ignoring range" $ do
        liftA2 termEqIgnoreRange (parseTermHelper "x ~> y") (parseTermHelper "x ~> y") `shouldReturn` True
      
      it "detects different Transition terms" $ do
        liftA2 termEqIgnoreRange (parseTermHelper "x ~> y") (parseTermHelper "z ~> y") `shouldReturn` False
      
      it "returns False for different term types" $ do
        liftA2 termEqIgnoreRange (parseTermHelper "x") (parseTermHelper "f(x)") `shouldReturn` False
    
    describe "rangeOf instance for Term" $ do
      it "gets range from Atom" $ do
        term <- parseTermHelper "x"
        let range = rangeOf term
        range `shouldNotBe` dummyRange
        positionLine (rangeStart range) `shouldBe` 1
        positionColumn (rangeStart range) `shouldBe` 1
      
      it "gets range from Functor" $ do
        term <- parseTermHelper "f(x)"
        let range = rangeOf term
        range `shouldNotBe` dummyRange
        positionLine (rangeStart range) `shouldBe` 1
        positionColumn (rangeStart range) `shouldBe` 1
      
      it "gets range from Eqq" $ do
        term <- parseTermHelper "x = y"
        let range = rangeOf term
        range `shouldNotBe` dummyRange
        positionLine (rangeStart range) `shouldBe` 1
        positionColumn (rangeStart range) `shouldBe` 1
      
      it "gets range from Transition" $ do
        term <- parseTermHelper "x ~> y"
        let range = rangeOf term
        range `shouldNotBe` dummyRange
        positionLine (rangeStart range) `shouldBe` 1
        positionColumn (rangeStart range) `shouldBe` 1
    
    describe "infixNames" $ do
      it "contains expected infix operators" $ do
        infixNames `shouldContain` ["="]
        infixNames `shouldContain` ["~>"]
    
    describe "integer literal parsing" $ do
      it "parses positive integer literals" $ do
        term <- parseTermHelper "42"
        case term of
          TermValue (IntValue 42) _ _ -> return ()
          _ -> expectationFailure $ "Expected TermValue (IntValue 42), got: " ++ show term
      
      it "parses zero" $ do
        term <- parseTermHelper "0"
        case term of
          TermValue (IntValue 0) _ _ -> return ()
          _ -> expectationFailure $ "Expected TermValue (IntValue 0), got: " ++ show term
      
      it "parses large integers" $ do
        term <- parseTermHelper "123456"
        case term of
          TermValue (IntValue 123456) _ _ -> return ()
          _ -> expectationFailure $ "Expected TermValue (IntValue 123456), got: " ++ show term
      
      it "works in equality expressions" $ do
        term <- parseTermHelper "x = 42"
        case term of
          Eqq (Atom (Identity "x") _ _) (TermValue (IntValue 42) _ _) _ _ -> return ()
          _ -> expectationFailure $ "Expected 'x = 42' with integer literal, got: " ++ show term
      
      it "works in functor arguments" $ do
        term <- parseTermHelper "f(42, x)"
        case term of
          Functor "f" [TermValue (IntValue 42) _ _, Atom (Identity "x") _ _] _ _ -> return ()
          _ -> expectationFailure $ "Expected 'f(42, x)' with integer literal, got: " ++ show term
      
      it "atomNames excludes integer literals" $ do
        term <- parseTermHelper "f(42, x)"
        atomNames term `shouldBe` Set.singleton "x"
