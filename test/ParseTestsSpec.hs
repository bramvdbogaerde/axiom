module ParseTestsSpec where

import Test.Hspec
import Language.AST
import Language.Range
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
          Atom (Identity name) _ -> atomNames term `shouldBe` Set.singleton name
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
