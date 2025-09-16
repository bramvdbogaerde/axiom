{-# LANGUAGE LambdaCase #-}
module ParseTestsSpec where

import Test.Hspec
import Language.AST
import Language.Types (Value(..))
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
        term <- parseGoalHelper "x = y"
        atomNames term `shouldBe` Set.fromList ["x", "y"]
      
      it "extracts functor names from Transition" $ do
        term <- parseTermHelper "x ~> y"
        atomNames term `shouldBe` Set.fromList ["x", "y"]
    
    describe "termEqIgnoreRange comparison" $ do
      it "compares Eqq terms ignoring range" $ do
        liftA2 termEqIgnoreRange (parseGoalHelper "x = y") (parseGoalHelper "x = y") `shouldReturn` True
      
      it "detects different Eqq terms" $ do
        liftA2 termEqIgnoreRange (parseGoalHelper "x = y") (parseGoalHelper "x = z") `shouldReturn` False
      
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
        term <- parseGoalHelper "x = y"
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
        term <- parseGoalHelper "x = 42"
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
    
    describe "SetOfTerms parsing" $ do
      it "parses empty sets" $ do
        term <- parseTermHelper "{}"
        case term of
          SetOfTerms terms _ _ -> Set.size terms `shouldBe` 0
          _ -> expectationFailure $ "Expected SetOfTerms, got: " ++ show term
      
      it "parses single element sets" $ do
        term <- parseTermHelper "{a()}"
        case term of
          SetOfTerms terms _ _ -> do
            Set.size terms `shouldBe` 1
            case Set.toList terms of
              [elem] -> case elem of
                Functor "a" [] _ _ -> return ()
                _ -> expectationFailure $ "Expected atom 'a', got: " ++ show elem
              other -> expectationFailure $ "Expected single element, got: " ++ show other
          _ -> expectationFailure $ "Expected SetOfTerms, got: " ++ show term
      
      it "parses multiple element sets" $ do
        term <- parseTermHelper "{a(), b(), c()}"
        case term of
          SetOfTerms terms _ _ -> do
            Set.size terms `shouldBe` 3
            let atomNames = Set.fromList [name | Functor name [] _ _ <- Set.toList terms]
            atomNames `shouldBe` Set.fromList ["a", "b", "c"]
          _ -> expectationFailure $ "Expected SetOfTerms, got: " ++ show term
      
      it "parses sets with complex terms" $ do
        term <- parseTermHelper "{f(x, y), g(z)}"
        case term of
          SetOfTerms terms _ _ -> do
            Set.size terms `shouldBe` 2
            let hasFunctor name = any (\case Functor n _ _ _ -> n == name; _ -> False) (Set.toList terms)
            hasFunctor "f" `shouldBe` True
            hasFunctor "g" `shouldBe` True
          _ -> expectationFailure $ "Expected SetOfTerms, got: " ++ show term
      
      it "parses nested sets" $ do
        term <- parseTermHelper "{{a()}, {b()}}"
        case term of
          SetOfTerms outerTerms _ _ -> do
            Set.size outerTerms `shouldBe` 2
            let innerSets = [terms | SetOfTerms terms _ _ <- Set.toList outerTerms]
            length innerSets `shouldBe` 2
          _ -> expectationFailure $ "Expected SetOfTerms, got: " ++ show term
      
      it "works in equality expressions" $ do
        term <- parseGoalHelper "x = {a(), b()}"
        case term of
          Eqq (Atom (Identity "x") _ _) (SetOfTerms terms _ _) _ _ -> 
            Set.size terms `shouldBe` 2
          _ -> expectationFailure $ "Expected 'x = {a(), b()}', got: " ++ show term
      
      it "works in functor arguments" $ do
        term <- parseTermHelper "f({a()}, x)"
        case term of
          Functor "f" [SetOfTerms terms _ _, Atom (Identity "x") _ _] _ _ -> 
            Set.size terms `shouldBe` 1
          _ -> expectationFailure $ "Expected 'f({a()}, x)', got: " ++ show term
      
      it "atomNames extracts from set elements" $ do
        term <- parseTermHelper "{x, f(y, z)}"
        atomNames term `shouldBe` Set.fromList ["x", "y", "z"]

    describe "EmptyMap parsing" $ do
      it "parses empty map" $ do
        term <- parseTermHelper "[]"
        case term of
          TermExpr (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected TermExpr (EmptyMap), got: " ++ show term

      it "works in functor arguments" $ do
        term <- parseTermHelper "f([], x)"
        case term of
          Functor "f" [TermExpr (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) _, Atom (Identity "x") _ _] _ _ -> return ()
          _ -> expectationFailure $ "Expected 'f(∅, x)', got: " ++ show term

    describe "UpdateMap parsing" $ do
      it "parses single binding from empty map" $ do
        term <- parseTermHelper "[x |-> y]"
        case term of
          TermExpr (UpdateMap (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) (Atom (Identity "x") _ _) (Atom (Identity "y") _ _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected '[x |-> y]' with UpdateMap, got: " ++ show term

      it "parses single binding with integer value" $ do
        term <- parseTermHelper "[x |-> 42]"
        case term of
          TermExpr (UpdateMap (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) (Atom (Identity "x") _ _) (TermValue (IntValue 42) _ _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected '[x |-> 42]' with UpdateMap, got: " ++ show term

      it "parses single binding with functor value" $ do
        term <- parseTermHelper "[x |-> f(a, b)]"
        case term of
          TermExpr (UpdateMap (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) (Atom (Identity "x") _ _) (Functor "f" [Atom (Identity "a") _ _, Atom (Identity "b") _ _] _ _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected '[x |-> f(a, b)]' with UpdateMap, got: " ++ show term

      it "works in equality expressions" $ do
        term <- parseGoalHelper "map = [key |-> value]"
        case term of
          Eqq (Atom (Identity "map") _ _) (TermExpr (UpdateMap _ (Atom (Identity "key") _ _) (Atom (Identity "value") _ _) _ _) _) _ _ -> return ()
          _ -> expectationFailure $ "Expected 'map = [key |-> value]', got: " ++ show term

      it "parses multiple bindings from empty map" $ do
        term <- parseTermHelper "[x |-> y; a |-> b]"
        case term of
          TermExpr (UpdateMap (UpdateMap (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) (Atom (Identity "x") _ _) (Atom (Identity "y") _ _) _ _) (Atom (Identity "a") _ _) (Atom (Identity "b") _ _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected '[x |-> y; a |-> b]' with nested UpdateMap, got: " ++ show term

      it "parses three bindings from empty map" $ do
        term <- parseTermHelper "[x |-> 1; y |-> 2; z |-> 3]"
        case term of
          TermExpr (UpdateMap (UpdateMap (UpdateMap (GroundTerm (TermExpr (EmptyMap _ _) _) _ _) (Atom (Identity "x") _ _) (TermValue (IntValue 1) _ _) _ _) (Atom (Identity "y") _ _) (TermValue (IntValue 2) _ _) _ _) (Atom (Identity "z") _ _) (TermValue (IntValue 3) _ _) _ _) _ -> return ()
          _ -> expectationFailure $ "Expected '[x |-> 1; y |-> 2; z |-> 3]' with triple nested UpdateMap, got: " ++ show term

