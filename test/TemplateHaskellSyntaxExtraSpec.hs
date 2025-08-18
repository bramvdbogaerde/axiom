module TemplateHaskellSyntaxExtraSpec where

import Test.Hspec
import Language.TemplateHaskell.SyntaxExtra
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.List.NonEmpty as NE

-- Helper function to create simple names
mkName' :: String -> Name
mkName' s = mkName s

spec :: Spec
spec = do
  describe "freeVars" $ do
    describe "Basic expressions" $ do
      it "extracts free variable from VarE" $ do
        let expr = VarE (mkName' "x")
        freeVars expr `shouldBe` Set.singleton "x"
      
      it "returns empty set for ConE" $ do
        let expr = ConE (mkName' "Just")
        freeVars expr `shouldBe` Set.empty
      
      it "returns empty set for LitE" $ do
        let expr = LitE (IntegerL 42)
        freeVars expr `shouldBe` Set.empty
      
      it "extracts free variable from UnboundVarE" $ do
        let expr = UnboundVarE (mkName' "undefined_var")
        freeVars expr `shouldBe` Set.singleton "undefined_var"

    describe "Application expressions" $ do
      it "extracts free variables from AppE" $ do
        let expr = AppE (VarE (mkName' "f")) (VarE (mkName' "x"))
        freeVars expr `shouldBe` Set.fromList ["f", "x"]
      
      it "handles nested applications" $ do
        let expr = AppE (AppE (VarE (mkName' "f")) (VarE (mkName' "x"))) (VarE (mkName' "y"))
        freeVars expr `shouldBe` Set.fromList ["f", "x", "y"]
      
      it "handles application with constructor" $ do
        let expr = AppE (ConE (mkName' "Just")) (VarE (mkName' "x"))
        freeVars expr `shouldBe` Set.singleton "x"

    describe "Infix expressions" $ do
      it "extracts free variables from InfixE with all parts" $ do
        let expr = InfixE (Just (VarE (mkName' "x"))) (VarE (mkName' "+")) (Just (VarE (mkName' "y")))
        freeVars expr `shouldBe` Set.fromList ["x", "+", "y"]
      
      it "handles InfixE with missing left operand" $ do
        let expr = InfixE Nothing (VarE (mkName' "+")) (Just (VarE (mkName' "y")))
        freeVars expr `shouldBe` Set.fromList ["+", "y"]
      
      it "handles InfixE with missing right operand" $ do
        let expr = InfixE (Just (VarE (mkName' "x"))) (VarE (mkName' "+")) Nothing
        freeVars expr `shouldBe` Set.fromList ["x", "+"]
      
      it "extracts free variables from UInfixE" $ do
        let expr = UInfixE (VarE (mkName' "x")) (VarE (mkName' "+")) (VarE (mkName' "y"))
        freeVars expr `shouldBe` Set.fromList ["x", "+", "y"]

    describe "Parentheses and tuples" $ do
      it "extracts free variables from ParensE" $ do
        let expr = ParensE (VarE (mkName' "x"))
        freeVars expr `shouldBe` Set.singleton "x"
      
      it "extracts free variables from TupE" $ do
        let expr = TupE [Just (VarE (mkName' "x")), Just (VarE (mkName' "y")), Nothing]
        freeVars expr `shouldBe` Set.fromList ["x", "y"]
      
      it "extracts free variables from UnboxedTupE" $ do
        let expr = UnboxedTupE [Just (VarE (mkName' "a")), Just (VarE (mkName' "b"))]
        freeVars expr `shouldBe` Set.fromList ["a", "b"]

    describe "Lambda expressions" $ do
      it "removes bound variables from lambda" $ do
        let expr = LamE [VarP (mkName' "x")] (VarE (mkName' "x"))
        freeVars expr `shouldBe` Set.empty
      
      it "keeps free variables in lambda body" $ do
        let expr = LamE [VarP (mkName' "x")] (VarE (mkName' "y"))
        freeVars expr `shouldBe` Set.singleton "y"
      
      it "handles multiple lambda parameters" $ do
        let expr = LamE [VarP (mkName' "x"), VarP (mkName' "y")] 
                       (AppE (VarE (mkName' "f")) (AppE (VarE (mkName' "x")) (VarE (mkName' "z"))))
        freeVars expr `shouldBe` Set.fromList ["f", "z"]
      
      it "handles complex patterns in lambda" $ do
        let expr = LamE [TupP [VarP (mkName' "x"), VarP (mkName' "y")]] 
                       (AppE (VarE (mkName' "x")) (VarE (mkName' "z")))
        freeVars expr `shouldBe` Set.singleton "z"

    describe "Conditional expressions" $ do
      it "extracts free variables from CondE" $ do
        let expr = CondE (VarE (mkName' "p")) (VarE (mkName' "x")) (VarE (mkName' "y"))
        freeVars expr `shouldBe` Set.fromList ["p", "x", "y"]
      
      it "handles nested conditionals" $ do
        let expr = CondE (VarE (mkName' "p")) 
                         (CondE (VarE (mkName' "q")) (VarE (mkName' "a")) (VarE (mkName' "b")))
                         (VarE (mkName' "c"))
        freeVars expr `shouldBe` Set.fromList ["p", "q", "a", "b", "c"]

    describe "List expressions" $ do
      it "extracts free variables from ListE" $ do
        let expr = ListE [VarE (mkName' "x"), VarE (mkName' "y"), LitE (IntegerL 42)]
        freeVars expr `shouldBe` Set.fromList ["x", "y"]
      
      it "handles empty ListE" $ do
        let expr = ListE []
        freeVars expr `shouldBe` Set.empty

    describe "Type signatures and records" $ do
      it "extracts free variables from SigE" $ do
        let expr = SigE (VarE (mkName' "x")) (ConT (mkName' "Int"))
        freeVars expr `shouldBe` Set.singleton "x"
      
      it "extracts free variables from RecConE" $ do
        let expr = RecConE (mkName' "Person") [(mkName' "name", VarE (mkName' "n")), (mkName' "age", VarE (mkName' "a"))]
        freeVars expr `shouldBe` Set.fromList ["n", "a"]
      
      it "extracts free variables from RecUpdE" $ do
        let expr = RecUpdE (VarE (mkName' "person")) [(mkName' "age", VarE (mkName' "newAge"))]
        freeVars expr `shouldBe` Set.fromList ["person", "newAge"]

    describe "Other expressions" $ do
      it "extracts free variables from StaticE" $ do
        let expr = StaticE (VarE (mkName' "x"))
        freeVars expr `shouldBe` Set.singleton "x"
      
      it "extracts free variables from GetFieldE" $ do
        let expr = GetFieldE (VarE (mkName' "record")) "field"
        freeVars expr `shouldBe` Set.singleton "record"
      
      it "returns empty set for ProjectionE" $ do
        let expr = ProjectionE (NE.fromList ["field"])
        freeVars expr `shouldBe` Set.empty

    describe "Complex nested expressions" $ do
      it "handles deeply nested expressions" $ do
        -- f (g x) (h y z)
        let expr = AppE (AppE (VarE (mkName' "f"))
                             (AppE (VarE (mkName' "g")) (VarE (mkName' "x"))))
                       (AppE (AppE (VarE (mkName' "h")) (VarE (mkName' "y"))) (VarE (mkName' "z")))
        freeVars expr `shouldBe` Set.fromList ["f", "g", "x", "h", "y", "z"]
      
      it "handles lambda with complex body" $ do
        -- \x -> f x (g y)
        let expr = LamE [VarP (mkName' "x")]
                       (AppE (AppE (VarE (mkName' "f")) (VarE (mkName' "x")))
                             (AppE (VarE (mkName' "g")) (VarE (mkName' "y"))))
        freeVars expr `shouldBe` Set.fromList ["f", "g", "y"]
      
      it "handles multiple lambdas" $ do
        -- \x -> \y -> f x y z
        let expr = LamE [VarP (mkName' "x")]
                       (LamE [VarP (mkName' "y")]
                             (AppE (AppE (AppE (VarE (mkName' "f")) (VarE (mkName' "x")))
                                              (VarE (mkName' "y")))
                                   (VarE (mkName' "z"))))
        freeVars expr `shouldBe` Set.fromList ["f", "z"]

    describe "Pattern variable extraction" $ do
      it "extracts variables from complex patterns" $ do
        -- \(x, Just (y, z)) -> x + y + z + w
        let pattern = TupP [VarP (mkName' "x"), 
                           ConP (mkName' "Just") [] [TupP [VarP (mkName' "y"), VarP (mkName' "z")]]]
        let expr = LamE [pattern]
                       (AppE (AppE (VarE (mkName' "+"))
                                  (AppE (AppE (VarE (mkName' "+")) (VarE (mkName' "x")))
                                        (VarE (mkName' "y"))))
                             (AppE (AppE (VarE (mkName' "+")) (VarE (mkName' "z")))
                                   (VarE (mkName' "w"))))
        freeVars expr `shouldBe` Set.fromList ["+", "w"]
      
      it "handles AsP patterns" $ do
        -- \whole@(x, y) -> f whole x
        let pattern = AsP (mkName' "whole") (TupP [VarP (mkName' "x"), VarP (mkName' "y")])
        let expr = LamE [pattern]
                       (AppE (AppE (VarE (mkName' "f")) (VarE (mkName' "whole")))
                             (VarE (mkName' "x")))
        freeVars expr `shouldBe` Set.singleton "f"