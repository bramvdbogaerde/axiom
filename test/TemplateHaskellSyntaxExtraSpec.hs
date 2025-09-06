module TemplateHaskellSyntaxExtraSpec where

import Test.Hspec
import Language.TemplateHaskell.SyntaxExtra
import Language.Haskell.TH.Syntax
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import Data.Functor.Identity
import Control.Monad.Writer

spec :: Spec
spec = do
  describe "freeVars" $ do
    describe "Basic expressions" $ do
      it "extracts free variable from VarE" $ do
        let expr = VarE (mkName "x")
        freeVars expr `shouldBe` Set.singleton "x"

      it "returns empty set for ConE" $ do
        let expr = ConE (mkName "Just")
        freeVars expr `shouldBe` Set.empty

      it "returns empty set for LitE" $ do
        let expr = LitE (IntegerL 42)
        freeVars expr `shouldBe` Set.empty

      it "extracts free variable from UnboundVarE" $ do
        let expr = UnboundVarE (mkName "undefined_var")
        freeVars expr `shouldBe` Set.singleton "undefined_var"

    describe "Application expressions" $ do
      it "extracts free variables from AppE" $ do
        let expr = AppE (VarE (mkName "f")) (VarE (mkName "x"))
        freeVars expr `shouldBe` Set.fromList ["f", "x"]

      it "handles nested applications" $ do
        let expr = AppE (AppE (VarE (mkName "f")) (VarE (mkName "x"))) (VarE (mkName "y"))
        freeVars expr `shouldBe` Set.fromList ["f", "x", "y"]

      it "handles application with constructor" $ do
        let expr = AppE (ConE (mkName "Just")) (VarE (mkName "x"))
        freeVars expr `shouldBe` Set.singleton "x"

    describe "Infix expressions" $ do
      it "extracts free variables from InfixE with all parts" $ do
        let expr = InfixE (Just (VarE (mkName "x"))) (VarE (mkName "+")) (Just (VarE (mkName "y")))
        freeVars expr `shouldBe` Set.fromList ["x", "+", "y"]

      it "handles InfixE with missing left operand" $ do
        let expr = InfixE Nothing (VarE (mkName "+")) (Just (VarE (mkName "y")))
        freeVars expr `shouldBe` Set.fromList ["+", "y"]

      it "handles InfixE with missing right operand" $ do
        let expr = InfixE (Just (VarE (mkName "x"))) (VarE (mkName "+")) Nothing
        freeVars expr `shouldBe` Set.fromList ["x", "+"]

      it "extracts free variables from UInfixE" $ do
        let expr = UInfixE (VarE (mkName "x")) (VarE (mkName "+")) (VarE (mkName "y"))
        freeVars expr `shouldBe` Set.fromList ["x", "+", "y"]

    describe "Parentheses and tuples" $ do
      it "extracts free variables from ParensE" $ do
        let expr = ParensE (VarE (mkName "x"))
        freeVars expr `shouldBe` Set.singleton "x"

      it "extracts free variables from TupE" $ do
        let expr = TupE [Just (VarE (mkName "x")), Just (VarE (mkName "y")), Nothing]
        freeVars expr `shouldBe` Set.fromList ["x", "y"]

      it "extracts free variables from UnboxedTupE" $ do
        let expr = UnboxedTupE [Just (VarE (mkName "a")), Just (VarE (mkName "b"))]
        freeVars expr `shouldBe` Set.fromList ["a", "b"]

    describe "Lambda expressions" $ do
      it "removes bound variables from lambda" $ do
        let expr = LamE [VarP (mkName "x")] (VarE (mkName "x"))
        freeVars expr `shouldBe` Set.empty

      it "keeps free variables in lambda body" $ do
        let expr = LamE [VarP (mkName "x")] (VarE (mkName "y"))
        freeVars expr `shouldBe` Set.singleton "y"

      it "handles multiple lambda parameters" $ do
        let expr = LamE [VarP (mkName "x"), VarP (mkName "y")]
                       (AppE (VarE (mkName "f")) (AppE (VarE (mkName "x")) (VarE (mkName "z"))))
        freeVars expr `shouldBe` Set.fromList ["f", "z"]

      it "handles complex patterns in lambda" $ do
        let expr = LamE [TupP [VarP (mkName "x"), VarP (mkName "y")]]
                       (AppE (VarE (mkName "x")) (VarE (mkName "z")))
        freeVars expr `shouldBe` Set.singleton "z"

    describe "Conditional expressions" $ do
      it "extracts free variables from CondE" $ do
        let expr = CondE (VarE (mkName "p")) (VarE (mkName "x")) (VarE (mkName "y"))
        freeVars expr `shouldBe` Set.fromList ["p", "x", "y"]

      it "handles nested conditionals" $ do
        let expr = CondE (VarE (mkName "p"))
                         (CondE (VarE (mkName "q")) (VarE (mkName "a")) (VarE (mkName "b")))
                         (VarE (mkName "c"))
        freeVars expr `shouldBe` Set.fromList ["p", "q", "a", "b", "c"]

    describe "List expressions" $ do
      it "extracts free variables from ListE" $ do
        let expr = ListE [VarE (mkName "x"), VarE (mkName "y"), LitE (IntegerL 42)]
        freeVars expr `shouldBe` Set.fromList ["x", "y"]

      it "handles empty ListE" $ do
        let expr = ListE []
        freeVars expr `shouldBe` Set.empty

    describe "Type signatures and records" $ do
      it "extracts free variables from SigE" $ do
        let expr = SigE (VarE (mkName "x")) (ConT (mkName "Int"))
        freeVars expr `shouldBe` Set.singleton "x"

      it "extracts free variables from RecConE" $ do
        let expr = RecConE (mkName "Person") [(mkName "name", VarE (mkName "n")), (mkName "age", VarE (mkName "a"))]
        freeVars expr `shouldBe` Set.fromList ["n", "a"]

      it "extracts free variables from RecUpdE" $ do
        let expr = RecUpdE (VarE (mkName "person")) [(mkName "age", VarE (mkName "newAge"))]
        freeVars expr `shouldBe` Set.fromList ["person", "newAge"]

    describe "Other expressions" $ do
      it "extracts free variables from StaticE" $ do
        let expr = StaticE (VarE (mkName "x"))
        freeVars expr `shouldBe` Set.singleton "x"

      it "extracts free variables from GetFieldE" $ do
        let expr = GetFieldE (VarE (mkName "record")) "field"
        freeVars expr `shouldBe` Set.singleton "record"

      it "returns empty set for ProjectionE" $ do
        let expr = ProjectionE (NE.fromList ["field"])
        freeVars expr `shouldBe` Set.empty

    describe "Complex nested expressions" $ do
      it "handles deeply nested expressions" $ do
        -- f (g x) (h y z)
        let expr = AppE (AppE (VarE (mkName "f"))
                             (AppE (VarE (mkName "g")) (VarE (mkName "x"))))
                       (AppE (AppE (VarE (mkName "h")) (VarE (mkName "y"))) (VarE (mkName "z")))
        freeVars expr `shouldBe` Set.fromList ["f", "g", "x", "h", "y", "z"]

      it "handles lambda with complex body" $ do
        -- \x -> f x (g y)
        let expr = LamE [VarP (mkName "x")]
                       (AppE (AppE (VarE (mkName "f")) (VarE (mkName "x")))
                             (AppE (VarE (mkName "g")) (VarE (mkName "y"))))
        freeVars expr `shouldBe` Set.fromList ["f", "g", "y"]

      it "handles multiple lambdas" $ do
        -- \x -> \y -> f x y z
        let expr = LamE [VarP (mkName "x")]
                       (LamE [VarP (mkName "y")]
                             (AppE (AppE (AppE (VarE (mkName "f")) (VarE (mkName "x")))
                                              (VarE (mkName "y")))
                                   (VarE (mkName "z"))))
        freeVars expr `shouldBe` Set.fromList ["f", "z"]

    describe "Pattern variable extraction" $ do
      it "extracts variables from complex patterns" $ do
        -- \(x, Just (y, z)) -> x + y + z + w
        let pattern = TupP [VarP (mkName "x"),
                           ConP (mkName "Just") [] [TupP [VarP (mkName "y"), VarP (mkName "z")]]]
        let expr = LamE [pattern]
                       (AppE (AppE (VarE (mkName "+"))
                                  (AppE (AppE (VarE (mkName "+")) (VarE (mkName "x")))
                                        (VarE (mkName "y"))))
                             (AppE (AppE (VarE (mkName "+")) (VarE (mkName "z")))
                                   (VarE (mkName "w"))))
        freeVars expr `shouldBe` Set.fromList ["+", "w"]

      it "handles AsP patterns" $ do
        -- \whole@(x, y) -> f whole x
        let pattern = AsP (mkName "whole") (TupP [VarP (mkName "x"), VarP (mkName "y")])
        let expr = LamE [pattern]
                       (AppE (AppE (VarE (mkName "f")) (VarE (mkName "whole")))
                             (VarE (mkName "x")))
        freeVars expr `shouldBe` Set.singleton "f"

  describe "mapSuffixedTypes" $ do
    describe "Basic type transformations" $ do
      it "transforms ConT with matching suffix" $ do
        let inputType = ConT (mkName "Adr#")
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))

      it "leaves ConT with non-matching suffix unchanged" $ do
        let inputType = ConT (mkName "Int")
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` ConT (mkName "Int")

      it "leaves VarT unchanged" $ do
        let inputType = VarT (mkName "a")
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` VarT (mkName "a")

    describe "Complex type transformations" $ do
      it "transforms Map Adr# V to Map (PureTerm' p) V" $ do
        let inputType = AppT (AppT (ConT (mkName "Map")) (ConT (mkName "Adr#"))) (VarT (mkName "V"))
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        let expected = AppT (AppT (ConT (mkName "Map"))
                                  (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
                           (VarT (mkName "V"))
        result `shouldBe` expected

      it "transforms nested applications with multiple suffixed types" $ do
        let inputType = AppT (AppT (ConT (mkName "Either")) (ConT (mkName "Adr#"))) (ConT (mkName "Val#"))
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        let expected = AppT (AppT (ConT (mkName "Either"))
                                  (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
                           (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p")))
        result `shouldBe` expected

      it "handles ParensT with suffixed types" $ do
        let inputType = ParensT (AppT (ConT (mkName "Maybe")) (ConT (mkName "Adr#")))
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        let expected = ParensT (AppT (ConT (mkName "Maybe"))
                                     (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        result `shouldBe` expected

      it "handles InfixT with suffixed types" $ do
        let inputType = InfixT (ConT (mkName "Adr#")) (mkName "->") (ConT (mkName "Int"))
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        let expected = InfixT (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p")))
                              (mkName "->")
                              (ConT (mkName "Int"))
        result `shouldBe` expected

    describe "Quantified types" $ do
      it "handles ForallT with suffixed types in context and body" $ do
        let ctx = [AppT (ConT (mkName "Show")) (ConT (mkName "Adr#"))]
        let body = AppT (ConT (mkName "Maybe")) (ConT (mkName "Val#"))
        let inputType = ForallT [PlainTV (mkName "a") SpecifiedSpec] ctx body
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        let expectedCtx = [AppT (ConT (mkName "Show"))
                               (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p")))]
        let expectedBody = AppT (ConT (mkName "Maybe"))
                                (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p")))
        let expected = ForallT [PlainTV (mkName "a") SpecifiedSpec] expectedCtx expectedBody
        result `shouldBe` expected

    describe "Atomic types remain unchanged" $ do
      it "leaves TupleT unchanged" $ do
        let inputType = TupleT 3
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` TupleT 3

      it "leaves ArrowT unchanged" $ do
        let inputType = ArrowT
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` ArrowT

      it "leaves ListT unchanged" $ do
        let inputType = ListT
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "#" transform inputType)
        result `shouldBe` ListT

    describe "Different suffixes" $ do
      it "only transforms types ending with specified suffix" $ do
        let inputType = AppT (AppT (ConT (mkName "Map")) (ConT (mkName "AdrSuffix"))) (ConT (mkName "ValOther"))
        let transform = const (return (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
        let result = runIdentity (mapSuffixedTypes "Suffix" transform inputType)
        let expected = AppT (AppT (ConT (mkName "Map"))
                                  (AppT (ConT (mkName "PureTerm'")) (VarT (mkName "p"))))
                           (ConT (mkName "ValOther"))
        result `shouldBe` expected

    describe "Writer monad usage for collecting suffixed types" $ do
      it "collects all suffixed type names using Writer monad" $ do
        let inputType = AppT (AppT (ConT (mkName "Map")) (ConT (mkName "Adr#"))) (ConT (mkName "Val#"))
        let collectSuffixed :: Type -> Writer [String] Type
            collectSuffixed typ = do
              tell [nameBase (typeName typ)]
              return typ
        let (result, collected) = runWriter (mapSuffixedTypes "#" collectSuffixed inputType)
        collected `shouldBe` ["Adr#", "Val#"]
        -- The result type should be unchanged since we return the original type
        result `shouldBe` inputType
      
      it "collects suffixed types from nested structures" $ do
        let ctx = [AppT (ConT (mkName "Show")) (ConT (mkName "Adr#"))]
        let body = AppT (AppT (ConT (mkName "Either")) (ConT (mkName "Val#"))) (ConT (mkName "Ref#"))
        let inputType = ForallT [PlainTV (mkName "a") SpecifiedSpec] ctx body
        let collectSuffixed :: Type -> Writer [String] Type
            collectSuffixed typ = do
              tell [nameBase (typeName typ)]
              return typ
        let (_, collected) = runWriter (mapSuffixedTypes "#" collectSuffixed inputType)
        collected `shouldBe` ["Adr#", "Val#", "Ref#"]
      
      it "collects only suffixed types with specified suffix" $ do
        let inputType = AppT (AppT (ConT (mkName "Map")) (ConT (mkName "AdrSuffix"))) (ConT (mkName "ValOther"))
        let collectSuffixed :: Type -> Writer [String] Type
            collectSuffixed typ = do
              tell [nameBase (typeName typ)]
              return typ
        let (_, collected) = runWriter (mapSuffixedTypes "Suffix" collectSuffixed inputType)
        collected `shouldBe` ["AdrSuffix"]
      
      it "handles empty collection when no suffixed types present" $ do
        let inputType = AppT (AppT (ConT (mkName "Map")) (ConT (mkName "Int"))) (ConT (mkName "String"))
        let collectSuffixed :: Type -> Writer [String] Type
            collectSuffixed typ = do
              tell [nameBase (typeName typ)]
              return typ
        let (_, collected) = runWriter (mapSuffixedTypes "#" collectSuffixed inputType)
        collected `shouldBe` ([] :: [String])

-- Helper function to extract type name from ConT
typeName :: Type -> Name
typeName (ConT name) = name
typeName _ = error "Expected ConT"
