module CommentTestsSpec where

import Test.Hspec
import Language.Parser
import Language.AST

spec :: Spec
spec = do
  describe "Comment parsing" $ do
    it "parses comments alongside declarations" $ do
      let testInput = unlines [
            "% This is a comment at the top",
            "syntax {",
            "  x in Expr ::= atom | func(x);",
            "};",
            "% Another comment",
            "rules {",
            "  rule \"test\" [atom] => [func(atom)];",
            "};"
            ]
      case parseProgram testInput of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 2
          length comments `shouldBe` 2
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" This is a comment at the top"]
          commentTexts `shouldContain` [" Another comment"]

    it "handles programs without comments" $ do
      let emptyInput = "syntax { x in Expr ::= atom; };"
      case parseProgram emptyInput of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 1
          length comments `shouldBe` 0

    it "parses mixed comments and declarations correctly" $ do
      let mixedInput = unlines [
            "% Header comment",
            "syntax { x in Expr ::= atom; };",
            "% Middle comment", 
            "% Another comment",
            "rules { rule \"test\" [atom] => [atom]; };"
            ]
      case parseProgram mixedInput of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 2
          length comments `shouldBe` 3

    it "allows comments within syntax blocks but only captures top-level comments" $ do
      let inputWithInnerComments = unlines [
            "% Top level comment",
            "syntax {",
            "  % Comment inside syntax block (should be allowed but not captured)",
            "  x in Expr ::= atom | func(x);",
            "  % Another comment in syntax",
            "};",
            "% Comment between sections",
            "rules {",
            "  % Comment inside rules block",
            "  rule \"test\" [atom] => [atom];",
            "  % Final comment in rules",
            "};"
            ]
      case parseProgram inputWithInnerComments of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          -- Should parse successfully with correct number of declarations
          length decls `shouldBe` 2
          -- Should only capture top-level comments (not those inside blocks)
          length comments `shouldBe` 2
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" Top level comment"]
          commentTexts `shouldContain` [" Comment between sections"]

    it "allows comments within rule precedents and consequents" $ do
      let ruleWithComments = unlines [
            "rules {",
            "  rule \"test1\" [",
            "    % Comment in precedent",
            "    atom;",
            "    % Another precedent comment",
            "    func(x)",
            "  ] => [",
            "    % Comment in consequent", 
            "    func(atom);",
            "    % Final consequent comment",
            "    result",
            "  ];",
            "};"
            ]
      case parseProgram ruleWithComments of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 1
          length comments `shouldBe` 0  -- No top-level comments

    it "allows comments within syntax element definitions" $ do
      let syntaxWithComments = unlines [
            "syntax {",
            "  x in Expr ::= ",
            "    % Comment before first production",
            "    atom |",
            "    % Comment between productions",
            "    func(",
            "      % Comment within function parameters",
            "      x,",
            "      % Another parameter comment",
            "      y",
            "    ) |",
            "    % Comment before last production",
            "    variable;",
            "};"
            ]
      case parseProgram syntaxWithComments of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 1
          length comments `shouldBe` 0  -- No top-level comments