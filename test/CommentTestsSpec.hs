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

    it "allows comments within syntax blocks and captures all comments" $ do
      let inputWithInnerComments = unlines [
            "% Top level comment",
            "syntax {",
            "  % Comment inside syntax block (should be allowed and captured)",
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
          -- Should capture all comments including those inside blocks
          length comments `shouldBe` 6
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" Top level comment"]
          commentTexts `shouldContain` [" Comment between sections"]
          commentTexts `shouldContain` [" Comment inside syntax block (should be allowed and captured)"]
          commentTexts `shouldContain` [" Another comment in syntax"]
          commentTexts `shouldContain` [" Comment inside rules block"]
          commentTexts `shouldContain` [" Final comment in rules"]

    it "allows comments within rule precedents and consequents and captures them" $ do
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
          length comments `shouldBe` 4  -- Captures all comments within the rule
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" Comment in precedent"]
          commentTexts `shouldContain` [" Another precedent comment"]
          commentTexts `shouldContain` [" Comment in consequent"]
          commentTexts `shouldContain` [" Final consequent comment"]

    it "allows comments within syntax element definitions and captures them" $ do
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
          length comments `shouldBe` 5  -- Captures all comments within syntax definitions
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" Comment before first production"]
          commentTexts `shouldContain` [" Comment between productions"]
          commentTexts `shouldContain` [" Comment within function parameters"]
          commentTexts `shouldContain` [" Another parameter comment"]
          commentTexts `shouldContain` [" Comment before last production"]

    it "handles files with trailing comments at end of file" $ do
      let trailingCommentsInput = unlines [
            "syntax {",
            "  elem in Any;",
            "  l in List ::= nil() | cons(elem, l);",
            "};",
            "rules {",
            "  rule \"test\" [] => [append(nil(), l, l)];",
            "};",
            "% test: this is a trailing comment",
            "% test: another trailing comment"
            ]
      case parseProgram trailingCommentsInput of
        Left err -> expectationFailure $ "Parse error: " ++ show err
        Right (Program decls comments) -> do
          length decls `shouldBe` 2
          length comments `shouldBe` 2
          let commentTexts = map (\(Comment content _) -> content) comments
          commentTexts `shouldContain` [" test: this is a trailing comment"]
          commentTexts `shouldContain` [" test: another trailing comment"]