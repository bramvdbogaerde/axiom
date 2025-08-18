{-# LANGUAGE TemplateHaskell #-}

import Language.AST
import Language.AST.CodeGen
import Language.Range
import Data.Functor.Identity

testRange :: Range
testRange = Range (Position 1 1 Nothing) (Position 1 5 Nothing)

-- Test that we can generate code for an empty program
generatedEmptyProgram :: Program
generatedEmptyProgram = $(astToCode (Program [] []))

-- Test with a more complex AST containing syntax declarations
complexAST :: Program
complexAST = Program 
  [ Syntax 
      [ SyntaxDecl ["x"] "Expr" 
          [ Atom (Identity "var") testRange
          , Functor "add" [Atom (Identity "x") testRange, Atom (Identity "y") testRange] testRange
          ] testRange
      ] testRange
  , Rewrite 
      (RewriteDecl "simplify" [Atom (Identity "x") testRange] (Atom (Identity "x") testRange) testRange) 
      testRange
  ] 
  [Comment "Test comment" testRange]

generatedComplexProgram :: Program
generatedComplexProgram = $(astToCode complexAST)

main :: IO ()
main = do
  let originalEmpty = Program [] []
  putStrLn "=== Empty Program Test ==="
  putStrLn "Original program:"
  print originalEmpty
  putStrLn "Generated program (from Template Haskell):"
  print generatedEmptyProgram
  putStrLn $ "Are they equal? " ++ show (originalEmpty == generatedEmptyProgram)
  
  putStrLn "\n=== Complex Program Test ==="
  putStrLn "Original complex program:"
  print complexAST
  putStrLn "Generated complex program (from Template Haskell):"
  print generatedComplexProgram
  putStrLn $ "Are they equal? " ++ show (complexAST == generatedComplexProgram)
  
  putStrLn "\n=== Template Haskell Code Generation Test ==="
  putStrLn "The $(astToCode prog) splice generates Haskell code that constructs the AST."
  putStrLn "This demonstrates that we can take an AST and generate Haskell code"
  putStrLn "that, when executed, recreates that same AST."