import Language.Parser
import Language.Lexer

main = do
  content <- readFile "tests/list.sem"
  let tokens = lex content
  putStrLn "=== TOKENS ==="
  mapM_ print tokens
  putStrLn "\n=== PARSE RESULT ==="
  case parseProgram content of
    Left err -> print err
    Right prog -> print prog