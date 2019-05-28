import Tester(runSuite)

main :: IO ()
main = do
    runSuite "Tokenizer AST"
    runSuite "Tokenizer NFA"