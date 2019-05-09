module Tests (suites) where

import Data.Maybe

import RegexToDFA(Node(Repeat, Alt, Concat, C), buildAST)

suites :: [String -> IO ()]
suites = [test "Tokenizer" tokenizer tokenizerTests]

tokenizerTests = [("a", Concat[C('a')])]

tokenizer :: (String, Node) -> Maybe String
tokenizer (regex, expect) = let ast = fst $ buildAST regex [] in
                         if ast == expect then Nothing else Just $ "Expected " ++ (show expect) ++ " from \"" ++ regex ++ "\", got " ++ (show ast)

test :: String -> ((i, e) -> Maybe String) -> [(i, e)] -> (String -> IO ())
test name runner tests = let results = foldl (\rs -> \r -> rs ++ "  " ++ r ++ "\n") "" $ catMaybes $ map runner tests in
                         if results == "" then (\_ -> putStr $ name ++ ": pass\n")
                                          else (\_ -> putStr $ name ++ ": fail\n" ++ results)