module Main where

import Tester(runTests)

main = do
    runTests "Tokenizer"

{- main = do
    input <- getContents
    putStr $ buildAST (tail input) C(head input) -}