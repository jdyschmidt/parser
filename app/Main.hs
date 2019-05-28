module Main where

import RegexToNFA(buildAST, Node(..))

main :: IO ()
main = do
    input <- getContents
    putStr $ snd $ buildAST (tail input) [C(head input)]