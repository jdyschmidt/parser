module RegexToDFA (
    Node(Repeat, Alt, Concat, C),
    buildAST
) where

import Data.Char

data Node = Repeat Node | Alt [Node] | Concat [Node] | C Char deriving(Eq, Show)

buildAST :: String -> [Node] -> (Node, String)
buildAST [] ns = (Concat(reverse ns), "")
buildAST (')':rs) ns = (Concat(reverse ns), rs)
buildAST (']':rs) ns = (Alt(ns), rs)
buildAST ('*':rs) (n:ns) = buildAST rs $ Repeat(n) : ns
buildAST ('(':r:rs) ns = let (group, rest) = buildAST rs [C(r)]
                         in buildAST rest $ group : ns
buildAST ('[':r:rs) ns = buildAST ('(':r:rs) ns --Group type determined by end paren, matching not checked.
buildAST ('\\':r:rs) ns = buildAST rs $ C(r) : ns
buildAST (r:rs) ns = buildAST rs $ C(r) : ns