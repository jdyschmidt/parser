module TokenizerTests (astTester, astTests, nfaTests, nfaTester) where

import RegexToNFA(Node(..), buildAST, astToNFA)
import NFA(NFA(..), State(..))
import UUID(discardUniqueness)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe


astTests :: [(String, Node)]
astTests = [("a", Concat[C('a')]),
                  ("(ab)*[ba(123)]a", Concat[Repeat(Concat[C('a'),C('b')]), Alt[C('b'),C('a'),Concat[C('1'),C('2'),C('3')]], C('a')])]

astTester :: (String, Node) -> Maybe String
astTester (regex, expect) = let ast = fst $ buildAST regex [] in
                                if ast == expect then Nothing else Just $ "Expected " ++ (show expect) ++ " from regex \"" ++ regex ++ "\", got " ++ (show ast)


nfaTests :: [(Node, NFA () -> Bool)]
nfaTests = [(C('a'), \nfa -> (Map.size (states nfa)) == 2 &&
                                (Set.size (final nfa)) == 1 &&
                                fromMaybe False ((==1) <$> (Map.size <$> (delta <$> (Map.lookup (start nfa) (states nfa))))) &&
                                fromMaybe False ((\m -> Set.member m (final nfa)) <$>
                                    (Set.elemAt 0 <$> ((delta <$> (Map.lookup (start nfa) (states nfa))) >>= Map.lookup 'a'))))]

nfaTester :: (Node, NFA () -> Bool) -> Maybe String
nfaTester (ast, nfaCheck) = let nfa = discardUniqueness $ astToNFA ast in
                                if nfaCheck nfa then Nothing else Just $ "Incorrect NFA " ++ (show nfa) ++ " from AST " ++ (show ast)
