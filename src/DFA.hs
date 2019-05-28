module DFA (
    DFA(..),
    State(..)
) where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified NFA(NFA(..), State(..))
import UUID


---------------------------------------------------------------------------------

data State a = State { label :: UUID, info :: a, delta :: Map.Map Char UUID } deriving(Show)
instance (Eq a) => Eq (State a) where
    State l1 _ _ == State l2 _ _ = l1 == l2
instance (Eq a) => Ord (State a) where
    State l1 _ _ <= State l2 _ _ = l1 <= l2


-----------------------------------------------------------------------------------

data DFA a = DFA { states :: Map.Map UUID (State a), final :: Set.Set UUID, start :: UUID }
instance (Show a) => Show (DFA a) where
    show dfa = "DFA { start = " ++ (show $ start dfa) ++
            ",\n      final = " ++ (show $ final dfa) ++
            ",\n      states = " ++ foldr (\s -> (++ "               " ++ (show s) ++ "\n")) "" (states dfa)


-----------------------------------------------------------------------------------

nfaToDFA :: NFA.NFA a -> IDGen (DFA a)
nfaToDFA = (return . determinize) <=< collectTransitions <=< contractEpsilons

determinize :: NFA.NFA a -> DFA a
determinize nfa = let determinizeDelta = Map.map (\v -> if (Set.size v) /= 1
                                                            then error $ "Too many transitions: " ++ (show v)
                                                            else Set.elemAt 0 v)
                      determinizeState s = State { delta = determinizeDelta $ NFA.delta s, label = NFA.label s, info = NFA.info s } in
                        DFA { states = Map.map determinizeState $ NFA.states nfa, final = NFA.final nfa, start = NFA.start nfa }

collectTransitions :: NFA.NFA a -> IDGen (NFA.NFA a)
collectTransitions nfa = return nfa

contractEpsilons :: NFA.NFA a -> IDGen (NFA.NFA a)
contractEpsilons nfa = return nfa