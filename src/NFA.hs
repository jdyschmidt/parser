module NFA (
    NFA(..),
    State(..),
    emptyNFA,
    insertTransition
) where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import UUID(UUID, IDGen, nextID)
import Data.Maybe


-----------------------------------------------------------------------------------

data State a = State { label :: UUID, info :: a, delta :: Map.Map Char (Set.Set UUID) } deriving(Show)
instance (Eq a) => Eq (State a) where
    State l1 _ _ == State l2 _ _ = l1 == l2
instance (Eq a) => Ord (State a) where
    State l1 _ _ <= State l2 _ _ = l1 <= l2


-----------------------------------------------------------------------------------

-- Epsilon is represented by NUL char '\0'
data NFA a = NFA { states :: Map.Map UUID (State a), final :: Set.Set UUID, start :: UUID }
instance (Show a) => Show (NFA a) where
    show nfa = "NFA { start = " ++ (show $ start nfa) ++
            ",\n      final = " ++ (show $ final nfa) ++
            ",\n      states = " ++ foldr (\s -> (++ "               " ++ (show s) ++ "\n")) "" (states nfa)


emptyNFA :: a -> IDGen (NFA a)
emptyNFA newInfo = do
    sID <- nextID
    let s = State { label = sID, info = newInfo, delta = Map.empty } in
        return NFA { states = Map.singleton sID s, final = Set.singleton sID, start = sID }


insertTransition :: State a -> Char -> UUID -> State a
insertTransition from c toID = let deltas = delta from
                                   oldDelta = fromMaybe Set.empty $ Map.lookup c deltas
                                   newDelta = Map.insert c (Set.insert toID oldDelta) deltas in
                                       from { delta = newDelta}
