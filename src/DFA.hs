module DFA (
    DFA(..),
    State(..),
    nfaToDFA,
    contractEpsilons,
    weedDisconnected
) where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe

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

nfaToDFA :: Eq a => NFA.NFA a -> IDGen (DFA (Set.Set a))
nfaToDFA = collectTransitions <=< (return . weedDisconnected . contractEpsilons)

determinize :: NFA.NFA a -> DFA a
determinize nfa = let determinizeDelta = Map.map (\v -> if (Set.size v) /= 1
                                                            then error $ "Too many transitions: " ++ (show v)
                                                            else Set.elemAt 0 v)
                      determinizeState s = State { delta = determinizeDelta $ NFA.delta s, label = NFA.label s, info = NFA.info s } in
                        DFA { states = Map.map determinizeState $ NFA.states nfa, final = NFA.final nfa, start = NFA.start nfa }

buildStateSetDelta :: NFA.NFA a -> Set.Set UUID -> Map.Map Char (Set.Set UUID)

generateState :: NFA.NFA a -> Set.Set UUID -> State (Set.Set a)
generateState nfa nfaID = let nfaState = fromJust $ Map.lookup nfaID NFA.states

--  Map is from NFA state sets to new DFA states
generateStates :: NFA.NFA a -> Set.Set UUID -> Map.Map (Set.Set UUID) UUID -> DFA (Set.Set a) -> IDGen(Map.Map (Set.Set UUID) UUID, DFA (Set.Set a))
generateStates nfa currentStateIDs nfaToDFA prevDFA = let 
    nfaStates = NFA.states nfa
    currentStates = Set.foldr (Set.insert (\id -> fromJust $ Map.lookup id nfaStates)) Set.empty currentStateIDs
    requiredTransitions = Set.foldr (Set.union . Map.keysSet . NFA.delta) Set.empty currentStates
    followTransitions :: Char -> Set.Set UUID
    followTransitions t = Set.foldr (Set.union . (\state -> fromMaybe Set.empty (Map.lookup t $ NFA.delta state))) Set.empty currentStates
        -- APPLY followTransitions ACCORDING TO requiredTransitions


collectTransitions :: NFA.NFA a -> IDGen (DFA (Set.Set a))
collectTransitions nfa = let rootID = nextID
                             rootState :: State
                             rootState = State {} in -- Use generateState
                                generateStates nfa (Map.singleton rootID (NFA.start nfa)) (DFA { states = Set.singleton rootState, final = isFinal rootState, start = rootID })

contractEpsilonBad :: Eq a => (Map.Map UUID (NFA.State a), Set.Set UUID) -> UUID -> (Map.Map UUID (NFA.State a), Set.Set UUID) -> (Map.Map UUID (NFA.State a), Set.Set UUID)
contractEpsilonBad (allStates, oldFinal) root (prevVisited, prevFinal) = let epsilonStateIDs = fromMaybe Set.empty $ Map.lookup '\0' $ NFA.delta $ fromJust $ Map.lookup root allStates
                                                                             (newVisited, newFinal) = Set.foldr (contractEpsilonBad (allStates, oldFinal)) (prevVisited, Set.empty) epsilonStateIDs
                                                                             thisFinal = if Set.disjoint oldFinal newFinal then newFinal else Set.insert root newFinal
                                                                             oldState = fromJust $ Map.lookup root allStates
                                                                             newEpsilons = Set.map (\uuid -> fromJust $ Map.lookup uuid newVisited) epsilonStateIDs
                                                                             newDelta = Set.foldr ((Map.unionWith Set.union) . NFA.delta) (NFA.delta oldState) newEpsilons
                                                                             disconnectedDelta = Map.delete '\0' newDelta
                                                                             newState = NFA.State { NFA.label = root, NFA.info = NFA.info oldState, NFA.delta = disconnectedDelta } in
                                                                                if Map.member root prevVisited then (prevVisited, prevFinal)
                                                                                    else (Map.insert root newState newVisited, Set.union thisFinal prevFinal)

--  This won't give the optimal DFA if there is an epsilon-cycle.
--  If we ignore cycles we can get mn time; by including cycles (naively) we get (mn)^2 time. Yay!
--  Better solutions:
--    - Patch out cycles during tree to NFA
--    - Only fallback to (mn)^2 when a cycle is detected (probably best)
--  Probable solution:
--    - Run DFA-minimization later, because it'll be fun (and even slower, yay!)
--  Current solution:
--    - Ignore it and hope for the best. My favourite solution.
contractEpsilonsBad :: Eq a => NFA.NFA a -> NFA.NFA a
contractEpsilonsBad nfa = let (visited, newFinal) = Set.foldr (\root -> \(_, prevFinal) -> contractEpsilonBad (NFA.states nfa, NFA.final nfa) root (Map.empty, prevFinal)) (Map.empty, Set.empty) (Map.keysSet $ NFA.states nfa) in
                            nfa { NFA.states = visited, NFA.final = newFinal }

collectEpsilons :: Eq a => NFA.NFA a -> UUID -> Set.Set UUID -> Set.Set UUID
collectEpsilons nfa rootID acc = let epsilons = Map.findWithDefault Set.empty '\0' $ NFA.delta $ fromJust $ Map.lookup rootID $ NFA.states nfa in
    Set.foldr (\travelID -> \acc -> if Set.member travelID acc then acc else Set.union acc $ collectEpsilons nfa travelID acc) acc epsilons

collapseStates :: Eq a => NFA.NFA a -> UUID -> Set.Set UUID -> NFA.NFA a
collapseStates nfa rootID toCollapseIDs = let oldStates = NFA.states nfa
                                              oldRoot = fromJust $ Map.lookup rootID oldStates
                                              toCollapse = Set.map (\id -> fromJust $ Map.lookup id oldStates) toCollapseIDs
                                              newDelta = Set.foldr (\state -> \newDelta -> Map.union Set.union newDelta $ NFA.delta state)
                                                                   (NFA.delta oldRoot)
                                                                   toCollapse
                                              oldFinal = NFA.final nfa
                                              checkFinal :: [UUID] -> Bool
                                              checkFinal states = if states == [] then False else
                                                let (head:tail) = states in (Set.member head oldFinal) || (checkFinal tail)
                                              newRoot = oldRoot { NFA.delta = newDelta }
                                              newStates = Map.insert rootID newRoot $ Map.withoutKeys oldStates toCollapseIDs
                                              newFinal = if checkFinal (rootID : (Map.keys oldStates)) then Set.insert rootID oldFinal else oldFinal in
                                                nfa { NFA.states = newStates, NFA.final = newFinal }

contractEpsilons :: Eq a => NFA.NFA a -> NFA.NFA a
contractEpsilons nfa = let oldIDs = Map.keysSet $ NFA.states nfa 
                           contractEpsilon :: Eq a => UUID -> NFA.NFA a -> NFA.NFA a 
                           contractEpsilon id nfa = case Map.lookup id $ NFA.states nfa of
                                                        Just state -> collapseStates nfa id $ collectEpsilons nfa id Set.empty
                                                        Nothing -> nfa in
    Set.foldr contractEpsilon nfa oldIDs

weedDisconnected :: NFA.NFA a -> NFA.NFA a
weedDisconnected nfa = let allStates = NFA.states nfa
                           visit :: UUID -> Set.Set UUID -> Set.Set UUID
                           visit stateID visited = if Set.member stateID visited then visited else
                                                    let state = fromJust $ Map.lookup stateID allStates
                                                        nextIDs = Map.foldr Set.union Set.empty (NFA.delta state) in
                                                            Set.foldr visit (Set.insert stateID visited) nextIDs
                           connectedIDs = visit (NFA.start nfa) Set.empty
                           connectedStates = Map.restrictKeys (NFA.states nfa) connectedIDs
                           finalConnected = Set.intersection connectedIDs (NFA.final nfa) in
                            nfa { NFA.states = connectedStates, NFA.final = finalConnected }
