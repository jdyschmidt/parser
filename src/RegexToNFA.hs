module RegexToNFA (
    Node(Repeat, Alt, Concat, C),
    buildAST,
    astToNFA,
    regexToNFA
) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import UUID(UUID, IDGen, discardUniqueness, nextID)
import NFA(NFA(..), State(..), emptyNFA, insertTransition)


------------------------------------------------------------------------------

data Node = Repeat Node | Alt [Node] | Concat [Node] | C Char deriving(Eq, Show)
instance Ord Node where
    Repeat _ <= Alt _ = True
    Repeat _ <= Concat _ = True
    Repeat _ <= C _ = True
    Alt _ <= Repeat _ = False
    Alt _ <= Concat _ = True
    Alt _ <= C _ = True
    Concat _ <= Repeat _ = False
    Concat _ <= Alt _ = False
    Concat _ <= C _ = True
    C _ <= Repeat _ = False
    C _ <= Alt _ = False
    C _ <= Concat _ = False
    Repeat n1 <= Repeat n2 = n1 <= n2
    Alt ns1 <= Alt ns2 = ns1 <= ns2
    Concat ns1 <= Concat ns2 = ns1 <= ns2
    C a1 <= C a2 = a1 <= a2

buildAST :: String -> [Node] -> (Node, String)
buildAST [] ns = (Concat(reverse ns), "")
buildAST (')':rs) ns = (Concat(reverse ns), rs)
buildAST (']':rs) ns = (Alt(reverse ns), rs)
buildAST ('*':rs) (n:ns) = buildAST rs $ Repeat(n) : ns
buildAST ('(':r:rs) ns = let (group, rest) = buildAST rs [C(r)]
                         in buildAST rest $ group : ns
buildAST ('[':r:rs) ns = buildAST ('(':r:rs) ns --Group type determined by end paren, matching not checked.
buildAST ('\\':r:rs) ns = buildAST rs $ C(r) : ns
buildAST (r:rs) ns = buildAST rs $ C(r) : ns


-------------------------------------------------------------------------------------

astToNFA :: Node -> IDGen (NFA ())
astToNFA (C b) = do
    sID <- nextID
    fID <- nextID
    let s = State sID () (Map.fromList [(b, Set.singleton fID)])
        f = State fID () Map.empty in
            return $ NFA (Map.fromList [(sID, s), (fID, f)]) (Set.singleton fID) sID
astToNFA (Concat ns) = do
    nfas <- sequenceA $ map astToNFA ns
    foldr
        (\beforeNFA -> \baseNFAWithUUID -> do
            baseNFA <- baseNFAWithUUID
            let nextStart = start baseNFA
                linkedFinals = Set.map
                                (\fID -> let f = fromJust $ Map.lookup fID $ states beforeNFA in
                                                    insertTransition f '\0' nextStart)
                                (final beforeNFA)
                newStates = Set.foldr (\f -> Map.insert (label f) f) (states beforeNFA) linkedFinals in
                    return baseNFA { states = Map.union newStates $ states baseNFA, start = start beforeNFA })
        (emptyNFA ())
        nfas
astToNFA (Alt ns) = do
    nfas <- sequenceA $ map astToNFA ns
    startID <- nextID
    let s = State { label = startID, info = (), delta = Map.singleton '\0' starts }
        starts = foldr (Set.union . Set.singleton . start) Set.empty nfas
        finals = foldr (Set.union . final) Set.empty nfas
        allStates = Map.union (Map.singleton startID s) (foldMap states nfas) in
            return NFA { states = allStates, final = finals, start = startID }
astToNFA (Repeat n) = do
    nfa <- astToNFA n
    let startID = start nfa
        finals = final nfa
        wrapToStart :: State a -> State a
        wrapToStart s = insertTransition s '\0' startID
        finalFilter f s | (Set.member (label s) finals) = f s
                        | otherwise                     = s
        repeatStates = Map.map (finalFilter wrapToStart) (states nfa) in
        return nfa { states = repeatStates, final = Set.insert startID finals }


regexToNFA :: String -> IDGen (NFA ())
regexToNFA = astToNFA . fst . (\s -> buildAST s [])
