module Automata where
import Data.List
import Data.Maybe 

type State = String

class FA a where
  states :: a symbol -> [State]
  alphabet :: a symbol -> [symbol]
  finalStates :: a symbol -> [State]
  transitionTable :: a symbol -> [(State,[(symbol,[State])])]

data NFA symbol = NFA {
  nfaStates ::[State],
  nfaAlphabet :: [symbol],
  nfaStartStates :: [State],
  nfaFinalStates :: [State],
  nfaTransitionTable :: [(State,[(symbol,[State])])]
} deriving (Show)

data DFA symbol = DFA {
  dfaStates :: [State],
  dfaAlphabet :: [symbol],
  dfaStartState :: State,
  dfaFinalStates :: [State],
  dfaTransitionTable :: [(State,[(symbol,[State])])]
}

instance FA NFA where
  
  states = nfaStates
  alphabet = nfaAlphabet
  finalStates = nfaFinalStates
  transitionTable = nfaTransitionTable

instance FA DFA where
  states = dfaStates
  alphabet = dfaAlphabet
  finalStates = dfaFinalStates
  transitionTable = dfaTransitionTable


instance (Show symbol) => Show (DFA symbol) where
  show dfa = do
    st <- transitionTable dfa
    filter (not . (`elem` "\"")) (format st) where
      format transition =
        let 
          st = fst transition
          state   | st == dfaStartState dfa = "> " ++ show st 
                  | st `elem` finalStates dfa =  "* " ++ show st 
                  | otherwise = "  " ++ show st
          (_:symbolTrStates) = concat [ ", "++((show.fst) syTr ++ " " ++ unwords (map show (snd syTr)))  |  syTr <- snd transition]
        in
           state ++ " |" ++ symbolTrStates ++ "\n"

keys = map fst

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

mylookup :: State -> [(State, [p])] -> [p]
mylookup state table = 
  let r = find ((\x -> null (x \\ state)) . fst) table 
      j | isJust r = snd $ fromJust r
        | otherwise = []
  in
    j 
-- getStates :: (Eq a1, Eq a2) => [(State, [(a1, [State])])] -> a2 -> State -> [State]
getStates table state symbol = fromMaybe [] $ lookup symbol ( mylookup state table)

nondeterministicA = NFA ["0", "1","2"] ["a", "b"] ["0"] ["2"] [("0", [("a",["0","1"]),("b",["0"])]),("1", [("b",["2"])]),("2", [("a",["0","2"]),("b",["1"])])]

subsetConstruction :: Ord symbol => NFA symbol -> DFA symbol
subsetConstruction nfa=
  let
    newFinalStates =  [concat s | s <- powerset $ states nfa, intersect s (finalStates nfa) /= []  ]
    newTable = [ (concat s,  [(sym, foldl union [] [ getStates (transitionTable nfa) p sym | p <- s, isJust $ lookup p (transitionTable nfa)]) | sym <- alphabet nfa]) | s <- powerset $ states nfa, s /= [] ]  
    newStates = keys newTable
    
  in
    DFA newStates (alphabet nfa) (head $ nfaStartStates nfa) newFinalStates newTable


partialTransition fa state symbol = 
  let 
    nextSt = getStates (transitionTable fa) state symbol 
  in
    nextSt