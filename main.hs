import Data.List
import Data.List.Split 
import System.IO
import Data.Maybe 
import Data.Ord
import System.Environment

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

trim ch = dropWhileEnd (==ch) . dropWhile (==ch)

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs


getStates :: (Eq a1, Eq a2) => [(a2, [(a1, [a3])])] -> a2 -> a1 -> [a3]
getStates table state symbol = fromMaybe [] $ lookup symbol (fromJust $ lookup state table)

parse :: [Char] -> ([Char], [Char], [([Char], [([Char], [[Char]])])])
parse str = 
    let 
       [s,t] = splitOn ['|'] (trim ' ' str)
       state =  trim ' ' (s \\ ['>', '*'])
       startState = if '>' `elem` s then state else ""
       finalState = if '*' `elem` s then state else ""
       transitions = [(head list, tail list) | list <- parseTransition $ trim ' ' t]
       parseTransition text = map (splitOn [' ']. trim ' ') (splitOn [','] text)
    in
        (startState, finalState,[(state,transitions)])


nondeterministicA = NFA ["0", "1","2"] ["a", "b"] ["0"] ["2"] [("0", [("a",["0","1"]),("b",["0"])]),("1", [("b",["2"])]),("2", [("a",["0","2"]),("b",["1"])])]

subsetConstruction :: Ord symbol => NFA symbol -> DFA symbol
subsetConstruction nfa=
  let
    newFinalStates =  [concat s | s <- powerset $ states nfa, intersect s (finalStates nfa) /= []  ]
    newTable = [ (concat s,  [(sym, foldl union [] [ getStates (transitionTable nfa) p sym | p <- s, isJust $ lookup p (transitionTable nfa)]) | sym <- alphabet nfa]) | s <- powerset $ states nfa, s /= [] ]  
    newStates = map fst newTable
    
  in
    DFA newStates (alphabet nfa) (head $ nfaStartStates nfa) newFinalStates newTable
sel1 (x,y,z) = x
sel2 (x,y,z) = y
sel3 (x,y,z) = z

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> doHandler stdin
    file:_ -> withFile file ReadMode doHandler

doHandler :: Handle -> IO ()
doHandler h = do
  s <- hGetContents h
  print $ finalOut (lines s)

  
finalOut contents =  
    let 
      parsed = map parse contents
      table = concatMap sel3 parsed 
      startStates = map sel1 parsed
      finalStates = map sel2 parsed
      parsedStates = map fst table
      symbols = maximumBy (comparing length) (map (map fst . snd) table)
      deterministic = subsetConstruction  (NFA parsedStates symbols startStates finalStates table)
    in 
      deterministic