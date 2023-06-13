import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map as M
import qualified Data.List as L
import Data.Array ( listArray, Array )
import System.IO
import Data.Maybe 
import Data.Either
import Data.Ord
import System.Environment
import Control.Monad

class FA a where
  states :: a state symbol -> [[state]]
  alphabet :: a state symbol -> [symbol]
  -- startStates :: a state symbol -> Either state [state]
  finalStates :: a state symbol -> [[state]]
  transitionTable :: a state symbol -> M.Map [state] (M.Map symbol [[state]])

-- data NFA state symbol = NFA [state] [symbol] [state] [state] (M.Map state (M.Map symbol [state]))
data NFA state symbol = NFA {
  nfaStates :: [[state]],
  nfaAlphabet :: [symbol],
  nfaStartStates :: [[state]],
  nfaFinalStates :: [[state]],
  nfaTransitionTable :: M.Map [state] (M.Map symbol [[state]])
} deriving (Show)

data DFA state symbol = DFA {
  dfaStates :: [[state]],
  dfaAlphabet :: [symbol],
  dfaStartState :: [state],
  dfaFinalStates :: [[state]],
  dfaTransitionTable :: M.Map [state] (M.Map symbol [[state]])
}

instance FA NFA where
  
  states = nfaStates
  alphabet = nfaAlphabet
  finalStates = nfaFinalStates
  transitionTable = nfaTransitionTable

instance FA DFA where
  states = dfaStates
  alphabet = dfaAlphabet
  -- startStates dfa = Left (dfaStartState dfa)
  finalStates = dfaFinalStates
  transitionTable = dfaTransitionTable

instance (Eq state,Show state, Show symbol) => Show (DFA state symbol) where
  show dfa = do
    st <- M.assocs $ transitionTable dfa
    filter (not . (`elem` "\"")) (format st) where
      format :: ([state], M.Map symbol [[state]]) -> String
      format transition =
        let 
          st = fst transition
          state   | st == dfaStartState dfa = "> " ++ show st 
                  | st `elem` finalStates dfa =  "* " ++ show st 
                  | otherwise = "  " ++ show st
          (_:symbolTrStates) = concat [ ", "++((show.fst) syTr ++ " " ++ unwords (L.map show (snd syTr)))  |  syTr <- M.assocs $ snd transition]
        in
           state ++ " |" ++ symbolTrStates ++ "\n"

  

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

getStates :: (Ord k1, Ord k2) => M.Map k2 (M.Map k1 [a]) -> k2 -> k1 -> [a]
getStates table state symbol = fromMaybe [] $ M.lookup symbol (fromJust $ M.lookup state table)

parse str = 
    let 
       [s,t] = T.splitOn (T.singleton '|') (T.pack str)
       state = T.dropAround (\x -> x == '>' || x == '*' || x == ' ') s
       startState = if T.elem '>' s then state else T.pack ""
       finalState = if T.elem '*' s then state else T.pack ""
       transitions = [(T.unpack $ L.head list, L.map T.unpack (L.tail list)) | list <- parseTransition t] where
       parseTransition text = L.map (T.splitOn (T.singleton ' ')) $ T.splitOn (T.singleton ',') (T.strip text)
    in
        (T.unpack startState, T.unpack finalState,[(T.unpack state, M.fromList transitions)])


nondeterministicA = NFA ["0", "1","2"] ["a", "b"] ["0"] ["2"] (M.fromList [("0",M.fromList [("a",["0","1"]),("b",["0"])]),("1",M.fromList [("b",["2"])]),("2",M.fromList [("a",["0","2"]),("b",["1"])])])

subsetConstruction :: Eq state => Ord state => Ord symbol => NFA state symbol -> DFA state symbol
subsetConstruction nfa=
  let
    newFinalStates =  [concat s | s <- powerset $ states nfa, L.intersect s (finalStates nfa) /= []  ]
    newTable = M.fromList [ (concat s, M.fromList [(sym, foldl L.union [] [ getStates (transitionTable nfa) p sym | p <- s, isJust $ M.lookup p (transitionTable nfa)]) | sym <- alphabet nfa]) | s <- powerset $ states nfa, s /= [] ]  
    newStates = M.keys newTable
    
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

doHandler :: Handle -> IO()
doHandler h = handleHelper h [] where
     handleHelper handle input = do
      eof <- hIsEOF handle
      if eof then
        print $ finalOut input
      else do
        line <- hGetLine handle
        handleHelper handle (input++[line])

  

finalOut :: [String] -> DFA Char String
finalOut contents =  
    let 
      parsed = L.map parse contents
      table = M.fromList $ concatMap sel3 parsed 
      startStates = map sel1 parsed
      finalStates = map sel2 parsed
      parsedStates = M.keys table
      symbols = L.maximumBy (comparing length) (L.map (M.keys . snd) (M.assocs table))
      deterministic = subsetConstruction  (NFA parsedStates symbols startStates finalStates table)
    in 
      deterministic