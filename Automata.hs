module Automata where
import Data.List
import Data.Maybe 
import Control.Monad

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
}

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
  show (DFA q syms start f table) = show (NFA q syms [start] f table)  

instance (Show symbol) => Show (NFA symbol) where
  show nfa = do

    t <- transitionTable nfa
    filter (not . (`elem` "\"")) (format t) where

      format (st,transition)  =
        let
          state   | st `elem` finalStates nfa  && st `elem` nfaStartStates nfa = ">* " ++ show st 
                  | st `elem` nfaStartStates nfa = ">  " ++ show st 
                  | st `elem` finalStates nfa =  "*  " ++ show st 
                  | otherwise = "   " ++ show st
          (_:symbolTrStates) = 
            intercalate "," 
           [(show from ++ " " ++ unwords (map show to)) | (from, to) <- transition]
        in
           state ++ " |" ++ symbolTrStates ++ "\n"
           
keys :: [(a, b)] -> [a]
keys = map fst

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

getStates :: Eq a1 => [(State, [(a1, [a2])])] -> State -> a1 -> [a2]
getStates table state symbol = fromMaybe [] $ lookup symbol $ fromMaybe [] ( lookup state table)

subsetConstruction :: Ord symbol => NFA symbol -> DFA symbol
subsetConstruction nfa=
  let
    newFinalStates =  [ concat s | s <- powerset $ states nfa, intersect s (finalStates nfa) /= [] ]
    newTable = subsetConstructionTable (transitionTable nfa) nfa
    newStates = keys newTable
    
  in
    DFA newStates (alphabet nfa) (head $ nfaStartStates nfa) newFinalStates newTable

subsetConstructionTable table nfa = do
  s <- filter (/= []) $ powerset $ states nfa
  let first = concat s
  let second = do
        sym <- alphabet nfa
        let stateList = do
              p <- s
              guard (isJust $ lookup p (transitionTable nfa))
              return $ getStates (transitionTable nfa) p sym
        return (sym, foldl union [] stateList)
  return (first, second)


partialTransition :: (Eq a1, FA a2) => a2 a1 -> State -> a1 -> [State]
partialTransition fa = getStates (transitionTable fa) 
extendedTransition :: FA a => Eq sym => a [sym] -> [sym] -> State -> State  
extendedTransition fa as state = foldl f state as
  where f state a = concat $ partialTransition fa state [a]

isAccepted fa w = extendedTransition fa w (dfaStartState fa) `elem` finalStates fa

applyUntil :: Eq a => (a -> a) -> a -> a
applyUntil f s 
    | s == next = s
    | otherwise = applyUntil f next
      where next = f s

distinguishable :: Eq symbol => DFA symbol -> [(State,State)]
distinguishable dfa = 
  applyUntil (\x -> nub (x ++ findAllMarked (states dfa) x)) (marked (states dfa))

  where 
    marked []  = []
    marked (x:xs) = 
        [(x,a) | a <- xs , (x `elem` finalStates dfa) /= (a `elem` finalStates dfa) ] ++ marked xs

    isDist table (a,b) x dist = 
        (concat $ getStates table a x , concat $ getStates table b x) `elem` dist
    
    findAllMarked []  _  = []
    findAllMarked (a:as) dist = 
        [ (a,b) | b <- states dfa , x <- alphabet dfa , isDist (transitionTable dfa) (a,b) x dist ]
        ++ findAllMarked as dist

undistinguishable :: Eq symbol => DFA symbol -> [(State,State)] -> [[State]]
undistinguishable dfa dist =
        isfindUniqueDist 
        [ i : [j | j <- states dfa , (i,j) `notElem` dist, (j,i) `notElem` dist, j/= i ] 
        | i <- states dfa ]

isfindUniqueDist :: Eq a => [[a]] -> [[a]]
isfindUniqueDist [] = []
isfindUniqueDist xs = 
  [x | let y = tail xs, x <- xs, null $ concatMap (x `intersect`) y] ++ isfindUniqueDist (tail xs)

minimizeDFA :: Eq symbol => DFA symbol -> DFA symbol
minimizeDFA dfa = 
  let 
    dist = distinguishable dfa
    und = undistinguishable dfa dist
    table = minimizeDfaTable und dfa
    fstates = nub [unwords $ concat $ filter (elem f) und | f <- finalStates dfa]
    sstate = unwords $ concat $ filter (elem (dfaStartState dfa)) und
  in
    DFA (map unwords und) (alphabet dfa) sstate fstates table

minimizeDfaTable und dfa = do
    ust <- und
    let fstElem = unwords ust
    let sndElem = do
          sy <- alphabet dfa
          let innerElem = do
                st <- ust
                let g = getStates (transitionTable dfa) st sy
                let filtered = filter (elem (concat g)) und
                return $ unwords $ concat filtered
          return (sy, nub innerElem)
    return (fstElem, sndElem)
