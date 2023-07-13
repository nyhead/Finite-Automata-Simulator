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

printFA :: Show sym => NFA sym -> Bool -> String
printFA nfa isDFA = do
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
            [show from ++ " " ++ printTransition to | (from, to) <- transition] where
            printTransition tr 
              | isDFA = concatMap show tr
              | otherwise =  unwords (map show tr)
        in
           state ++ " |" ++ symbolTrStates ++ "\n"


instance (Show symbol) => Show (NFA symbol) where
  show nfa = printFA nfa False

instance (Show symbol) => Show (DFA symbol) where
  show (DFA q syms start f table) = printFA (NFA q syms [start] f table) True

           
keys :: [(a, b)] -> [a]
keys = map fst

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs

getSymbols :: [(sym, [State])] -> [sym]
getSymbols = map fst

getAlphabet :: Eq sym => [(State, [(sym, [State])])] -> [sym]
getAlphabet = nub . concatMap (getSymbols . snd)


getStates :: Eq a1 => [(State, [(a1, [a2])])] -> State -> a1 -> [a2]
getStates table state symbol = fromMaybe [] $ lookup symbol $ fromMaybe [] ( lookup state table)

partialTransition :: (Eq a1, FA a2) => a2 a1 -> State -> a1 -> [State]
partialTransition fa = getStates (transitionTable fa) 

extendedTransition :: FA a => Eq sym => a [sym] -> [sym] -> State -> State  
extendedTransition fa as state = foldl f state as
  where f state a = concat $ partialTransition fa state [a]

isAccepted :: Eq sym => DFA [sym] -> [sym] -> Bool
isAccepted fa w = extendedTransition fa w (dfaStartState fa) `elem` finalStates fa


reachable :: Eq sy => DFA sy -> [State]
reachable dfa = findAllReachable [dfaStartState dfa] where
  findAllReachable mi = 
    let 
      mi1 = mi `union` 
        [concat q | p <- mi, q <- maybe [] (map snd) (lookup p (transitionTable dfa))]
    in
      if mi1 /= mi then
        findAllReachable mi1
      else
        mi1
  
delUnReachable :: Eq sy => DFA sy -> DFA sy
delUnReachable dfa = 
  let 
    r = reachable dfa
    newFinal = intersect (finalStates dfa) r
    newTable = filter (\x -> fst x `elem` r) (transitionTable dfa)
    newDFA = DFA r (alphabet dfa) (dfaStartState dfa) newFinal newTable
  in
    newDFA

findRealRepr :: [State] -> [[State]] -> [State]
findRealRepr st states = fromMaybe [] (find (\x -> null (x \\ st)) states )

subsetConstructionTable :: (FA f, Eq sy) => f sy -> [(State, [(sy, [State])])]
subsetConstructionTable nfa = do
  let sts = filter (/= []) $ powerset $ states nfa
  s <- sts
  let first = concat s
  let second = do
        sym <- alphabet nfa
        let stateList = do
              p <- s
              return $ getStates (transitionTable nfa) p sym
        return (sym, findRealRepr (foldl union [] stateList) sts)
  return (first, second)

subsetConstruction :: Ord symbol => NFA symbol -> DFA symbol
subsetConstruction nfa =
  let
    newFinalStates =  [ concat s | s <- powerset $ states nfa, intersect s (finalStates nfa) /= [] ]
    newTable = subsetConstructionTable nfa
    newStates = keys newTable
    newStartState = concat $ findRealRepr (nfaStartStates nfa) (map (map (:[])) newStates)
    
  in
   DFA newStates (alphabet nfa) newStartState newFinalStates newTable




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
    findAllMarked as dist = concatMap f as
        where f a = [ (a,b) | b <- states dfa , x <- alphabet dfa ,
                              isDist (transitionTable dfa) (a,b) x dist ]

isfindUniqueDist :: Eq a => [[a]] -> [[a]]
isfindUniqueDist [] = []
isfindUniqueDist xs = 
  [x | let y = tail xs, x <- xs, null $ concatMap (x `intersect`) y] ++ isfindUniqueDist (tail xs)

indistinguishable :: Eq symbol => DFA symbol -> [(State,State)] -> [[State]]
indistinguishable dfa dist =
        isfindUniqueDist 
        [ i : [j | j <- states dfa , (i,j) `notElem` dist, (j,i) `notElem` dist, j/= i ] 
        | i <- states dfa ]

minimizeDfaTable :: (Eq sy, FA f) => [[State]] -> f sy -> [(State, [(sy, [State])])]
minimizeDfaTable und dfa = do
    ust <- und
    let fstElem = concat ust
    let sndElem = do
          sy <- alphabet dfa
          let innerElem = do
                st <- ust
                let g = getStates (transitionTable dfa) st sy
                let filtered = filter (elem (concat g)) und
                return $ concat $ concat filtered
          return (sy, nub innerElem)
    return (fstElem, sndElem)

minimizeDFA :: Eq symbol => DFA symbol -> DFA symbol
minimizeDFA dfa = 
  let 
    dist = distinguishable dfa
    und = indistinguishable dfa dist
    table = minimizeDfaTable und dfa
    fstates = nub [concat $ concat $ filter (elem f) und | f <- finalStates dfa]
    sstate = concat $ concat $ filter (elem (dfaStartState dfa)) und
  in
    DFA (map concat und) (alphabet dfa) sstate fstates table
