import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Ord
import Automata

trim ch = dropWhileEnd (==ch) . dropWhile (==ch)

sel1 (x,_,_) = x
sel2 (_,y,_) = y
sel3 (_,_,z) = z

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

doHandler :: Handle -> String -> IO ()
doHandler h w = do
  s <- hGetContents h
  let m = minimizeDFA $ finalOut (lines s)
  let strAcc = [ "is " ++ word ++ " accepted: " ++ show ( isAccepted m word) | word <- words w]
  let d = unlines strAcc ++ show m
  putStrLn d


  
finalOut contents =  
    let 
      parsed = map parse contents
      table = concatMap sel3 parsed 
      startStates = map sel1 parsed
      finalStates = map sel2 parsed
      parsedStates = keys table
      symbols = maximumBy (comparing length) (map (keys . snd) table)
      deterministic = subsetConstruction  (NFA parsedStates symbols startStates finalStates table)
    in 
      deterministic

main :: IO ()
main = do
  args <- getArgs
  case args of
    [words] -> doHandler stdin words
    [file,words] -> do 
      h <- openFile file ReadMode
      doHandler h words 
