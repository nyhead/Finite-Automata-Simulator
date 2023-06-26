import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Ord
import Automata

trim :: Eq a => [a] -> [a] -> [a]
trim chs = dropWhileEnd (`elem` chs) . dropWhile (`elem` chs)

parse :: String -> (State, State, [(State, [(State, [State])])], [String])
parse str
    | '|' `elem` str = let
          [s,t] = splitOn "|" (trim " \r" str)
          parseTransition text = map (splitOn " ". trim " \r") (splitOn "," text)
          
          state = trim " \r" (s \\ ">*")
          startState = if '>' `elem` s then state else ""
          finalState = if '*' `elem` s then state else ""
          transitions = [(head list, tail list) | list <- parseTransition $ trim " \r" t]
        in
          (startState, finalState, [(state,transitions)], [])
    | otherwise = ("", "", [], words str)  -- assuming this is a word line


finalOut :: [(State, State, [(State, [(State, [State])])], [State])]
         -> (DFA State, [State])
finalOut parsed = 
    let 
      (startStates, finalStates, transitions, w) = unzip4 parsed
      table = concat transitions
      words = concat w
      parsedStates = keys table
      symbols = maximumBy (comparing length) (map (keys . snd) table)
      deterministic = subsetConstruction  (NFA parsedStates symbols startStates finalStates table)
    in 
      (deterministic, words)

doHandler :: Handle -> [String] -> IO ()
doHandler h givenWords = do
  s <- hGetContents h
  let
    (dfa, parsedWords) = finalOut $ map parse (lines s) --parse nfa, convert to dfa
    mdfa = minimizeDFA dfa

    wordsToCheck = if null givenWords then parsedWords else givenWords 
    str word = "is " ++ word ++ " accepted: " ++ show ( isAccepted mdfa word)
    strAcc = map str wordsToCheck -- check input words

    d = unlines strAcc ++ show mdfa
  putStrLn d

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "runghc <file_name> <words, if not present in the input file>" --change
    file:iwords -> do 
      h <- openFile file ReadMode
      doHandler h iwords
