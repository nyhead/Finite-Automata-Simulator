import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Ord
import Automata

trim :: Eq a => [a] -> [a] -> [a]
trim chs = dropWhileEnd (`elem` chs) . dropWhile (`elem` chs)

parse :: String -> (String, String, [(String, [(String, [String])])], [String])
parse str
    | '|' `elem` str = let
          [s,t] = splitOn "|" (trim " \r" str)
          state = trim " \r" (s \\ ">*")
          startState = if '>' `elem` s then state else ""
          finalState = if '*' `elem` s then state else ""
          transitions = [(head list, tail list) | list <- parseTransition $ trim " \r" t]
          parseTransition text = map (splitOn " ". trim " \r") (splitOn "," text)
        in
          (startState, finalState, [(state,transitions)], [])
    | otherwise = ("", "", [], words str)  -- assuming this is a word line


finalOut :: [(String, String, [(String, [(String, [String])])], [String])]
         -> (DFA String, [String])
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
    [] -> print "" --change
    file:iwords -> do 
      h <- openFile file ReadMode
      doHandler h iwords
