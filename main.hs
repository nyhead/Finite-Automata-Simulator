import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Ord
import Automata
import Data.Maybe

trim :: Eq a => [a] -> [a] -> [a]
trim chs = dropWhileEnd (`elem` chs) . dropWhile (`elem` chs)

parse :: String -> (State, State, [(State, [(State, [State])])], [String])
parse str
    | '|' `elem` str = let
          [s,t] = splitOn "|" (trim " \r" str)
          parseTransition text = map words (splitOn "," text)
          
          state = trim " \r" (s \\ ">*")
          startState = if '>' `elem` s then state else ""
          finalState = if '*' `elem` s then state else ""
          transitions = [(h,t) | (h:t) <- parseTransition t]
        in
          (startState, finalState, [(state,transitions)], [])
    | otherwise = ("", "", [], words str)  -- assuming this is a word line


finalOut :: [(State, State, [(State, [(State, [State])])], [State])]
         -> (NFA State, [State])
finalOut parsed = 
    let 
      (startStates, finalStates, transitions, w) = unzip4 parsed
      table = concat transitions
      words = concat w
      parsedStates = keys table
      symbols = getAlphabet table
      fa = (NFA parsedStates symbols  startStates finalStates table)
    in 
      (fa, words)

processInput :: String -> String -> [String] -> String
processInput s minimize givenWords =
  let
  (nd, parsedWords) = finalOut $ map parse (lines s) --parse nfa, convert to dfa
  in
    if isJust $ find (\x -> length x > 1) (states nd) then
      "Each state must be named by a character, not a string."
    else 
    let
      d = subsetConstruction nd
      dfa 
        | minimize == "-m" = minimizeDFA d
        | otherwise = d
      
      wordsToCheck
        | minimize /= "-m" = minimize:givenWords ++ parsedWords 
        | otherwise = givenWords ++ parsedWords 

      str word = "is " ++ word ++ " accepted: " ++ show ( isAccepted dfa word)
      strAcc = map str (filter (/= []) wordsToCheck) -- check input words

      out = unlines strAcc ++ show dfa
    in  
      out
    

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "runghc main.hs <file_name> -m <words, if not present in the input file>"
    file:rest -> do 
      h <- openFile file ReadMode
      i <- hGetContents h
      if rest /= [] then do
        let (minimize:iwords) = rest
        putStrLn $ processInput i minimize iwords
      else do 
          putStrLn $ processInput i "" [""]
