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
          parseTransition text = map words (splitOn "," text)
          
          state = trim " \r" (s \\ ">*")
          startState = if '>' `elem` s then state else ""
          finalState = if '*' `elem` s then state else ""
          transitions = [(h,t) | (h:t) <- parseTransition $ trim " \r" t]
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
      deterministic = subsetConstruction (NFA parsedStates symbols  startStates finalStates table)
    in 
      (deterministic, words)

doHandler :: Handle -> String -> [String] -> IO ()
doHandler h minimize givenWords = do
  s <- hGetContents h
  let
    (d, parsedWords) = finalOut $ map parse (lines s) --parse nfa, convert to dfa
    
    dfa 
      | minimize == "-m" = minimizeDFA d
      | otherwise = d
    
    wordsToCheck
      | minimize /= "-m" = minimize:givenWords ++ parsedWords 
      | otherwise = givenWords ++ parsedWords 

    str word = "is " ++ word ++ " accepted: " ++ show ( isAccepted dfa word)
    strAcc = map str (filter (/= []) wordsToCheck) -- check input words

    out = unlines strAcc ++ show dfa
  putStrLn out

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "runghc main.hs <file_name> -m <words, if not present in the input file>"
    file:rest -> do 
      h <- openFile file ReadMode
      if rest /= [] then do
        let (minimize:iwords) = rest
        doHandler h minimize iwords
      else 
          doHandler h "" [""]
