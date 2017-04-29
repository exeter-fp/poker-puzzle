module Main
  
import Data.Vect 

import Types
import Parsing
import Sorting
import Scoring
  
getScoreOfLine : String -> (Score, Score)
getScoreOfLine x = case parseDecks x of
                        (Just (a, b)) => (getScore a, getScore b)

loadPokerFile : (count : Nat) -> (file : File) -> IO (Nat)
loadPokerFile count file = do 
   do line <- fGetLine file 
      case line of
              (Left l) => do putStrLn $ "All gone wrong : " ++ (show l)
                             pure (0)
              (Right "") => pure count
              (Right str) => case parseDecks str of
                                  Nothing => do putStrLn $ "Failed to parse " ++ str
                                                pure 0
                                  (Just cards) => case getWinner cards of
                                                      (Winner, Loser) => loadPokerFile (count + 1) file
                                                      _               => loadPokerFile count file

main : IO ()
main = do
  f <- openFile "./poker.txt" Read
  case f of
      (Left l) => putStrLn $ "Failed to load file : " ++ (show l)
      (Right r) => do wins <- loadPokerFile 0 r
                      putStrLn $ "Wins " ++ show wins
                      closeFile r

