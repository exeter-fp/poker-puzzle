module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "../poker.txt"
    let winners = map (winner . parseRound) (lines input)
        p1Wins = length (filter (== Player1) winners) in
        putStrLn $ "Player 1 won " ++ show p1Wins ++ " times"
