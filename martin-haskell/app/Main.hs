module Main where

import Data.Maybe (mapMaybe)
import Model
import Parsing

main :: IO ()
main = do
    input <- readFile "../poker.txt"
    let rounds = mapMaybe parseRound (lines input)
        winners = map winner rounds
        p1Wins = length (filter (== Player1) winners)
    putStrLn $ "Player 1 won " ++ show p1Wins ++ " times"
