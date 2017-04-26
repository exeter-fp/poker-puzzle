-- https://projecteuler.net/problem=54

module Main where

import PokerPuzzle
import Parsing

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

import Control.Parallel.Strategies

boolToInt :: Bool -> Int
boolToInt bool = if bool then 1 else 0

main = do
  filecontent <- T.readFile "p054_poker.txt"
  let hands = map parseLine $ T.lines filecontent
  let player1Wins = map (boolToInt. isPlayer1Winner) hands `using` parList rseq
  let numPlayer1Wins = sum player1Wins
  -- let numPlayer1Wins = length $ filter isPlayer1Winner hands
  let outputString = "Player 1 has won " ++ show numPlayer1Wins ++ " times"
  putStrLn outputString
  pure numPlayer1Wins  

