-- https://projecteuler.net/problem=54

module Main where

import PokerPuzzle
import Parsing

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy as T

main = do
  filecontent <- T.readFile "p054_poker.txt"
  let hands = map parseLine $ T.lines filecontent
  let numPlayer1Wins = length $ filter isPlayer1Winner hands 
  let outputString = "Player 1 has won " ++ show numPlayer1Wins ++ " times"
  putStrLn outputString
  pure numPlayer1Wins  

