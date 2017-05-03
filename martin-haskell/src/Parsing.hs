module Parsing where

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Model


parseRound :: String -> Maybe Round
parseRound s = let cards = mapMaybe parseCard (splitOn " " s) in
                    case length cards of
                         10 -> Just $ Round (take 5 cards) (drop 5 cards)
                         _ -> Nothing

parseRank :: String -> Maybe Rank
parseRank s = case head s of
                   '2' -> Just Two
                   '3' -> Just Three
                   '4' -> Just Four
                   '5' -> Just Five
                   '6' -> Just Six
                   '7' -> Just Seven
                   '8' -> Just Eight
                   '9' -> Just Nine
                   'T' -> Just Ten
                   'J' -> Just Jack
                   'Q' -> Just Queen
                   'K' -> Just King
                   'A' -> Just Ace
                   _ -> Nothing

parseSuit :: String -> Maybe Suit
parseSuit s = case s !! 1 of
                   'H' -> Just Hearts
                   'C' -> Just Clubs
                   'D' -> Just Diamonds
                   'S' -> Just Spades
                   _ -> Nothing

parseCard :: String -> Maybe Card
parseCard s = case length s of
                   2 -> Card <$> parseRank s <*> parseSuit s
                   _ -> Nothing

