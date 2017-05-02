module Parsing where

import Data.List.Split (splitOn)
import Model


parseRound :: String -> Round
parseRound s = let cards = map parseCard (splitOn " " s) in
                   Round (take 5 cards) (drop 5 cards)

-- TODO: Sort out error cases - at the moment, we assume all the
-- strings have valid values, so we don't get any 'non-exhaustive'
-- pattern errors.  Instead, we should make `parseRank :: String -> Maybe Rank`
-- and compose them using Applicatives.
parseRank :: String -> Rank
parseRank s = case head s of
                   '2' -> Two
                   '3' -> Three
                   '4' -> Four
                   '5' -> Five
                   '6' -> Six
                   '7' -> Seven
                   '8' -> Eight
                   '9' -> Nine
                   'T' -> Ten
                   'J' -> Jack
                   'Q' -> Queen
                   'K' -> King
                   'A' -> Ace

parseSuit :: String -> Suit
parseSuit s = case s !! 1 of
                   'H' -> Hearts
                   'C' -> Clubs
                   'D' -> Diamonds
                   'S' -> Spades

parseCard :: String -> Card
parseCard s = Card (parseRank s) (parseSuit s)
