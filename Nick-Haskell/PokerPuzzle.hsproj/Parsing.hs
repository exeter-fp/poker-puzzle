{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import Data.Text.Lazy (Text, split, unpack)
import qualified Data.Text.Lazy as T (take, drop) 
import Model

parseLine :: Text -> (Hand, Hand)
parseLine line =
  let
    cardsText = split (==' ') line
    cards = map textToCard cardsText
  in
    (Hand $ take 5 cards, Hand $ drop 5 cards)
    

textToCard :: Text -> Card
textToCard cardText = 
  let
    valueText = T.take 1 cardText
    suitText = T.drop 1 cardText
  in
    Card (parseValue valueText) (parseSuit suitText)
    
parseValue :: Text -> Value
parseValue "T" = Ten
parseValue "A" = Ace
parseValue "K" = King
parseValue "Q" = Queen
parseValue "J" = Jack
parseValue valueText = toEnum $ (read $ unpack valueText) - 2 
    
parseSuit :: Text -> Suit
parseSuit "C" = Club
parseSuit "S" = Spade
parseSuit "H" = Heart
parseSuit "D" = Diamond
    