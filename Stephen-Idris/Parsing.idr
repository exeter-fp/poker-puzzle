module Parsing

import Data.Vect

import Types
import Sorting
import Scoring

%access public export

parseCard : String -> Maybe Card
parseCard str = parse $ unpack str
  where
    parse : List Char -> Maybe Card
    parse [] = Nothing
    parse (rank :: suit :: []) = do
                                   rank <- (cast rank) 
                                   suit <- (cast suit)
                                   pure $ MkCard rank suit
    parse _ = Nothing
 
||| Unit tests using the type checker :
testParseCard : parseCard "AS" = (Just (MkCard Ace Spades))
testParseCard = Refl
  
testParseInvalidCard : parseCard "1S" = Nothing
testParseInvalidCard = Refl
  
||| Converts a list of maybes to maybe a list - or nothing if any of the maybes are nothing...
allOrNuffin : List (Maybe n) -> Maybe (List n)
allOrNuffin [] = Just List.Nil 
allOrNuffin (Nothing :: xs) = Nothing
allOrNuffin ((Just x) :: xs) = case allOrNuffin xs of
                                  Nothing => Nothing
                                  Just ys => Just (x :: ys)
                                  
||| Parsing
parseCards : String -> Maybe (List Card)
parseCards line = do cards <- allOrNuffin $ map parseCard $ words line
                     pure cards
 
vectToSortedCards : Vect n Card -> SortedCards n
vectToSortedCards [] = (Vect.Nil ** IsSortedZero)
vectToSortedCards (x :: xs) = addToSortedCards x (vectToSortedCards xs)
  where
    addToSortedCards : (card : Card) -> SortedCards n -> SortedCards (S n)
    addToSortedCards card (cards ** prf) = (addToSortedList card cards prf)

parseDecks : String -> Maybe (SortedCards 5, SortedCards 5)
parseDecks line = do cards <- parseCards line
                     let cards1 = fromList $ List.take 5 cards
                     let cards2 = fromList $ List.drop 5 cards
                     -- It won't compile if we don't validate that we have at least 5 cards each
                     cards1' <- exactLength 5 cards1
                     cards2' <- exactLength 5 cards2
                     pure (vectToSortedCards cards1', vectToSortedCards cards2')

