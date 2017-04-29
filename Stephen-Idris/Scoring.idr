module Scoring

import Data.Vect
import Types
import Sorting
 
%access public export

||| Lazy (not in the Haskell way) groupBy. 
||| Really I should make this output a dependant type that guarantees the 
||| number of grouped elements is the same as the elements in the input vector..
groupBy : Eq b => (a -> b) -> Vect n a -> List (b, List a)
groupBy f [] = []
groupBy f (x :: xs) = let idx = f x
                          grouped = groupBy f xs in
                      case List.find (findIt idx) grouped of
                            Nothing => (idx, [x]) :: grouped
                            (Just _) => map (mapIt x idx) grouped
where
    findIt : Eq b => b -> (b, List w) -> Bool
    findIt idx (x', _) = x' == idx

    mapIt : Eq b => (x : a) -> (idx : b) -> (b, List a) -> (b, List a)
    mapIt x idx (a, xs) = if idx == a 
                        then (a, x :: xs)
                        else (a, xs)


||| Group cards by their rank.
groupPairs : SortedCards 5 -> List (Rank, Integer)
groupPairs (cards ** pf) = reverse $ sortBy counts $ map getCounts $ groupBy getRank cards
  where
    counts : (Rank, Integer) -> (Rank, Integer) -> Ordering
    counts (_, count) (_, count') = compare count count'

    getCounts : (Rank, List Card) -> (Rank, Integer)
    getCounts (rank, cards) = (rank, cast {to=Integer} (length cards))


||| Get the pairs in the hand
getPairs : SortedCards 5 -> Maybe Score
getPairs hand = case groupPairs hand of
                    ((rank, 4) :: _) => Just $ FourofaKind rank
                    ((rank, 3) :: _) => Just $ ThreeofaKind rank
                    ((rank, 2) :: (rank', 2) :: _) => Just $ TwoPairs rank rank'
                    ((rank, 3) :: (rank', 2) :: _) => Just $ FullHouse rank rank'
                    ((rank, 2) :: _) => Just $ OnePair rank
                    _ => Nothing


isStraight : SortedCards 5 -> Bool
isStraight (cards ** pf) = checkStraight cards
   where
    checkStraight : (ranks : Vect n Card) -> Bool
    checkStraight [] = True
    checkStraight (x :: []) = True
    checkStraight ((MkCard rank _) :: card@(MkCard rank' _) :: xs)
        = if (S (cast {to=Nat} rank)) == (cast {to=Nat} rank')
          then checkStraight $ card :: xs
          else False

isFlush : SortedCards 5 -> Bool
isFlush (cards ** pf) = (length $ groupBy getSuit cards) == 1

highestCard : SortedCards 5 -> Rank
highestCard (cards ** pf) = getHeadRank cards
  where 
    getHeadRank : (Vect (S n) Card) -> Rank
    -- List is guaranteed to be non empty so I don't need to worry about handling the empty case.
    getHeadRank (x :: xs) = getRank x
  
||| Go through all the possibilities in order to see what the best hand is.
getScore : SortedCards 5 -> Score
getScore hand with ( isFlush hand
                   , isStraight hand 
                   , getPairs hand
                   , highestCard hand
                   )
  getScore hand | (True, True, _, Ace) = RoyalFlush
  getScore hand | (True, True, _, _) = StraightFlush
  getScore hand | (_, _, Just $ FourofaKind rank, _) = FourofaKind rank
  getScore hand | (_, _, Just $ FullHouse rank rank', _) = FullHouse rank rank'
  getScore hand | (True, False, _, _) = Flush
  getScore hand | (False, True, _, _) = Straight
  getScore hand | (_, _, Just $ ThreeofaKind rank, _) = ThreeofaKind rank
  getScore hand | (_, _, Just $ TwoPairs rank rank', _) = TwoPairs rank rank'
  getScore hand | (_, _, Just $ OnePair rank, _) = OnePair rank
  getScore hand | (_, _, _, _) = HighCard

||| Go through and compare each card in turn
compareHands : SortedCards n -> SortedCards n -> (Status, Status)
compareHands (x ** pf) (x' ** pf') = compareHands' (reverse x) (reverse x')
  where
    compareHands' : (x : Vect n Card) -> (x' : Vect n Card) -> (Status, Status)
    compareHands' [] [] = (Loser, Loser) -- In a tie everyone loses
    compareHands' (card1 :: cards1) (card2 :: cards2) = case compare card1 card2 of
                                                            LT => (Loser, Winner)
                                                            EQ => compareHands' cards1 cards2
                                                            GT => (Winner, Loser)

||| Which hand is the winner
getWinner : (SortedCards 5, SortedCards 5) -> (Status, Status)
getWinner (cards1, cards2) = case compare (getScore cards1) (getScore cards2) of
                                    LT => (Loser, Winner)
                                    GT => (Winner, Loser)
                                    EQ => compareHands cards1 cards2
