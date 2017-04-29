module Types
 
import Data.Vect
import Sorting
  
%access public export

data Suit = Hearts | Clubs | Diamonds | Spades

data Rank = Two 
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine 
          | Ten
          | Jack
          | Queen
          | King
          | Ace
             
||| Let us cast Ranks to numbers so it is easier to compare / sort.
||| Idris does not have a deriving thing..
Cast Rank Nat where
    cast Two = 2
    cast Three = 3
    cast Four = 4
    cast Five = 5
    cast Six = 6
    cast Seven = 7
    cast Eight = 8
    cast Nine = 9
    cast Ten = 10
    cast Jack = 11
    cast Queen = 12
    cast King = 13
    cast Ace = 14
 
Eq Rank where
  (==) rank rank' = (the Nat (cast rank)) == (the Nat (cast rank'))

Ord Rank where
  compare rank rank' = compare (the Nat (cast rank)) (the Nat (cast rank'))
  
||| Parse Rank   
Cast Char (Maybe Rank) where  
    cast '2' = Just Two
    cast '3' = Just Three
    cast '4' = Just Four
    cast '5' = Just Five
    cast '6' = Just Six
    cast '7' = Just Seven
    cast '8' = Just Eight
    cast '9' = Just Nine
    cast 'T' = Just Ten
    cast 'J' = Just Jack
    cast 'Q' = Just Queen
    cast 'K' = Just King
    cast 'A' = Just Ace
    cast _ = Nothing
    
||| Parse Suit
Cast Char (Maybe Suit) where
    cast 'H' = Just Hearts
    cast 'D' = Just Diamonds
    cast 'C' = Just Clubs
    cast 'S' = Just Spades
    cast _ = Nothing

Eq Suit where 
  (==) Hearts Hearts = True
  (==) Clubs Clubs = True
  (==) Diamonds Diamonds = True
  (==) Spades Spades = True
  (==) _ _ = False
  

data Card = MkCard Rank Suit

Eq Card where
  (==) (MkCard rank suit) (MkCard rank' suit') = (the Nat (cast rank)) == (the Nat (cast rank'))

Ord Card where
  compare (MkCard rank suit) (MkCard rank' suit') = compare (the Nat (cast rank)) (the Nat (cast rank'))
    
getSuit: (card: Card) -> Suit
getSuit (MkCard _ suit) = suit

getRank: (card: Card) -> Rank
getRank (MkCard rank _) = rank

data Score = HighCard
           | OnePair Rank
           | TwoPairs Rank Rank
           | ThreeofaKind Rank
           | Straight 
           | Flush 
           | FullHouse Rank Rank
           | FourofaKind Rank
           | StraightFlush 
           | RoyalFlush 
  
Cast Score Nat where
    cast HighCard = 1
    cast (OnePair _) = 2
    cast (TwoPairs _ _) = 3
    cast (ThreeofaKind _) = 4
    cast Straight = 5
    cast Flush = 6
    cast (FullHouse _ _) = 7
    cast (FourofaKind _) = 8
    cast StraightFlush = 9
    cast RoyalFlush = 10
  
Eq Score where
  (==) score score' = cast {to=Nat} score == cast {to=Nat} score'

Ord Score where
  compare (OnePair rank) (OnePair rank') = compare rank rank'
  compare (TwoPairs rank rank2) (TwoPairs rank' rank2') = case compare rank rank' of
                                                              EQ => compare rank2 rank2'
                                                              a => a
  compare (ThreeofaKind rank) (ThreeofaKind rank') = compare rank rank'
  compare (FullHouse rank rank2) (FullHouse rank' rank2') = case compare rank rank' of
                                                                EQ => compare rank2 rank2'
                                                                a => a
  compare (FourofaKind rank) (FourofaKind rank') = compare rank rank'
  compare score score' = compare (cast {to=Nat} score) (cast {to=Nat} score')
 

||| A list of cards together with a proof that they are sorted.
SortedCards : Nat -> Type
SortedCards n = (cards: (Vect n Card) ** IsSorted cards)

data Status = Winner | Loser

