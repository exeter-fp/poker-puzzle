module Lib where

-- Suits don't have an `Ord` instance, as there's no notion of Suit order
data Suit =
      Hearts
    | Clubs
    | Diamonds
    | Spades
    deriving (Eq, Show)

data Rank =
      One
    | Two
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
    deriving (Eq, Ord, Show)

data Card =
    Card Rank Suit
    deriving (Eq, Show)

-- Sadly we can't specify the size of the hand.  This isn't Idris... :-(
type Hand = [Card]

-- Various type aliases to make the use of `Rank` in `BestHand` clearer
type HighRank = Rank
type LowRank = Rank
type ThreeRank = Rank
type TwoRank = Rank


-- A 'Best Hand' consists of the best available cards and possibly one or more
-- 'Kickers' the remaining cards that decide in the event of a 'best cards'
-- tie.  The assumptions we make about the `BestCards` and `Kickers` types
-- allow us to use the default derived `Ord` instance to detect winning hands.

data BestHand = 
    HighCard Rank Kickers
  | OnePair Rank Kickers
  | TwoPairs HighRank LowRank Kickers
  | ThreeOfAKind Rank Kickers
  | Straight Rank
    -- TODO - Consider how we compare two flushes
  | Flush
  | FullHouse ThreeRank TwoRank
  | FourOfAKind Rank Kickers
  | StraightFlush Rank
  | RoyalFlush
  deriving (Eq, Ord, Show)

type Kickers = [Rank]


-- Takes a hand of 5 cards and creates the best hand from it
getBestHand :: Hand -> BestHand
getBestHand = undefined
