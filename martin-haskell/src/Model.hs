module Model where

import           Control.Applicative ((<$>))
import           Control.Monad       (guard)
import           Data.List           (group, groupBy, sortBy)
import           Data.Maybe          (catMaybes)
import           Util
------------------------------------
-- Data model for Cards and Hands --
------------------------------------
-- Suits don't have an `Ord` instance, as there's no notion of Suit order
data Suit
    = Hearts
    | Clubs
    | Diamonds
    | Spades
    deriving (Eq, Show)

data Rank
    = Two
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
    deriving (Eq, Bounded, Enum, Ord, Show)

data Card =
    Card Rank
         Suit
    deriving (Eq, Show)

-- Card ordering doesn't take into account Suits, so we need to provide
-- a custom `Ord` instance
instance Ord Card where
    compare (Card r _) (Card r' _) = compare r r'

-- Accessor to extract the `Rank` from a `Card`
rank :: Card -> Rank
rank (Card r _) = r

-- Sadly we can't specify the size of the hand.  This isn't Idris... :-(
type Hand = [Card]

-----------------------------
-- Data model for BestHand --
-----------------------------
-- A 'Best Hand' consists of the best available cards and possibly one or more
-- 'Kickers' (the remaining cards that decide in the event of a 'best cards'
-- tie).  The assumptions we make about ordering in the `BestCards` and
-- `Kickers` types allow us to use the default derived `Ord` instance to detect
-- winning hands.
data BestHand
    = HighCard Rank
               Kickers
    | OnePair Rank
              Kickers
    | TwoPairs HighRank
               LowRank
               Kickers
    | ThreeOfAKind Rank
                   Kickers
    | Straight Rank
    | Flush
    | FullHouse ThreeRank
                TwoRank
    | FourOfAKind Rank
                  Kickers
    | StraightFlush Rank
    | RoyalFlush
    deriving (Eq, Ord, Show)

type Kickers = [Card]

-- Various type aliases to make the use of `Rank` in `BestHand` clearer
type HighRank = Rank

type LowRank = Rank

type ThreeRank = Rank

type TwoRank = Rank

-----------------------------------------------------------------------
-- Functions to determine the best possible hand from a given `Hand` --
-----------------------------------------------------------------------
-- Takes a hand and creates the best hand from it
bestHand :: Hand -> BestHand
bestHand h = maximum . catMaybes $ handMatchers <*> pure h

-- A list of 'hand matchers' - functions that can be used to 'match' hands.  By
-- applying this list of functions to a hand, we'll get a list of `Maybe
-- BestHand`, where each list entry is either `Nothing` if the hand doesn't
-- match, or a `Just` with the type of matching hand.
handMatchers :: [Hand -> Maybe BestHand]
handMatchers =
    [ highCardMatcher
    , onePairMatcher
    , twoPairsMatcher
    , threeOfAKindMatcher
    , straightMatcher
    , flushMatcher
    , fullHouseMatcher
    , fourOfAKindMatcher
    , straightFlushMatcher
    , royalFlushMatcher
    ]

-- Sorts a hand into descending order
descendingHand :: Hand -> Hand
descendingHand = sortBy (flip compare)

-- Removes all cards of rank `r` from a hand
withoutRank :: Rank -> Hand -> Hand
withoutRank r = filter (\c -> rank c /= r)

--------------------------------------------------------
-- Hand Matchers for the various possible `BestHand`s --
--------------------------------------------------------
highCardMatcher :: Hand -> Maybe BestHand
highCardMatcher h =
    let descHand = descendingHand h
        highestRank = rank . head $ descHand
    in Just $ HighCard highestRank (withoutRank highestRank descHand)

ranksAppearingExactlyNTimes :: Int -> [Rank] -> [Rank]
ranksAppearingExactlyNTimes n = map head . filter (\x -> length x == n) . group

-- Helper method to find 'n-of-a-kind' - extracts the common logic used for
-- matching `OnePair`, `ThreeOfAKind` and `FourOfAKind`.  `cons` is the data
-- constructor used for creating `BestHand`
nOfAKindMatcher :: Int
                -> (Rank -> Kickers -> BestHand)
                -> Hand
                -> Maybe BestHand
nOfAKindMatcher n cons h =
    cons highestRank kickers <$ guard (not (null groups))
  where
    descHand = descendingHand h
    groups = ranksAppearingExactlyNTimes n $ rank <$> descHand
    highestRank = head groups
    kickers = withoutRank highestRank descHand

onePairMatcher :: Hand -> Maybe BestHand
onePairMatcher = nOfAKindMatcher 2 OnePair

threeOfAKindMatcher :: Hand -> Maybe BestHand
threeOfAKindMatcher = nOfAKindMatcher 3 ThreeOfAKind

fourOfAKindMatcher :: Hand -> Maybe BestHand
fourOfAKindMatcher = nOfAKindMatcher 4 FourOfAKind

-- To match two pairs, we first match one pair, then attempt to match another
-- pair from the remaining cards
twoPairsMatcher :: Hand -> Maybe BestHand
twoPairsMatcher h = do
    OnePair h k <- onePairMatcher h
    OnePair l k' <- onePairMatcher k
    return $ TwoPairs h l k'

-- A `Straight` is a hand where each rank in descending order is a `pred`
-- of the previous one
straightMatcher :: Hand -> Maybe BestHand
straightMatcher h =
    Straight (head descendingRanks) <$ guard (allPredecessors descendingRanks)
  where
    descendingRanks = rank <$> descendingHand h

-- A `Flush`
flushMatcher :: Hand -> Maybe BestHand
flushMatcher h =
    Flush <$ guard (length (groupBy sameSuit h) == 1)
  where
    sameSuit (Card _ s) (Card _ s') = s == s'

-- A `FullHouse` is a `ThreeOfAKind` and a `OnePair`
fullHouseMatcher :: Hand -> Maybe BestHand
fullHouseMatcher h = do
    ThreeOfAKind t _ <- threeOfAKindMatcher h
    OnePair p _ <- onePairMatcher h
    return $ FullHouse t p

-- A `StraightFlush` is a `Flush` and a `Straight`
straightFlushMatcher :: Hand -> Maybe BestHand
straightFlushMatcher h = do
    flushMatcher h
    Straight r <- straightMatcher h
    return $ StraightFlush r

-- A `RoyalFlush` is a `StraightFlush` which ends in an `Ace`
royalFlushMatcher :: Hand -> Maybe BestHand
royalFlushMatcher h = do
    StraightFlush r <- straightFlushMatcher h
    RoyalFlush <$ guard (r == Ace)

---------------------------------------
-- Data Model for Rounds and Winners --
---------------------------------------
-- Here we just assume there are two players
data Round =
    Round Hand
          Hand
    deriving (Eq, Show)

data Winner
    = Player1
    | Player2
    | Draw
    deriving (Eq, Show)

winner :: Round -> Winner
winner (Round h1 h2) =
    case compare (bestHand h1) (bestHand h2) of
        GT -> Player1
        LT -> Player2
        EQ -> Draw
