{-# LANGUAGE MonadComprehensions #-}

module PokerPuzzle where
  
import Data.List (sortBy, group, find, nub)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)
import Model

type Kickers = [Card]
type SortedHand = Hand
type SortedCards = [Card]

data PokerResult = 
    HighCard Card Kickers
  | OnePair (Card, Card) Kickers
  | TwoPairs (Card, Card) (Card, Card) Kickers
  | ThreeOfAKind (Card, Card, Card) Kickers
  | Straight (Card, Card, Card, Card, Card)
  | Flush (Card, Card, Card, Card, Card)
  | FullHouse (Card, Card, Card) (Card, Card)
  | FourOfAKind (Card, Card, Card, Card) Kickers
  | StraightFlush (Card, Card, Card, Card, Card)
  | RoyalFlush (Card, Card, Card, Card, Card)
  deriving (Show, Eq, Ord)

--

newtype GroupedHand = GroupedHand {groups :: [[Card]]}

--

allSameSuit :: [Card] -> Bool
allSameSuit cards = 
  let
    firstSuit = suit $ head cards
  in 
    all (\card -> suit card == firstSuit) cards
    
-- alternative implementation
allSameSuit2 :: [Card] -> Bool
allSameSuit2 cards = length (nub $ map suit cards) == 1 

 
isStraight :: SortedCards -> Bool
isStraight cards  = 
  let
    values = map value cards
    currentPreviousList = zip values $ tail values
    currentSuccessorPrevious (prev, current) = prev == pred current
  in 
    all currentSuccessorPrevious currentPreviousList 
   
--
 
cardsTuple2 [card1, card2] = (card1, card2)
cardsTuple3 [card1, card2, card3] = (card1, card2, card3) 
cardsTuple4 [card1, card2, card3, card4] = (card1, card2, card3, card4)
cardsTuple5 [card1, card2, card3, card4, card5] = (card1, card2, card3, card4, card5)

--
    
highCard :: SortedHand -> PokerResult
highCard (Hand cards) = HighCard (head cards) (tail cards) 

onePair :: GroupedHand -> Maybe PokerResult
onePair (GroupedHand groups) = 
  -- using monadic do, see threeOfAKind for applicative alternative
  do 
    pair <- find ((==2) . length) groups
    let remainingCards = concat $ filter (/=pair) groups
    return $ OnePair (cardsTuple2 pair) remainingCards
    
twoPairs :: GroupedHand -> Maybe PokerResult
twoPairs (GroupedHand groups) = 
  let
    allTwos = filter ((==2) . length) groups
    pair1 = cardsTuple2 $ allTwos !! 0
    pair2 = cardsTuple2 $ allTwos !! 1
    otherCard = head $ filter ((==1) . length) groups
  in
    -- if then else, formulation; alternative using `guard` below.  
    if (length allTwos == 2) then Just (TwoPairs pair1 pair2 otherCard) else Nothing
      
threeOfAKind :: GroupedHand -> Maybe PokerResult
threeOfAKind (GroupedHand groups) =
  let 
    triplet = cardsTuple3 <$> find ((==3) . length) groups
    remainingCards = concat $ filter ((/=3) . length) groups
  in
    ThreeOfAKind <$> triplet <*> pure remainingCards  
      
straight :: SortedHand -> Maybe PokerResult
straight (Hand cards) = 
  (Straight $ cardsTuple5 cards) <$ guard (isStraight cards)
    
flush :: Hand -> Maybe PokerResult
flush (Hand cards) = (Flush $ cardsTuple5 cards) <$ guard (allSameSuit cards)
    
fullHouse :: GroupedHand -> Maybe PokerResult
fullHouse (GroupedHand groups) = 
  let
    threeOfAKind = cardsTuple3 <$> find ((==3) . length) groups
    twoOfAKind = cardsTuple2 <$> find ((==2) . length) groups
  in
    FullHouse <$> threeOfAKind <*> twoOfAKind

fourOfAKind :: GroupedHand -> Maybe PokerResult
fourOfAKind (GroupedHand groups) = 
  let 
    quadruplet = cardsTuple4 <$> find ((==4) . length) groups
    otherCard = head $ filter ((==1) . length) groups
  in
    FourOfAKind <$> quadruplet <*> pure otherCard

straightFlush :: SortedHand -> Maybe PokerResult
straightFlush (Hand cards) =
  let
     isStraightFlush = allSameSuit cards && isStraight cards
  in
    (StraightFlush $ cardsTuple5 cards) <$ guard isStraightFlush
    
royalFlush :: SortedHand -> Maybe PokerResult
royalFlush (Hand cards) =
  let
    lowestCardValue = value $ head cards
    isRoyalFlush = allSameSuit cards && isStraight cards && lowestCardValue == Jack
  in
    -- monad comprehension version of `<$ guard` and if..then..else
    [RoyalFlush $ cardsTuple5 cards | isRoyalFlush]
    
--

pokerResult :: Hand -> PokerResult
pokerResult (Hand cardsInHand) = 
  let
    sortedCards = (sortBy (flip compare))  cardsInHand
    sortedHand = Hand sortedCards
    groupedHand = GroupedHand $ group sortedCards
    
    options = [
        royalFlush sortedHand
      , straightFlush sortedHand
      , fourOfAKind groupedHand
      , fullHouse groupedHand
      , flush sortedHand
      , straight sortedHand
      , threeOfAKind groupedHand
      , twoPairs groupedHand
      , onePair groupedHand
      , Just $ highCard sortedHand
      ]
      
    best = find isJust options
  in
    fromJust $ fromJust best
    
    
isPlayer1Winner :: (Hand, Hand) -> Bool
isPlayer1Winner (player1Hand, player2Hand) =
  let
     player1Result = pokerResult player1Hand
     player2Result = pokerResult player2Hand
  in
    player1Result > player2Result
