module PokerPuzzle where
  
import Data.List (sort, groupBy, find)
import Data.Maybe (isJust, fromJust)
import Control.Monad (guard)
import Model

data PokerResult = 
    HighCard Card
  | OnePair (Card, Card) [Card]
  | TwoPairs (Card, Card) (Card, Card) Card
  | ThreeOfAKind (Card, Card, Card) [Card]
  | Straight (Card, Card, Card, Card, Card)
  | Flush (Card, Card, Card, Card, Card)
  | FullHouse (Card, Card, Card) (Card, Card)
  | FourOfAKind (Card, Card, Card, Card) Card
  | StraightFlush (Card, Card, Card, Card, Card)
  | RoyalFlush (Card, Card, Card, Card, Card)
  deriving (Show, Eq, Ord)

--

newtype GroupedHand = GroupedHand {groups :: [[Card]]}

--

allSameSuit :: Hand -> Bool
allSameSuit (Hand cards) = 
  let
    firstSuit = suit $ head cards
  in 
    all (\card -> suit card == firstSuit) cards
 
isStraight :: Hand -> Bool
isStraight (Hand cards)  = 
  let
    values = map value cards
    currentPreviousList = zip values $ tail values
    currentSuccessorPrevious (prev, current) = (succ prev) == current
  in 
    all currentSuccessorPrevious currentPreviousList 
   
--
 
cardsTuple2 [card1, card2] = (card1, card2)
cardsTuple3 [card1, card2, card3] = (card1, card2, card3) 
cardsTuple4 [card1, card2, card3, card4] = (card1, card2, card3, card4)
cardsTuple5 [card1, card2, card3, card4, card5] = (card1, card2, card3, card4, card5)

--
    
highCard :: Hand -> PokerResult
highCard hand = HighCard $ last $ cards hand 

onePair :: GroupedHand -> Maybe PokerResult
onePair (GroupedHand groups) = do
    pair <- find ((==2) . length) groups
    let remainingCards = reverse $ concat $ filter (/=pair) groups
    return $ OnePair (cardsTuple2 pair) remainingCards
    
twoPairs :: GroupedHand -> Maybe PokerResult
twoPairs (GroupedHand groups) = 
  let
    allTwos = filter ((==2) . length) groups
    pair1 = cardsTuple2 $ allTwos !! 0
    pair2 = cardsTuple2 $ allTwos !! 1
    otherCard = head . head $ filter ((==1) . length) groups
  in
    if (length allTwos == 2) then Just (TwoPairs pair1 pair2 otherCard) else Nothing
      
threeOfAKind :: GroupedHand -> Maybe PokerResult
threeOfAKind (GroupedHand groups) =
  let 
    triplet = cardsTuple3 <$> find ((==3) . length) groups
    remainingCards = reverse $ concat $ filter ((/=3) . length) groups
  in
    ThreeOfAKind <$> triplet <*> pure remainingCards  
      
straight :: Hand -> Maybe PokerResult
straight hand@(Hand cards) = 
  (Straight $ cardsTuple5 cards) <$ guard (isStraight hand)
    
flush :: Hand -> Maybe PokerResult
flush hand@(Hand cards) = (Flush $ cardsTuple5 cards) <$ guard (allSameSuit hand)
    
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
    otherCard = head . head $ filter ((==1) . length) groups
  in
    FourOfAKind <$> quadruplet <*> pure otherCard

straightFlush :: Hand -> Maybe PokerResult
straightFlush hand@(Hand cards) =
  let
     isStraightFlush = allSameSuit hand && isStraight hand
  in
    (StraightFlush $ cardsTuple5 cards) <$ guard isStraightFlush
    
royalFlush :: Hand -> Maybe PokerResult
royalFlush hand@(Hand cards) =
  let
    lowestCardValue = value $ head $ cards
    isRoyalFlush = allSameSuit hand && isStraight hand && lowestCardValue == Jack
  in
    (RoyalFlush $ cardsTuple5 cards) <$ guard isRoyalFlush
    
--

pokerResult :: Hand -> PokerResult
pokerResult (Hand cardsInHand) = 
  let
    sortedHand = Hand $ sort cardsInHand
    groupedHand = GroupedHand $ groupBy (\card1 card2 -> value card1 == value card2) $ cards sortedHand
    
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
    fromJust . fromJust $ best
    
    
isPlayer1Winner :: (Hand, Hand) -> Bool
isPlayer1Winner (player1Hand, player2Hand) =
  let
     player1Result = pokerResult player1Hand
     player2Result = pokerResult player2Hand
  in
    player1Result > player2Result








     
