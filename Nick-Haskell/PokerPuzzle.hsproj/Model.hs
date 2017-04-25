module Model where
  
data Suit = Club | Spade | Heart | Diamond
  deriving (Show, Eq)
  
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq, Ord)

data Card = Card {value :: Value, suit :: Suit}
 deriving (Show)
 
-- ignore Suit so can't automatically derive
instance Ord Card where
  (Card value1 _) `compare` (Card value2 _) = value1 `compare` value2

-- ignore Suit so can't automatically derive 
instance Eq Card where
  (Card value1 _) == (Card value2 _) = value1 == value2
  
newtype Hand = Hand {cards :: [Card]}
  deriving Show