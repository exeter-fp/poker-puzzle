module Tests where 
  
import Data.List
  
data Suit = Club | Spade | Heart | Diamond
  deriving (Show, Eq, Ord)
  
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Enum, Eq, Ord)

data Card = Card {value :: Value, suit :: Suit}
 deriving (Show, Eq, Ord)
 
test = groupBy (\card1 card2 -> value card1 == value card2) $ sort [Card Three Diamond, Card Four Spade, Card Three Heart]