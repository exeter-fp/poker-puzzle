module Util where

-- Predicate that tests whether each entry in a list is the immediate predecessor
-- of the previous entry in the list.
allPredecessors
    :: (Bounded a, Eq a, Enum a)
    => [a] -> Bool
allPredecessors [] = True
allPredecessors [x] = True
allPredecessors (x:y:ys)
    | x == minBound = False
    | otherwise = (y == pred x) && allPredecessors (y : ys)
