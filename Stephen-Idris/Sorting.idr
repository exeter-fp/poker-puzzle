module Sorting 

import Data.Vect
import Data.So
  
{-
  A provably sorted list. 

  Inspired by :
  http://dafoster.net/articles/2015/02/27/proof-terms-in-idris/
  https://github.com/davidfstr/idris-insertion-sort/blob/master/InsertionSort.idr#L36
  
  Should be pretty self explanatory...
-}

export
IsLte : Ord e => (x:e) -> (y:e) -> Type
IsLte x y = So (x <= y)
  
mkIsLte : Ord e => (x:e) -> (y:e) -> Maybe (IsLte x y)
mkIsLte x y =
    case (choose (x <= y)) of 
        Left proofXLteY =>
            Just proofXLteY
        Right proofNotXLteY =>
            Nothing 
 
||| Represents a proof that a Vect is a sorted vect
public export
data IsSorted : (xs: Vect n e) -> Type where
  IsSortedZero : IsSorted Nil
  IsSortedOne : Ord e => (x: e) -> IsSorted (x::Nil)
  IsSortedMany : Ord e => (x: e) -> (y:e) -> (ys: Vect n'' e) ->
                 (IsLte x y) -> IsSorted (y::ys) -> IsSorted(x::(y::ys))

export
mkIsSorted : Ord e => (xs:Vect n e) -> Maybe (IsSorted xs)
mkIsSorted Nil =
    Just IsSortedZero
mkIsSorted (x::Nil) =
    Just (IsSortedOne x)
mkIsSorted (x::(y::ys)) =
    case (mkIsLte x y) of
        Just proofXLteY =>
            case (mkIsSorted (y::ys)) of
                Just proofYYsIsSorted =>
                    Just (IsSortedMany x y ys proofXLteY proofYYsIsSorted)
                Nothing =>
                    Nothing
        Nothing =>
            Nothing


||| Given an `x` and a `y`, returns a proof that either `x <= y` or `y <= x`.
chooseLte :
    Ord e => 
    (x:e) -> (y:e) -> 
    Either (IsLte x y) (IsLte y x)
chooseLte x y =
    case choose (x <= y) of 
        Left proofXLteY => Left proofXLteY
        Right proofNotXLteY =>
            case choose (y <= x) of 
                Left proofYLteX => Right proofYLteX
                Right proofNotYLteX => believe_me "Impossible with a sane Ord implementation."


export
addToSortedList : Ord e => (x : e) -> (xs: Vect n e) -> (IsSorted xs) -> (ys:Vect (S n) e ** (IsSorted ys))
addToSortedList x [] IsSortedZero = ([x] ** IsSortedOne x)
addToSortedList x (y :: []) (IsSortedOne y) = 
  case (chooseLte x y) of
    (Left xLteYPrf) => ([x, y] ** IsSortedMany x y [] xLteYPrf (IsSortedOne y))
    (Right yLteXPrf) => ([y, x] ** IsSortedMany y x [] yLteXPrf (IsSortedOne x))

addToSortedList x (y :: (z :: ys)) prf@(IsSortedMany y z ys prfYLteZ prfZYs) = 
    -- Is X less than Y?
   case (chooseLte x y) of
         (Left xLteYPrf) => ((x :: y :: z :: ys) ** IsSortedMany x y (z :: ys) xLteYPrf prf)
         (Right yLteXPrf) => -- Y is Less than or equal to X, we need to recurse down..
            case (addToSortedList x (z :: ys) prfZYs) of
                   ((x' :: xs') ** prf) => case (chooseLte x' y) of
                                                (Left xLteYPrf) => -- Y is equal to X
                                                   case (chooseLte y x') of
                                                       (Left yLteXPrf) => 
                                                            ((y :: x' :: xs') ** IsSortedMany  y x' xs' yLteXPrf prf)
                                                       Right _ => believe_me "This is getting a bit silly now."
                                                (Right yLteXPrf) => 
                                                   ((y :: x' :: xs') ** IsSortedMany y x' xs' yLteXPrf prf)
                                                
||| Convert a Vector into a sorted Vector with the proof that it is sorted!
export
makeMeASortedList : Ord e => (Vect n e) -> (xs: Vect n e ** (IsSorted xs))
makeMeASortedList [] = ([] ** IsSortedZero)
makeMeASortedList (x :: xs) = case makeMeASortedList xs of
                                  (xs' ** pf) => addToSortedList x xs' pf
