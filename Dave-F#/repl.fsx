(*
    Model:
    - Rank is a single case union wrapping an int
    - Suit is a union type
    - Card is a tuple of a rank and suit
    - Hand is a list of cards wrapped in a single case union
*)

type Rank = Rank of int
let Rank = function x when x >= 2 && x <= 14 -> Rank(x) | _ -> failwith "Invalid card rank"

[<NoComparison>]  //  prevent Suit from being compared, in poker no suit is better than another
type Suit = Clubs | Spades | Hearts | Diamonds

type Card = Rank * Suit
type Hand = Hand of Card List   // Wrap card list in single case sum, enforce rule through shadowing constructor
let Hand cards = match List.length cards with 5 -> Hand(cards) | _ -> failwith "Need 5 cards"
let map f (Hand cards) = List.map f cards  

// Define getter/setter for if we change the representations
let newCard r s : Card = r,s
let getRank = fst
let getSuit = snd

let rankFromInt = id
let Ace, King, Queen, Jack, Ten = Rank 14,Rank 13, Rank 12, Rank 11,Rank 10

let parseHand (txt:System.String) : Hand =
    let rankFromDigit (d:char) = (int d) - 48
    let suit = function
        | 'H' -> Hearts
        | 'C' -> Clubs
        | 'S' -> Spades
        | 'D' -> Diamonds
        |  x  -> failwithf "Unknown suit %c" x
    let rank = function
        | 'A' -> Ace
        | 'K' -> King
        | 'Q' -> Queen
        | 'J' -> Jack
        | 'T' -> Ten
        |  x when List.contains x ['2'..'9'] -> Rank(rankFromDigit x)
        |  _  -> failwith "Invalid rank"
    txt.Split ' ' |> Array.map (fun p -> newCard (rank p.[0]) (suit p.[1] )) |> List.ofArray |> Hand

// Ordering this way makes builtin comparisions work
type Score =
| HighCard of Rank
| OnePair of Rank
| TwoPair of Higher:Rank * Lower:Rank
| ThreeOfKind of Rank
| Straight of High:Rank
| Flush
| FullHouse of Three:Rank * Two:Rank
| FourOfKind of Rank
| StraightFlush of High:Rank
| RoyalFlush

// Code for recognising the different types of hand
module HandRecognision =

    /// Descending list of duplicates, value * count
    let groupByRank = 
        map getRank >> List.groupBy id 
        >> List.choose (fun (r,items) -> if items.Length > 1 then Some(items.Length,r) else None) 
        >> List.sortDescending

    let highestRank = map getRank >> List.max
    
    /// Match numbers of a kind, partially applied below
    let matchOfAKind num = groupByRank >> function
        | (count,rank)::_ when count = num  -> Some rank
        | _                                 -> None

    let (|Pair|_|) = matchOfAKind 2
    let (|Three|_|) = matchOfAKind 3
    let (|Four|_|) = matchOfAKind 4

    let matchTwoKinds first second = groupByRank >> function
        | (f,x)::(s,y)::_ when f = first && s = second  -> Some(x,y)
        | _                                             -> None

    // Note here we have to make sure we order the two pairs currently
    let (|TwoPairs|_|) =  matchTwoKinds 2 2 >> function None -> None | Some(x,y) -> Some(max x y, min x y)
    let (|FullHouse|_|) = matchTwoKinds 3 2

    let (|Flush|_|) = map getSuit >> List.distinct >> List.length >> function 1 -> Some () | _ -> None

    let (|Straight|_|) (hand:Hand) =
        let ordered = hand |> map getRank |> List.sort
        if ordered.Head >= Jack then None
        else 
            let (Rank first) = ordered.Head
            match List.forall2 (=) ordered [for i in 0..4 -> first + i |> Rank ] with
            | true  -> ordered |> List.last |> Some
            | false -> None // Consider: how to handle low aces, not required in this puzzle but poker allows ace = 1 for straights

open HandRecognision
let scoreHand (hand:Hand) =
    match hand with
    | Flush & Straight(high) when high = Ace      -> RoyalFlush
    | Flush & Straight(x)                         -> StraightFlush(x)
    | Four(x)                                     -> FourOfKind(x)
    | FullHouse(f,s)                              -> FullHouse(f,s)
    | Flush                                       -> Flush
    | Straight(x)                                 -> Straight(x)
    | Three(x)                                    -> ThreeOfKind(x)
    | TwoPairs(f,s)                               -> TwoPair(f,s) 
    | Pair(x)                                     -> OnePair(x)
    | _                                           -> HighCard(highestRank hand)

let doesFirstWin f s =
    let fs, ss = scoreHand f, scoreHand s
    if fs = ss then
        // If the score is the same, we choose the highest card
        let sorted = map getRank >> List.sortDescending
        let fSorted, sSorted = sorted f, sorted s
        if fSorted = sSorted then failwith "Hand is draw" else fSorted > sSorted
    else
        fs > ss

let doProblem54 () =
    // Load from file
    let lines = System.IO.File.ReadAllLines("../poker.txt")

    if lines.Length <> 1000 then failwith "Incorrect line count"
    let processLine (input:string) =
        let firstHand =   input.[..13]        |> parseHand
        let secondHand =  input.[14..].Trim() |> parseHand
        if doesFirstWin firstHand secondHand then 1 else 0
    lines |> Array.sumBy processLine


#time
match doProblem54() with
| 376 -> printfn "Problem passed"
|  x  -> printfn "Problem failed with score %i" x 
