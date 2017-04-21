(*
    Model:
    - Suit is a sum type
    - Rank is either a simple int, or an enumeration  (or maybe other versions if I have time)
    - Card is a tuple of a rank and suit
    - Hand is a list of cards
*)

// Choose 1 of the following to represent a rank in differnt forms
#load "rank_int.fs"        // Simple ints
//#load "rank_enum.fs"      // Enumeration
//#load "rank_sum.fs"       // Full DU

open RankModel              // From above

type Suit = Clubs | Spades | Hearts | Diamonds
type Card = Rank * Suit
type Hand = Card List

// Define getter/setter for if we change the representations
let newCard r s : Card = r,s
let getRank = fst
let getSuit = snd

let parseHand (txt:System.String) : Hand =
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
        |  x when List.contains x ['2'..'9'] -> rankFromDigit x
        |  _  -> failwith "Invalid rank"
    let r = txt.Split ' ' |> Array.map (fun p -> newCard (rank p.[0]) (suit p.[1] )) |> List.ofArray
    if r.Length <> 5 then failwithf "Didn't get 5 cards: %A" r
    r

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
    let groupByRank (hand:Hand) = 
        hand 
        |> List.map getRank |> List.groupBy id 
        |> List.choose (fun (r,items) -> if items.Length > 1 then Some(items.Length,r) else None) 
        |> List.sortDescending

    /// Highest rank in hand
    let highestRank hand = hand |> List.map getRank |> List.max
    
    /// Match numbers of a kind, partially applied below
    let matchOfAKind num hand =
        match hand |> groupByRank with 
        | (count,rank)::_ when count = num  -> Some rank
        | _                                 -> None

    let (|Pair|_|) = matchOfAKind 2
    let (|Three|_|) = matchOfAKind 3
    let (|Four|_|) = matchOfAKind 4

    let matchTwoKinds first second hand =
        match hand |> groupByRank with 
        | (f,x)::(s,y)::_ when f = first && s = second  -> Some(x,y)
        | _                                             -> None

    // Note here we have to make sure we order the two pairs currently
    let (|TwoPairs|_|) =  matchTwoKinds 2 2 >> function None -> None | Some(x,y) -> Some(max x y, min x y)
    let (|FullHouse|_|) = matchTwoKinds 3 2

    let (|Flush|_|) (hand:Hand) = 
        if hand |> List.map getSuit |> List.distinct |> List.length = 1 then Some () else None

    let (|Straight|_|) (hand:Hand) =
        let ordered = hand |> List.map getRank |> List.sort
        let first = ordered.Head
        if ordered |> List.forall2 (=) [for i in 0..4 -> rankFromInt ((int first) + i) ] then
            Some (ordered |> List.last )
        else None  // Consider: how to handle low aces, now required in puzzle but poker allows ace = 1 for straights

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
        let sorted x = x |> List.map getRank |> List.sortDescending
        let fSorted, sSorted = sorted f, sorted s
        if fSorted = sSorted then failwith "Hand is draw" else fSorted > sSorted
    else
        fs > ss

module Tests =
    let check beTrue = if not beTrue then failwith "Test fail"
    let basics() =
        parseHand "5H 5C 6S 7S KD" |> highestRank = King |> check
        RoyalFlush > Flush |> check
        StraightFlush King > StraightFlush Ten |> check

    let scoring () =
        let check (test,expected) =
            let score = test |> parseHand |> scoreHand in if score <> expected then printfn "Fail: expected %A got %A" expected score
        [
            "5D 8C 9S JS AC", HighCard(Ace)
            "2C 5C 7D 8S QH", HighCard(Queen)
            "2D 9C AS AH AC", ThreeOfKind(Ace)
            "3D 6D 7D TD QD", Flush
            "4D 6S 9H QH QC", OnePair(Queen)
            "3D 6D 7H QD QS", OnePair(Queen)
            "TH TD AC AD AS", FullHouse(Ace,Ten)
        ] |> List.iter check

    let scoreComparisons () =
        [
            "5H 5C 6S 7S KD" , "2C 3S 8S 8D TD" , false
            "5D 8C 9S JS AC" , "2C 5C 7D 8S QH" , true
            "2D 9C AS AH AC" , "3D 6D 7D TD QD" , false
            "4D 6S 9H QH QC" , "3D 6D 7H QD QS" , true
            "2H 2D 4C 4D 4S" , "3C 3D 3S 9S 9D" , true
        ] |> List.iter (fun (f, s, winner) -> if winner <> (doesFirstWin (parseHand f) (parseHand s)) then failwithf "Test failed for %A" f )

    let doProblem54 () =
       
        // let downloadFile url =
        //     use wc = new System.Net.WebClient() in wc.DownloadString(url:string)
        // let src = downloadFile "http://projecteuler.net/project/resources/p054_poker.txt"
        // let lines = src.Trim().Split '\n'
        
        // Load from file
        let lines = System.IO.File.ReadAllLines("../poker.txt")

        if lines.Length <> 1000 then failwith "Incorrect line count"
        let processLine (input:string) =
            let firstHand =   input.[..13]        |> parseHand
            let secondHand =  input.[14..].Trim() |> parseHand
            if doesFirstWin firstHand secondHand then 1 else 0
        lines |> Array.sumBy processLine

    let run() =
        basics()
        scoring()
        scoreComparisons()
        //doProblem54() = ??? |> check
        doProblem54() |> ignore
        printfn "All tests pass"

#time
Tests.run()
