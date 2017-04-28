# Swift solution for Project Euler Poker puzzle

This is a direct Swift port of my [Haskell solution](https://github.com/exeter-fp/poker-puzzle/tree/master/Nick-Haskell)

The key files are:

* [`Model.swift`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Swift/PokerPuzzle/Model.swift)
* [`Parsing.swift`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Swift/PokerPuzzle/Parsing.swift)
* [`PokerPuzzle.swift`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Swift/PokerPuzzle/PokerPuzzle.swift) - this is where the bulk of the code lives
* [`main.swift`](https://github.com/exeter-fp/poker-puzzle/blob/master/Nick-Swift/PokerPuzzle/main.swift)

Build using [Xcode](http://appstore.com/mac/Xcode) - free download from a Mac. 

## Comparison with Haskell solution

* Once I'd written the Haskell solution it just worked&trade; first time; however the Swift version required debugging - surprising as the Haskell version came first.
* The bugs I introduced into the Swift version were in the boilerplate or the missing standard library code (`group`) that I had to write 😟
* The time to write both versions was similar, if I deduct the time it took me to work out the skeleton of the solution. This surprised me as I'm far more fluent in Swift than Haskell. However I was using areas of Swift that I haven't used before eg overloading `Comparable`, `Equatable` and `Hashable` and it took time to write all the additional boilerplate required in Swift.
* While porting the solution to Swift, I noticed a number of small improvement and simplifications I could make to the Haskell version. I guess I [paired](https://en.wikipedia.org/wiki/Pair_programming) with myself!
* Swift's syntatic support for `Optional` (equivalent to Haskell's `Maybe`) - `guard let` and `if let` - meant I didn't miss Haskell's higher kinded types; `Functor`, `Applicative` and `Monad`
* With the exception of the extra boiler-plate required, the Haskell and Swift solutions are remarkably similar.
* The extra boiler-plate required for the Swift solution, means a more compact (idiomatic?) Swift solution most likely wouldn't have used anything similar to the `PokerResult` data-type.
* Comparing IDEs; for simple problems like this, [Haskell for Mac](http://haskellformac.com) worked really well. I was immediately able to see the results of my changes and experiment with code. Xcode's playgrounds offers similar features however I found that Xcode's playground would often fail without properly reporting the error. I also found that Xcode (8.3.2) crashed more frequently than [Haskell for Mac](http://haskellformac.com).
* Despite superior code completion and documentation integration in Xcode, I really missed [Hoogle](https://www.haskell.org/hoogle) when searching Swift's standard library. 
* [Haskell for Mac](http://haskellformac.com) code completion and lookup has a long way to go before it reaches what is expected from a modern IDE.
* Swift's `switch .. case` has surprisingly powerful pattern matching capabilities. I think this is under-appreciated by the Swift community.

## Extra boilerplate required by Swift

### Implementing `Ord` & `Eq` on `PokerResult`

In haskell I could just add `deriving (Show, Eq, Ord)` to the bottom of `PokerResult` and the results comparision was taken care of. Not so in Swift. Here is the extra boilerplate required:

```swift
extension PokerResult {
    var rankValue: Int {
        let value:Int
        
        switch (self) {
        case .highCard:
            value = 0
        case .onePair:
            value = 1
        case .twoPairs:
            value = 2
        case .threeOfAKind:
            value = 3
        case .straight:
            value = 4
        case .flush:
            value = 5
        case .fullHouse:
            value = 6
        case .fourOfAKind:
            value = 7
        case .straightFlush:
            value = 8
        case .royalFlush:
            value = 9
        }
        return value
    }
}

// MARK: -
extension PokerResult : Equatable {
    static func == (lhs: PokerResult, rhs: PokerResult) -> Bool {
        switch (lhs, rhs) {
        case (let .highCard(card1, kickers1), let .highCard(card2, kickers2)):
            return card1 == card2 && kickers1 == kickers2
            
        case (let .onePair(cards1, kickers1), let .onePair(cards2, kickers2)):
            return cards1 == cards2 && kickers1 == kickers2
            
        case (let .twoPairs(cards1a, cards1b, kickers1), let .twoPairs(cards2a, cards2b, kickers2)):
            return cards1a == cards2a && cards1b == cards2b && kickers1 == kickers2
            
        case (let .threeOfAKind(cards1, kickers1), let .threeOfAKind(cards2, kickers2)):
            return cards1 == cards2 && kickers1 == kickers2
            
        case (let .straight(cards1), let .straight(cards2)):
            return cards1 == cards2
            
        case (let .flush(cards1), let .flush(cards2)):
            return cards1 == cards2
            
        case (let .fullHouse(cards1a, cards1b), let .fullHouse(cards2a, cards2b)):
            return cards1a == cards2a && cards1b == cards2b
            
        case (let .fourOfAKind(cards1, kickers1), let .fourOfAKind(cards2, kickers2)):
            return cards1 == cards2 && kickers1 == kickers2
            
        case (let .straightFlush(cards1), let .straightFlush(cards2)):
            return cards1 == cards2
            
        case (let .royalFlush(cards1), let .royalFlush(cards2)):
            return cards1 == cards2
            
        default:
            return false
        }
    }
}

func < (lhs: Array<Card>, rhs: Array<Card>) -> Bool {
    for i in 0...(lhs.count - 1) {
        if lhs[i] != rhs[i] {
            return lhs[i] < rhs[i]
        }
    }
    return false
}


extension PokerResult : Comparable {
    static func < (lhs: PokerResult, rhs: PokerResult) -> Bool {
        switch (lhs, rhs) {
        case (let .highCard(card1, kickers1), let .highCard(card2, kickers2)):
            if card1 != card2 {
                return card1 < card2
            }
            return kickers1 < kickers2
            
        case (let .onePair(cards1, kickers1), let .onePair(cards2, kickers2)):
            if cards1 != cards2 {
                return cards1 < cards2
            }
            return kickers1 < kickers2
            
        case (let .twoPairs(cards1a, cards1b, kickers1), let .twoPairs(cards2a, cards2b, kickers2)):
            if cards1a != cards2a {
                return cards1a < cards2a
            } else if cards1b != cards2b {
                return cards1b < cards2b
            }
            return kickers1 < kickers2
            
        case (let .threeOfAKind(cards1, kickers1), let .threeOfAKind(cards2, kickers2)):
            if cards1 != cards2 {
                return cards1 < cards2
            }
            return kickers1 < kickers2
            
        case (let .straight(cards1), let .straight(cards2)):
            return cards1 < cards2
            
        case (let .flush(cards1), let .flush(cards2)):
            return cards1 < cards2
            
        case (let .fullHouse(cards1a, cards1b), let .fullHouse(cards2a, cards2b)):
            if cards1a != cards2a {
                return cards1a < cards2a
            }
            return cards1b < cards2b
            
        case (let .fourOfAKind(cards1, kickers1), let .fourOfAKind(cards2, kickers2)):
            if cards1 != cards2 {
                return cards1 < cards2
            }
            return kickers1 < kickers2
            
        case (let .straightFlush(cards1), let .straightFlush(cards2)):
            return cards1 < cards2
            
        case (let .royalFlush(cards1), let .royalFlush(cards2)):
            return cards1 < cards2
            
        default:
            return lhs.rankValue < rhs.rankValue
        }
    }
}
```

That is 137 lines of Swift to cover a single line in Haskell -  `deriving (Show, Eq, Ord)`

###  No `Enum`  typeclass equivalent in Swift

In haskell I can use the `Enum` typeclasses `pred` to move to a previous enumeration

```haskell
isStraight :: SortedCards -> Bool
isStraight cards  = 
  let
    values = map value cards
    currentPreviousList = zip values $ tail values
    currentSuccessorPrevious (prev, current) = prev == pred current
  in 
    all currentSuccessorPrevious currentPreviousList 
```

However in swift I have to make the enum's raw representation an `Int` and then convert from enum into `rawValue`.

```swift
func isStraight(cards : SortedCards) -> Bool {
    let lowestCardValue = cards.last!.value.rawValue
    let highestCardValue = cards.first!.value.rawValue
    
    for i in lowestCardValue...highestCardValue {
        if cards[i - lowestCardValue].value.rawValue != i {
            return false
        }
    }
    return true
}
```
###  No Swift version of `group`

I wrote my own horribly imperative version of group, as I couldn't find one in the stardard library:

```swift
extension Array where Element == Card {
    func group() -> [[Card]] {
        var current = [Card]()
        var result = [[Card]]()
        for i in self {
            if current.isEmpty || current.first! == i {
                current.append(i)
            } else {
                result.append(current)
                current = [i]
            }
        }
        result.append(current)
        return result
    }
}
```
... which I managed to get wrong twice.

Note: I could have written the above code as a generic extension to `Array` for any `Element`, but choose to experiment with new [Swift 3.1 features](https://medium.com/@NilStack/swift-world-new-in-swift-3-1-concrete-constrained-extensions-fca7f51b8713)

### Extra array construction

See the extra constuction of arrays in the `return` statement of the following:

```swift
func parseLine(line: String) -> (Hand, Hand) {
    let cardsText = line.components(separatedBy: " ")
    let cards = cardsText.map(textToCard)
    
    return (Array(cards.dropLast(5)), Array(cards.dropFirst(5)))
}
```

Without the `Array` construction I saw the error: `error: cannot convert return expression of type 'ArraySlice<Card>' to return type 'Hand' (aka 'Array<Card>')`

Similarly dropping `Array` from the `return` on the following code resulted in the opaque: `error: cannot convert value of type 'FlattenBidirectionalCollection<[[Card]]>' (aka 'FlattenBidirectionalCollection<Array<Array<Card>>>') to expected argument type 'Kickers' (aka 'Array<Card>')`

```swift
func onePair(groups: GroupedHand) -> PokerResult? {
    guard let pair = groups.first(where: { $0.count == 2 }) else {
        return nil
    }
    
    let remainingCards = groups.filter { $0 != pair }
    return .onePair(cardsTuple2(array: pair), Array(remainingCards.joined()))
}
```
