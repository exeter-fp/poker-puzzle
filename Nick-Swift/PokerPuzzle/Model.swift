import Foundation

enum Suit {
    case club
    case spade
    case heart
    case diamond
}

// MARK: -
enum Value: Int {
    case two
    case three
    case four
    case five
    case six
    case seven
    case eight
    case nine
    case ten
    case jack
    case queen
    case king
    case ace
}

// MARK: -
struct Card {
    let value: Value
    let suit: Suit
}


// MARK: -
extension Value : Comparable {
    static func < (lhs: Value, rhs: Value) -> Bool {
        return lhs.rawValue < rhs.rawValue
    }
}

// MARK: -
extension Card : Comparable {
    static func < (lhs: Card, rhs: Card) -> Bool {
        return lhs.value < rhs.value
    }
}

extension Card : Hashable {
    var hashValue: Int {
        return value.hashValue
    }
    
    static func == (lhs: Card, rhs: Card) -> Bool {
        return lhs.value == rhs.value
    }
}

// MARK: -
// no enforcement, just for documentation
typealias Hand = [Card]
typealias SortedHand = [Card]
typealias SortedCards = [Card]



