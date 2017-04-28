import Foundation

typealias Kickers = [Card]

enum PokerResult {
    case highCard(Card, Kickers)
    case onePair((Card, Card), Kickers)
    case twoPairs((Card, Card), (Card, Card), Kickers)
    case threeOfAKind((Card, Card, Card), Kickers)
    case straight((Card, Card, Card, Card, Card))
    case flush((Card, Card, Card, Card, Card))
    case fullHouse((Card, Card, Card), (Card, Card))
    case fourOfAKind((Card, Card, Card, Card), Kickers)
    case straightFlush((Card, Card, Card, Card, Card))
    case royalFlush((Card, Card, Card, Card, Card))
}

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

// MARK: -
typealias GroupedHand = [[Card]]

// MARK: -

func allSameSuit(cards: [Card]) -> Bool {
    let firstSuit = cards.first!.suit
    let notSame = cards.first { $0.suit != firstSuit }
    return notSame == nil
}

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

// MARK: -

func cardsTuple2(array:SortedCards) -> (Card, Card) {
    return (array[0], array[1])
}

func cardsTuple3(array:SortedCards) -> (Card, Card, Card) {
    return (array[0], array[1], array[2])
}

func cardsTuple4(array:SortedCards) -> (Card, Card, Card, Card) {
    return (array[0], array[1], array[2], array[3])
}

func cardsTuple5(array:SortedCards) -> (Card, Card, Card, Card, Card) {
    return (array[0], array[1], array[2], array[3], array[4])
}

// MARK: -

func highCard(cards: SortedHand) -> PokerResult {
    return .highCard(cards.first!, Array(cards.dropFirst(1)))
}

func onePair(groups: GroupedHand) -> PokerResult? {
    guard let pair = groups.first(where: { $0.count == 2 }) else {
        return nil
    }
    
    let remainingCards = groups.filter { $0 != pair }
    return .onePair(cardsTuple2(array: pair), Array(remainingCards.joined()))
}

func twoPairs(groups: GroupedHand) -> PokerResult? {
    let allTwos = groups.filter { $0.count == 2 }
    if allTwos.count != 2 {
        return nil
    }
    
    let pair1 = cardsTuple2(array: allTwos[0])
    let pair2 = cardsTuple2(array: allTwos[1])
    let otherCard = groups.filter { $0.count == 1 }.first!
    
    return .twoPairs(pair1, pair2, otherCard)
}

func threeOfAKind(groups: GroupedHand) -> PokerResult? {
    guard let triplet = groups.first(where: { $0.count == 3 }) else {
        return nil
    }
    
    let remainingCards = groups.filter { $0.count != 3}.joined()
    return .threeOfAKind(cardsTuple3(array:triplet), Array(remainingCards))
}

func straight(cards: SortedHand) -> PokerResult? {
    guard isStraight(cards: cards) else {
        return nil
    }
    
    return .straight(cardsTuple5(array: cards))
}

func flush(cards: Hand) -> PokerResult? {
    guard allSameSuit(cards: cards) else {
        return nil
    }
    
    return .flush(cardsTuple5(array: cards))
}

func fullHouse(groups: GroupedHand) -> PokerResult? {
    guard let threeOfAKind = groups.first(where: { $0.count == 3 }), let twoOfAKind = groups.first(where: { $0.count == 2 }) else {
        return nil
    }
    return .fullHouse(cardsTuple3(array: threeOfAKind), cardsTuple2(array: twoOfAKind))
}

func fourOfAKind(groups: GroupedHand) -> PokerResult? {
    guard let quadruplet = groups.first(where: { $0.count == 4 }) else {
        return nil
    }
    let remainingCards = groups.filter { $0.count == 1 }.first!
    return .fourOfAKind(cardsTuple4(array:quadruplet), remainingCards)
}

func straightFlush(cards: SortedHand) -> PokerResult? {
    guard allSameSuit(cards: cards) && isStraight(cards: cards) else {
        return nil
    }
    return .straightFlush(cardsTuple5(array: cards))
}

func royalFlush(cards: SortedHand) -> PokerResult? {
    let lowestCardValue = cards.last!.value
    guard allSameSuit(cards: cards) && isStraight(cards: cards) && lowestCardValue == .jack else {
        return nil
    }
    return .royalFlush(cardsTuple5(array: cards))
}

// MARK: -

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


func pokerResult(cards:Hand) -> PokerResult {
    let sortedCards = cards.sorted { $0 > $1 }
    let groupedCards = sortedCards.group()
    
    if let result = royalFlush(cards: sortedCards) {
        return result
    } else if let result = straightFlush(cards: sortedCards) {
        return result
    } else if let result = fourOfAKind(groups: groupedCards) {
        return result
    } else if let result = fullHouse(groups: groupedCards) {
        return result
    } else if let result = flush(cards: sortedCards) {
        return result
    } else if let result = straight(cards: sortedCards) {
        return result
    } else if let result = threeOfAKind(groups: groupedCards) {
        return result
    } else if let result = twoPairs(groups: groupedCards) {
        return result
    } else if let result = onePair(groups: groupedCards) {
        return result
    } else {
        return highCard(cards: sortedCards)
    }
}

func isPlayer1Winner(handTuple: (Hand, Hand)) -> Bool {
    let player1Result = pokerResult(cards: handTuple.0)
    let player2Result = pokerResult(cards: handTuple.1)
    
    return player1Result > player2Result
}

