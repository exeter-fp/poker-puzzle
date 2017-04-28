import Foundation

func parseLine(line: String) -> (Hand, Hand) {
    let cardsText = line.components(separatedBy: " ")
    let cards = cardsText.map(textToCard)
    
    return (Array(cards.dropLast(5)), Array(cards.dropFirst(5)))
}

func textToCard(cardText:String) -> Card {
    let valueText = String(cardText.characters.prefix(1))
    let suitText = String(cardText.characters.suffix(1))
    
    return Card(value: parseValue(valueText), suit: parseSuit(suitText))
}

func parseValue(_ valueText:String) -> Value {
    switch valueText {
    case "T":
        return .ten
    case "A":
        return .ace
    case "K":
        return .king
    case "Q":
        return .queen
    case "J":
        return .jack
    default:
        return Value(rawValue: Int(valueText)! - 2)!
    }
}

func parseSuit(_ suitText:String) -> Suit {
    if suitText == "C" {
        return .club
    } else if suitText == "S" {
        return .spade
    } else if suitText == "H" {
        return .heart
    } else {
        return .diamond
    }
}
