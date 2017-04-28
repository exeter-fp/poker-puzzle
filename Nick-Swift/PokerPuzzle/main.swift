import Foundation

func printUsage() {
    let executableName = URL(string: CommandLine.arguments[0])!.lastPathComponent
    
    print("usage:")
    print("\(executableName) file")
}

if CommandLine.argc != 2 {
    printUsage()
    exit(EXIT_SUCCESS)
}
let pokerFilePath = CommandLine.arguments[1]

do {
    var fileContent = try String(contentsOfFile: pokerFilePath, encoding: String.Encoding.utf8)
    fileContent = fileContent.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines) // remove trailing new line
    let lines = fileContent.components(separatedBy: CharacterSet.newlines)
    let hands = lines.map(parseLine)
    
    let numPlayer1Wins = hands.filter(isPlayer1Winner).count
    print("Player 1 has won \(numPlayer1Wins) times")
} catch {
    print("Unable to read from file: '\(pokerFilePath)'. \nerror = \(error)")
}
