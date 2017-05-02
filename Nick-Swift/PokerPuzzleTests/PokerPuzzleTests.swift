//
//  PokerPuzzleTests.swift
//  PokerPuzzleTests
//
//  Created by Nick Ager on 29/04/2017.
//  Copyright © 2017 Rocketbox Ltd. All rights reserved.
//

import XCTest

class PokerPuzzleTests: XCTestCase {    
    func testExamples () {
        XCTAssertFalse(isPlayer1Winner(handTuple: parseLine(line: "5H 5C 6S 7S KD 2C 3S 8S 8D TD")))
        XCTAssertTrue(isPlayer1Winner(handTuple: parseLine(line: "5D 8C 9S JS AC 2C 5C 7D 8S QH")))
        XCTAssertFalse(isPlayer1Winner(handTuple: parseLine(line: "2D 9C AS AH AC 3D 6D 7D TD QD")))
        XCTAssertTrue(isPlayer1Winner(handTuple: parseLine(line: "4D 6S 9H QH QC 3D 6D 7H QD QS")))
        XCTAssertTrue(isPlayer1Winner(handTuple: parseLine(line: "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D")))
    }
}
