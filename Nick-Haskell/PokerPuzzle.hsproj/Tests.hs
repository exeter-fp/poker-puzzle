{-# LANGUAGE OverloadedStrings #-}

module Tests where
  
import PokerPuzzle
import Parsing

import Test.Hspec
import Test.HUnit

spec = hspec $ do
  describe "Samples" $ do
     it "player 1 looses" $ do
       (isPlayer1Winner $ parseLine "5H 5C 6S 7S KD 2C 3S 8S 8D TD") `shouldBe` False
     it "player 1 wins" $ do     
        (isPlayer1Winner $ parseLine "5D 8C 9S JS AC 2C 5C 7D 8S QH") `shouldBe` True
     it "player 1 looses" $ do
       (isPlayer1Winner $ parseLine "2D 9C AS AH AC 3D 6D 7D TD QD") `shouldBe` False
     it "player 1 wins" $ do     
        (isPlayer1Winner $ parseLine "4D 6S 9H QH QC 3D 6D 7H QD QS") `shouldBe` True
     it "player 1 wins" $ do     
        (isPlayer1Winner $ parseLine "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D") `shouldBe` True
        
--

test1 = TestCase $ assertBool "player 1 looses" $ not $ isPlayer1Winner $ parseLine "5H 5C 6S 7S KD 2C 3S 8S 8D TD"
test2 = TestCase $ assertBool "player 1 wins" $ isPlayer1Winner $ parseLine "5D 8C 9S JS AC 2C 5C 7D 8S QH"
test3 = TestCase $ assertBool "player 1 looses" $ not $ isPlayer1Winner $ parseLine "2D 9C AS AH AC 3D 6D 7D TD QD"
test4 = TestCase $ assertBool "player 1 wins" $ isPlayer1Winner $ parseLine "4D 6S 9H QH QC 3D 6D 7H QD QS"
test5 = TestCase $ assertBool "player 1 wins" $ isPlayer1Winner $ parseLine "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"


tests = TestList [test1, test2, test3, test4, test5]