import Test.Hspec
import Lib


testHand1 = describe "Hand 1" $ do
                it "Pair of Eights beats Pair of Fives" $ do
                    (player1 > player2) `shouldBe` False
                    where player1 = OnePair Five  [King, Seven, Six]
                          player2 = OnePair Eight [Ten, Three, Two]

testHand2 = describe "Hand 2" $ do
                it "One Ace beats one Queen" $ do
                    (player1 > player2) `shouldBe` True
                    where player1 = HighCard Ace   [Jack, Nine, Eight, Five]
                          player2 = HighCard Queen [Eight, Seven, Five, Two]

testHand3 = describe "Hand 3" $ do
                it "Flush beats Three Aces" $ do
                    (player1 > player2) `shouldBe` False
                    where player1 = ThreeOfAKind Ace [Nine, Two]
                          player2 = Flush

testHand4 = describe "Hand 4" $ do
                it "Queens Nine High beats Queens Seven High" $ do
                    (player1 > player2) `shouldBe` True
                    where player1 = OnePair Queen [Nine, Six, Four]
                          player2 = OnePair Queen [Seven, Six, Three]

testHand5 = describe "Hand 5" $ do
                it "Full House (Fours & Twos) beats Full House (Threes & Nines)" $ do
                    (player1 > player2) `shouldBe` True
                    where player1 = FullHouse Four Two
                          player2 = FullHouse Three Nine

main :: IO ()
main = hspec $ do
        describe "Basic tests of default derived `Ord` instances" $ do
            testHand1
            testHand2
            testHand3
            testHand4
            testHand5

