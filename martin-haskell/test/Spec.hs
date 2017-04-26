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


testMatcherHand1 = describe "Hand 1" $ do
                it "Player 1 - 5H 5C 6S 7S KD => Pair of Fives" $ do
                    bestHand player1 `shouldBe` OnePair Five [King, Seven, Six]
                it "Player 2 - 2C 3S 8S 8D TD => Pair of Eights" $ do
                    bestHand player2 `shouldBe` OnePair Eight [Ten, Three, Two]

                    where player1 = [Card Five Hearts, Card Five Clubs, Card Six Spades, Card Seven Spades, Card King Diamonds]
                          player2 = [Card Two Clubs, Card Three Spades, Card Eight Spades, Card Eight Diamonds, Card Ten Diamonds]


testMatcherHand2 = describe "Hand 2" $ do
                it "Player 1 - 5D 8C 9S JS AC => High Card Ace" $ do
                    bestHand player1 `shouldBe` HighCard Ace [Jack, Nine, Eight, Five]
                it "Player 2 - 2C 5C 7D 8S QH => High Card Queen" $ do
                    bestHand player2 `shouldBe` HighCard Queen [Eight, Seven, Five, Two]

                    where player1 = [Card Five Diamonds, Card Eight Clubs, Card Nine Spades, Card Jack Spades, Card Ace Clubs]
                          player2 = [Card Two Clubs, Card Five Clubs, Card Seven Diamonds, Card Eight Spades, Card Queen Hearts]


testMatcherHand3 = describe "Hand 3" $ do
                it "Player 1 - 2D 9C AS AH AC => Three Aces" $ do
                    bestHand player1 `shouldBe` ThreeOfAKind Ace [Nine, Two]
                it "Player 2 - 3D 6D 7D TD QD => Flush" $ do
                    bestHand player2 `shouldBe` Flush

                    where player1 = [Card Two Diamonds, Card Nine Clubs, Card Ace Spades, Card Ace Hearts, Card Ace Clubs]
                          player2 = [Card Three Diamonds, Card Six Diamonds, Card Seven Diamonds, Card Ten Diamonds, Card Queen Diamonds]

testMatcherHand4 = describe "Hand 4" $ do
                it "Player 1 - 4D 6S 9H QH QC => Queen Pair, Nine High" $ do
                    bestHand player1 `shouldBe` OnePair Queen [Nine, Six, Four]
                it "Player 2 - 3D 6D 7H QD QS => Queen Pair, Seven High" $ do
                    bestHand player2 `shouldBe` OnePair Queen [Seven, Six, Three]

                    where player1 = [Card Four Diamonds, Card Six Spades, Card Nine Hearts, Card Queen Hearts, Card Queen Clubs]
                          player2 = [Card Three Diamonds, Card Six Diamonds, Card Seven Hearts, Card Queen Diamonds, Card Queen Spades]

testMatcherHand5 = describe "Hand 5" $ do
                it "Player 1 - 2H 2D 4C 4D 4S => Full House, Fours and Twos" $ do
                    bestHand player1 `shouldBe` FullHouse Four Two
                it "Player 2 - 3C 3D 3S 9S 9D => Full House, Threes and Nines" $ do
                    bestHand player2 `shouldBe` FullHouse Three Nine

                    where player1 = [Card Two Hearts, Card Two Diamonds, Card Four Clubs, Card Four Diamonds, Card Four Spades]
                          player2 = [Card Three Clubs, Card Three Diamonds, Card Three Spades, Card Nine Spades, Card Nine Diamonds]

main :: IO ()
main = hspec $ do
        describe "Basic tests of default derived `Ord` instances" $ do
            testHand1
            testHand2
            testHand3
            testHand4
            testHand5

        describe "Hand matcher tests" $ do
            testMatcherHand1
            testMatcherHand2
            testMatcherHand3
            testMatcherHand4
            testMatcherHand5
