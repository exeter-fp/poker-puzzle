import Test.Hspec
import Model
import Parsing
import Data.Maybe (mapMaybe)

testHand1 = describe "Hand 1" $
                it "Pair of Eights beats Pair of Fives" $
                    (player1 > player2) `shouldBe` False
                    where player1 = OnePair Five  [Card King Diamonds, Card Seven Spades, Card Six Spades]
                          player2 = OnePair Eight [Card Ten Diamonds, Card Three Spades, Card Two Clubs]

testHand2 = describe "Hand 2" $
                it "One Ace beats one Queen" $
                    (player1 > player2) `shouldBe` True
                    where player1 = HighCard Ace   [Card Jack Spades, Card Nine Spades, Card Eight Clubs, Card Five Diamonds]
                          player2 = HighCard Queen [Card Eight Spades, Card Seven Diamonds, Card Five Clubs, Card Two Clubs]

testHand3 = describe "Hand 3" $
                it "Flush beats Three Aces" $
                    (player1 > player2) `shouldBe` False
                    where player1 = ThreeOfAKind Ace [Card Nine Clubs, Card Two Diamonds]
                          player2 = Flush

testHand4 = describe "Hand 4" $
                it "Queens Nine High beats Queens Seven High" $
                    (player1 > player2) `shouldBe` True
                    where player1 = OnePair Queen [Card Nine Hearts, Card Six Spades, Card Four Diamonds]
                          player2 = OnePair Queen [Card Seven Hearts, Card Six Diamonds, Card Three Diamonds]

testHand5 = describe "Hand 5" $
                it "Full House (Fours & Twos) beats Full House (Threes & Nines)" $
                    (player1 > player2) `shouldBe` True
                    where player1 = FullHouse Four Two
                          player2 = FullHouse Three Nine


testMatcherHand1 = describe "Hand 1" $ do
                it "Player 1 - 5H 5C 6S 7S KD => Pair of Fives" $
                    bestHand player1 `shouldBe` OnePair Five [Card King Diamonds, Card Seven Spades, Card Six Spades]
                it "Player 2 - 2C 3S 8S 8D TD => Pair of Eights" $
                    bestHand player2 `shouldBe` OnePair Eight [Card Ten Diamonds, Card Three Spades, Card Two Clubs]

                    where player1 = [Card Five Hearts, Card Five Clubs, Card Six Spades, Card Seven Spades, Card King Diamonds]
                          player2 = [Card Two Clubs, Card Three Spades, Card Eight Spades, Card Eight Diamonds, Card Ten Diamonds]


testMatcherHand2 = describe "Hand 2" $ do
                it "Player 1 - 5D 8C 9S JS AC => High Card Ace" $
                    bestHand player1 `shouldBe` HighCard Ace [Card Jack Spades, Card Nine Spades, Card Eight Clubs, Card Five Diamonds]
                it "Player 2 - 2C 5C 7D 8S QH => High Card Queen" $
                    bestHand player2 `shouldBe` HighCard Queen [Card Eight Spades, Card Seven Diamonds, Card Five Clubs, Card Two Clubs]

                    where player1 = [Card Five Diamonds, Card Eight Clubs, Card Nine Spades, Card Jack Spades, Card Ace Clubs]
                          player2 = [Card Two Clubs, Card Five Clubs, Card Seven Diamonds, Card Eight Spades, Card Queen Hearts]


testMatcherHand3 = describe "Hand 3" $ do
                it "Player 1 - 2D 9C AS AH AC => Three Aces" $
                    bestHand player1 `shouldBe` ThreeOfAKind Ace [Card Nine Clubs, Card Two Diamonds]
                it "Player 2 - 3D 6D 7D TD QD => Flush" $
                    bestHand player2 `shouldBe` Flush

                    where player1 = [Card Two Diamonds, Card Nine Clubs, Card Ace Spades, Card Ace Hearts, Card Ace Clubs]
                          player2 = [Card Three Diamonds, Card Six Diamonds, Card Seven Diamonds, Card Ten Diamonds, Card Queen Diamonds]

testMatcherHand4 = describe "Hand 4" $ do
                it "Player 1 - 4D 6S 9H QH QC => Queen Pair, Nine High" $
                    bestHand player1 `shouldBe` OnePair Queen [Card Nine Hearts, Card Six Spades, Card Four Diamonds]
                it "Player 2 - 3D 6D 7H QD QS => Queen Pair, Seven High" $
                    bestHand player2 `shouldBe` OnePair Queen [Card Seven Hearts, Card Six Diamonds, Card Three Diamonds]

                    where player1 = [Card Four Diamonds, Card Six Spades, Card Nine Hearts, Card Queen Hearts, Card Queen Clubs]
                          player2 = [Card Three Diamonds, Card Six Diamonds, Card Seven Hearts, Card Queen Diamonds, Card Queen Spades]

testMatcherHand5 = describe "Hand 5" $ do
                it "Player 1 - 2H 2D 4C 4D 4S => Full House, Fours and Twos" $
                    bestHand player1 `shouldBe` FullHouse Four Two
                it "Player 2 - 3C 3D 3S 9S 9D => Full House, Threes and Nines" $
                    bestHand player2 `shouldBe` FullHouse Three Nine

                    where player1 = [Card Two Hearts, Card Two Diamonds, Card Four Clubs, Card Four Diamonds, Card Four Spades]
                          player2 = [Card Three Clubs, Card Three Diamonds, Card Three Spades, Card Nine Spades, Card Nine Diamonds]


player1Wins :: IO Int
player1Wins = do
    input <- readFile "../poker.txt"
    let rounds = mapMaybe parseRound (lines input)
        winners = map winner rounds
        p1Wins = length (filter (== Player1) winners) in
        return p1Wins

testPlayer1Wins = describe "Correct puzzle answer" $
    it "Player 1 wins 376 hands using sample data" $ do
        p1w <- player1Wins
        p1w `shouldBe` 376

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

        testPlayer1Wins
