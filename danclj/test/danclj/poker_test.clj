(ns danclj.poker-test
  (:require [clojure.test :refer :all]
            [danclj.poker :refer :all]))

(deftest test-examples
  (are [a s] (= (:score (score a)) s)
    "5H 5C 6S 7S KD" :one-pair
    "2C 3S 8S 8D TD" :one-pair
    "5D 8C 9S JS AC" :high-card
    "2C 5C 7D 8S QH" :high-card
    "2D 9C AS AH AC" :three-of-a-kind
    "3D 6D 7D TD QD" :flush
    "4D 6S 9H QH QC" :one-pair
    "3D 6D 7H QD QS" :one-pair
    "2H 2D 4C 4D 4S" :full-house
    "3C 3D 3S 9S 9D" :full-house)
  (are
    [a b w]
    (let [n (compare-hands a b)]
      (cond
        (pos? n)
        (= w :player-1)
        (neg? n)
        (= w :player-2)
        (zero? n)
        (= w :draw)))
    "5H 5C 6S 7S KD" "2C 3S 8S 8D TD" :player-2
    "5D 8C 9S JS AC" "2C 5C 7D 8S QH" :player-1
    "2D 9C AS AH AC" "3D 6D 7D TD QD" :player-2
    "4D 6S 9H QH QC" "3D 6D 7H QD QS" :player-1
    "2H 2D 4C 4D 4S" "3C 3D 3S 9S 9D" :player-1))

(deftest test-problem-appears-solved
  (is (= (solve-problem) 376)))