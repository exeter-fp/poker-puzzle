(ns danclj.poker
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def suits
  #{:diamonds
    :hearts
    :spades
    :clubs})

(def rank-values
  {2 2
   3 3
   4 4
   5 5
   6 6
   7 7
   8 8
   9 9
   10 10
   :jack 11
   :queen 12
   :king 13
   :ace 14})

(def card-values
  (->> (for [suit suits
             [r v] rank-values]
         [{:rank r
           :suit suit} v])
       (into {})))

(defn highest-card
  [cards]
  (apply max-key card-values cards))

(defn- consecutive-elements?
  [coll]
  (and
    (<= 2 (count coll))
    (loop [a (first coll)
           coll (rest coll)]
      (if (seq coll)
        (let [b (first coll)]
          (if (= b (inc a))
            (recur b (rest coll))
            false))
        true))))

(defn straight?
  [hand]
  (consecutive-elements? (sort (map card-values hand))))

(defn flush?
  [hand]
  (= 1 (count (set (map :suit hand)))))

(defn score
  [hand]
  (let [by-value (group-by card-values hand)
        with-count (fn [n card-seqs]
                     (seq (keep #(when (= n (count %)) %) card-seqs)))
        pairs (with-count 2 (vals by-value))
        pair (first pairs)
        pair2 (second pairs)
        three (first (with-count 3 (vals by-value)))
        four (first (with-count 4 (vals by-value)))
        straight? (straight? hand)
        flush? (flush? hand)
        full-house? (and three pair)
        straight-flush? (and straight? flush?)
        royal-flush? (and flush? (= #{10 :jack :queen :king :ace} (set (map :rank hand))))
        comparable (fn [value & card-seqs]
                     [value (mapv (comp vec reverse sort (partial map card-values))
                                  card-seqs)])]
    (cond
      royal-flush?
      {:score      :royal-flush
       :comparable (comparable 10 hand)
       :cards      hand}
      straight-flush?
      {:score      :straight-flush
       :comparable (comparable 9 hand)
       :cards      hand}
      four
      {:score      :four-of-a-kind
       :comparable (comparable 8 hand)
       :cards hand}
      full-house?
      {:score      :full-house
       :comparable (comparable 7 three pair hand)
       :cards hand
       :three three
       :pair pair}
      flush?
      {:score :flush
       :comparable (comparable 6 hand)
       :cards hand}
      straight?
      {:score :straight
       :comparable (comparable 5 hand)
       :cards hand}
      three
      {:score :three-of-a-kind
       :comparable (comparable 4 three hand)
       :cards three}
      pair2
      {:score      :two-pairs
       :cards      (vec (concat pair pair2))
       :comparable (comparable 3 pair pair2 hand)}
      pair
      {:score :one-pair
       :cards pair
       :comparable (comparable 2 pair hand)}
      :else
      {:score :high-card
       :cards [(highest-card hand)]
       :comparable (comparable 1 hand)})))

(defn compare-hands
  [hand1 hand2]
  (let [score1 (score hand1)
        score2 (score hand2)]
    (compare (:comparable score1)
             (:comparable score2))))

(defn str->hand
  [s]
  (->> (str/split s #" ")
       (mapv
         (fn [c]
           (let [[rank suit] c]
             {:rank (case rank
                      \2 2
                      \3 3
                      \4 4
                      \5 5
                      \6 6
                      \7 7
                      \8 8
                      \9 9
                      \T 10
                      \J :jack
                      \Q :queen
                      \K :king
                      \A :ace)
              :suit (case suit
                      \C :clubs
                      \D :diamonds
                      \H :hearts
                      \S :spades)})))))

(defn solve-problem
  []
  (let [pline (fn [s]
                [(str->hand (subs s 0 14))
                 (str->hand (subs s 15))])]
    (with-open [reader (io/reader (io/resource "poker.txt"))]
      (reduce
        (fn [n line]
          (let [[h1 h2] (pline line)]
            (if (pos? (compare-hands h1 h2))
              (inc n)
              n)))
        0
        (line-seq reader)))))