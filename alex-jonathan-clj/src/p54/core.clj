(ns p54.core)

(defn score-hand [hand]
  (let [assoc-cnt (fn [v i cnt]
                    (let [cnt-i (.indexOf v cnt)]
                                  (if (pos? cnt-i)
                                    (-> (assoc v i cnt-i)
                                        (assoc cnt-i 0))
                                    v)))
        straight (fn [hand]
                   (->> (apply str (sort (clojure.string/replace hand #"[hcds ]" "")))
                        (.indexOf "23456789ABCDE")
                        inc))
        clear (fn [v to-clear]
                (reduce #(assoc %1 (int %2) 0) v to-clear))
        hand (reduce #(clojure.string/replace %1 (first %2) (second %2))
                     hand
                     ["Hh" "Dd" "Cc" "Ss" "JB" "QC" "KD" "AE" "TA"])]
    (-> (reduce #(update %1 (int %2) inc) (into [] (repeat 128 0)) hand)
        (assoc-cnt 124 5)
        (clear "hcds ")
        (assoc-cnt 120 2)
        (assoc-cnt 121 2)
        (assoc-cnt 122 3)
        (assoc 123 (straight hand))
        (#(assoc % 125 (pos? (* (nth % 120) (nth % 122)))))
        (assoc-cnt 126 4)
        (#(assoc % 127 (pos? (* (nth % 123) (nth % 124)))))
        reverse)))

(defn compare-hands [hand-a hand-b]
  (loop [a (score-hand hand-a)
         b (score-hand hand-b)]
    (let [res (compare (first a) (first b))]
      (if (= 0 res)
        (recur (rest a) (rest b))
        (/ (+ 1 res) 2)))))


(defn -main []
  (->> (slurp "resources/poker.txt")
       clojure.string/split-lines
       (map #(compare-hands (subs % 0 14) (subs % 15)))
       (reduce +)
       println))



