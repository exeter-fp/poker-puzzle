(ns p54.core)

(defn score [hand]
  (let [idx #(+ 120 (.indexOf [:pair :2pair :3kind :straight :flush :full-house :4kind :straight-flush] %))
        assoc-cnt (fn [v c cnt]                             ; find first idx of cnt in the 'score vector'
                    (let [cnt-i (.indexOf v cnt)]           ; place that idx into appropriate slot identified by c
                      (if (pos? cnt-i)
                        (-> (assoc v (idx c) cnt-i)
                            (assoc cnt-i 0))
                        v)))
        straight (fn [hand]
                   (->> (apply str (sort (clojure.string/replace hand #"[hcds ]" "")))
                        (.indexOf "23456789ABCDE")
                        inc))
        combo (fn [v res a b] 
                (assoc v (idx res) (pos? (* (nth v (idx a)) (nth v (idx b))))))
        hand (reduce #(clojure.string/replace %1 (first %2) (second %2)) ; fix hand string so card char value
                     hand                                                ; is appropriately ascending
                     ["Hh" "Dd" "Cc" "Ss" "JB" "QC" "KD" "AE" "TA"])]
    (-> (reduce #(update %1 (int %2) inc) (into [] (repeat 128 0)) hand) ; count into respective positions in score vector
        (assoc-cnt :flush 5)                                             ; check for flush first otherwise suit count will confuse things later
        ((fn [v] (reduce #(assoc %1 (int %2) 0) v "hcds ")))             ; clear suit count
        (assoc-cnt :pair 2)
        (assoc-cnt :2pair 2)                                ; if there's still a pair it's a second pair and higher value
        (assoc-cnt :3kind 3)
        (assoc (idx :straight) (straight hand))
        (combo :full-house :3kind :pair)
        (assoc-cnt :4kind 4)
        (combo :straight-flush :straight :flush)
        reverse)))

(defn -main []
  (->> (clojure.string/split-lines (slurp "resources/poker.txt"))
       (map (fn [line] (if (pos? (some #(when (not= 0 %) %)
                                       (map compare
                                            (score (subs line 15))
                                            (score (subs line 0 14))))) 0 1)))
       (reduce +)
       println))



