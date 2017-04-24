(asdf:load-system :cl-ppcre)
(defun iota (count &optional (start 1)) 
  (loop for n from 1 to count for x from start collect x))

(defun card-rank (card) (+ 2 (position (elt card 0) "23456789TJQKA")))
(defun card-suit (card) (elt card 1))
(defstruct hand-score rank rank-name ranked-cards all-cards)

(defun group-by-rank (sequence)  ; should be more general (with comparator and key really)
  (let ((groups (list (list (first sequence)))))
    (dolist (x (cdr sequence))
      (if (= (card-rank (first (first groups)))
             (card-rank x))
          (push x (first groups))
          (push (list x) groups)))
    groups))

(defun score-hand (hand)
  (let* ((hand      (sort hand '< :key 'card-rank))
         (flush     (not (find (card-suit (first hand)) hand :test-not 'eql :key 'card-suit)))
         (straight  (equal (mapcar 'card-rank hand)
                           (iota 5 (card-rank (first hand))))))
    (flet ((groups-of (n)
             (remove n (group-by-rank hand) :test-not '= :key 'length)))
      (macrolet ((scores (&rest scores)
                   `(or ,@ (loop for (name test) in scores for rank downfrom (length scores)
                              collect `(let ((match ,test))
                                         (when match (make-hand-score :rank ,rank :rank-name ',name
                                                                      :ranked-cards match :all-cards (reverse hand))))))))
        (scores                         ; highest scoring first
         (royal-flush      (when (and straight flush
                                      (= 10 (card-rank (first hand))))
                             hand))
         (straight-flush   (when (and straight flush) hand))
         (four-of-a-kind   (first (groups-of 4)))
         (full-house       (and (groups-of 2) (first (groups-of 3))))
         (flush            (when flush hand))
         (straight         (when straight hand))
         (three-of-a-kind  (first (groups-of 3)))
         (two-pairs        (when (cdr (groups-of 2))
                             (apply 'append (groups-of 2))))
         (one-pair         (first (groups-of 2)))
         (high-card        (last hand)))))))

(defun greater-score-p (a b) ; early exit could probably be eliminated
  (flet ((highest-ranked-card (score)
           (reduce #'max (mapcar #'card-rank (hand-score-ranked-cards score))))
         (compare (a b)
           (unless (= a b)  (return-from greater-score-p (> a b)))))
    (compare (hand-score-rank a) (hand-score-rank b))
    (compare (highest-ranked-card a) (highest-ranked-card b))
    (mapcar #'compare ; otherwise compare *all* the cards
            (mapcar 'card-rank (hand-score-all-cards a))
            (mapcar 'card-rank (hand-score-all-cards b)))
    (error "Tie (this wasn't supposed to happen)")))

(format t "Player 1 wins ~A time~:P~%"
        (count-if (lambda (row)
                    (greater-score-p (score-hand (subseq row 0 5))
                                     (score-hand (subseq row 5))))
                  (with-open-file (stream "./poker.txt")
                    (loop for line = (read-line stream nil) while line collect (cl-ppcre:split " " line)))))

