(asdf:load-system :cl-ppcre)
(defun iota (count &optional (start 1)) 
  (loop for n from 1 to count for x from start collect x))

(defun group (sequence)  ; should be more general (with comparator and key really)
  (let ((groups (list (list (first sequence)))))
    (dolist (x (cdr sequence))
      (if (= x (first (first groups)))
          (push x (first groups))
          (push (list x) groups)))
    groups))

(defun card-rank (card) (+ 2 (position (elt card 0) "23456789TJQKA")))
(defun card-suit (card) (elt card 1))

(defun score-hand (hand)
  (let* ((flush     (not (find (card-suit (first hand)) hand :test-not 'eql :key 'card-suit)))
         (ranks     (sort (mapcar 'card-rank hand) '<))
         (straight  (equal ranks (iota 5 (first ranks))))
         ;; sorting the groups like this simplifies finding the 'scoring' part of the hand
         (groups    (sort (group ranks) '>
                          :key (lambda (x) (+ (* (length x) 100) (first x))))))
    (flet ((make-hand-score (name rank ranked)
             (append (list name rank (first ranked)) (reverse ranks)))
           (groups (&rest matches)
             (equal matches (mapcar 'length groups))))
      (macrolet ((scores (&rest scores)
                   `(or ,@ (loop for (name test) in scores for rank downfrom (length scores)
                              collect `(when ,(or test name)
                                         (make-hand-score ',name ,rank (first groups)))))))
        (scores ; highest scoring first
         (royal-flush      (and straight flush (= 10 (first ranks))))
         (straight-flush   (and straight flush))
         (four-of-a-kind   (groups 4 1))
         (full-house       (groups 3 2))
         (flush)
         (straight)
         (three-of-a-kind  (groups 3 1 1))
         (two-pairs        (groups 2 2 1))
         (one-pair         (groups 2 1 1 1))
         (high-card        t))))))

;; (taking the cdr of the score because the car is the score's name)
(defun greater-score-p (a b)
  (loop for p1 in (cdr a) for p2 in (cdr b)
     unless (= p1 p2) do (return-from greater-score-p (> p1 p2)))
  (error "Scores are tied - this wasn't supposed to happen!"))

(format t "Player 1 wins ~A time~:P~%"
        (count-if (lambda (row)
                    (greater-score-p (score-hand (subseq row 0 5))
                                     (score-hand (subseq row 5))))
                  (with-open-file (stream "./poker.txt")
                    (loop for line = (read-line stream nil) while line collect (cl-ppcre:split " " line)))))
