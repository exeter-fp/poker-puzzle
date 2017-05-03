(asdf:load-system :cl-ppcre)
(load "read-macro.lisp")
(use-package :read-macro)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; problem 54 with the aid of a cunning read macro

;; Because we can

;; This is an implementation of problem 54 using a cunning (somewhat
;; APL inspired) read macro which makes certain things very concise
;; and (hopefully!) fairly clear too. The general idea is that things
;; in square brackets are expressions consisting of a sequence of
;; operands separated by dyadic operators. The set of operators is
;; fairly small and each operator is polymorphic on 3 main argument
;; types:-

;; 1. scalar values (numbers, characters and so on)
;; 2. lists (potentially other sequences too)
;; 3. functions

;; Operators are:-
;; +, -, /, * - simple arithmetic operators for scalars
;; =, <, >    - comparators AND sort operators for lists in the latter 2 cases
;; ..         - handy numeric range operator
;; <space>    - function(ish) application
;; .          - function application, but with high precedence
;; ,          - list operator

;; expressions are evaluated left to right and all operators have the
;; same precedence EXCEPT the dot operator which has higher
;; precedence. Even function application is polymorphic on operand
;; types.

;; While [fn list] will apply the function to the list, [list fn] will
;; MAP the function over the list. This is the general pattern.

;; Now, while [n1..n2] gives a numeric range from n1 to n2 [f1..f2]
;; (where f1 and f2 are functions) gives a FUNCTION which gives a
;; numeric range when applied to some object.

;; For example, [min..max] gives a function which when applied to some
;; object gives a numeric range from the value of the 'min' function
;; applied to it TO the value of the 'max' function applied to
;; it. Thus [min..max ranks] gives a numeric range from the min rank
;; to the max rank. 

;; Similarly, [f1,f2] gives a function like: (lambda (x) (list (f1 x) (f2 x)))
;; (in pseudo lisp), though it actually uses the , operator again.

;; Somewhat non obviously, [fn<list] SORTS the list in descending
;; order keying on the function. To sort a list of numbers you have to
;; do [id<list], where id is the identity function.

;; An alternative would be to just do [sort list], but sort needs a
;; comparator really.

;; [list=fn] checks whether each item of the list has equal value of
;; (fn item)

;; OTOH, [fn=list] checks whether the result of applying fn to list
;; yields a list which is equal to the original list (ie list is
;; invariant? a fixed point?) under application of fn.



(defun split-list (list n) (list (subseq list 0 n) (subseq list n)))

(defparameter group
  (lambda
      (sequence) ; should be more general (with comparator and key really)
    (let ((groups (list (list (first sequence)))))
      (dolist (x (cdr sequence))
        (if (= x (first (first groups)))
            (push x (first groups))
            (push (list x) groups)))
      groups)))

(defun score-hand (hand)
  (let* ((rank      (lambda (card) (+ 2 (position (elt card 0) "23456789TJQKA"))))
         (suit      (lambda (card) (elt card 1)))
         (ranks     [id<hand.rank])    ; sorted list of ranks
         ;; sorting the groups like this simplifies finding the 'scoring' part of the hand
         (groups    [length,first,id>group.ranks]))
    (flet ((make-hand-score (name rank ranked)   [name,rank,ranked.0,reverse.ranks])
           (groups (&rest matches)               [matches=groups.length]))
      (macrolet ((scores (&rest scores)
                   `(or ,@ (loop for (name test) in scores for rank downfrom (length scores)
                              collect `(when ,(or test name)
                                         (make-hand-score ',name ,rank (first groups)))))))
        (scores                         ; highest scoring first
         (royal-flush      (and [10..14=ranks] [hand=suit])) ; this is much clearer once you understand the notation
         (straight-flush   (and [min..max=ranks] [hand=suit]))
         (four-of-a-kind   (groups 4 1))
         (full-house       (groups 3 2))
         (flush            [hand=suit])
         (straight         [min..max=ranks])
         (three-of-a-kind  (groups 3 1 1))
         (two-pairs        (groups 2 2 1))
         (one-pair         (groups 2 1 1 1))
         (high-card        t)))
      )))

(format t "Player 1 wins ~A time~:P~%"
        (count-if (lambda (scores)
                    (when [scores.0=scores.1]
                      (error "Scores are tied - this wasn't supposed to happen"))
                    [scores.0>scores.1]
                    )
                  (with-open-file (stream "./poker.txt")
                    (loop for line = (read-line stream nil) while line
                       collect
                         (mapcar 'score-hand (split-list (cl-ppcre:split " " line) 5))))))
