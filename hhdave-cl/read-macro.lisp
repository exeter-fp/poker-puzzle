
;; this will probably need to go in its own system and package
;; I could upload it as an experiment.

(defpackage :read-macro
  (:use :cl)
  (:export #:min #:max #:id #:reverse #:length #:first #:id))

(in-package :read-macro)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PART 1

;; In which we define methods with stupid names

;; These will be the operators in our little embedded programming language


;; 1.1 range
(defmethod |op/..| ((a number) (b number))
  (if (> b a)
      (loop for i from a to b collect i)
      (loop for i downfrom a while (>= i b) collect i)))

;; (|op/..| 1 10)
;; (|op/..| 10 1)

(defmethod |op/..| ((a character) (b character))
  (mapcar 'code-char (|op/..| (char-code a) (char-code b))))

;; (|op/..| #\a #\z)
;; - of course, the actual characters returned will be implementation dependent but generally useful


;; this implementation intially assumes linear polynomial, which I would like to change 
(defmethod |op/..| ((a list) (b number))
  (loop for i from (first a) to b by (- (second a) (first a))
       collect i))

;; (|op/..| '(2 4) 10)

;; now we get some more interesting things...
(defmethod |op/..| ((a function) (b number))
  (lambda (x) (|op/..| (funcall a x) b)))

;; (funcall (|op/..| (lambda (x) (reduce #'min x)) 10) '(4 7 2))

(defmethod |op/..| ((a number) (b function))
  (lambda (x) (|op/..| a (funcall b x))))

(defmethod |op/..| ((a function) (b function))
  (lambda (x) (|op/..| (funcall a x) (funcall b x))))

;; thusly:-
;; (funcall (|op/..| (lambda (x) (apply 'min x)) (lambda (x) (apply 'max x))) '(5 7 8 5 2 3 5)) 





;; 1.2 function application

(defmethod |op/| ((a function) (b function))
  (lambda (x) (funcall a (funcall b x))))

;; (funcall (|op/ | #'first #'last) '(1 2 3 4))

(defmethod |op/| ((a function) b)
  (funcall a b))

;; (|op/ | #'first '(1 2 3))

(defmethod |op/| ((a list) (b function))
  (mapcar b a))

;; (|op/ | '(1 2 3) #'oddp)

(defmethod |op/| ((a sequence) (b number))
  (elt a b))

;; (progn [1,2,3,4 9])



(defmethod |op/,| ((a list) (b list))
  (append a b))
;; (\, '(1 2 3) '(3 4 5))

(defmethod |op/,| ((a list) b)
  (append a (list b)))
;; (\, '(1 2 3) 4)

(defmethod |op/,| (a b) (|op/,| (list a) b))
;; (\, 1 2)

;; (the comma operator above isn't going to be efficient)
;; it would be better for it to evaluate right to left, but we're not going to do that

(defmethod |op/,| ((a function) (b function))
  (lambda (x) (|op/,| (funcall a x) (funcall b x))))

;; (let ((l '(5 7 8 4 2 4))) [min,max l])



(defmethod |op/<| ((a number) (b number)) (< a b))
(defmethod |op/>| ((a number) (b number)) (> a b))

(defmethod |op/<| ((a list) (b list))
  (loop for x in a for y in b
     unless (eql x y)
     return (< x y)))

(defmethod |op/>| ((a list) (b list))
  (loop for x in a for y in b
       unless (eql x y)
       return (> x y)))

;; !!! SORTING by key
(defmethod |op/<| ((a function) (b list))
  (sort (mapcar #'identity b) #'|op/<| :key a))

(defmethod |op/>| ((a function) (b list))
  (sort (mapcar #'identity b) #'|op/>| :key a))


(defmethod |op/=| (a b) (eql a b))

(defmethod |op/=| ((a list) (b list))
  (equal a b))

;; does the result of applying a to b equal b
(defmethod |op/=| ((a function) (b list))
  (equal (funcall a b) b))


;; is each list item equal according to the value of the function b on the list items
(defmethod |op/=| ((a list) (b function))
  (not (find (funcall b (first a))
             (cdr a)
             :test-not #'|op/=|
             :key b)))

;; (let ((x (map 'list 'identity "aAaaA")) (u #'char-upcase )) [x=u])
;; (let ((x (map 'list 'identity "aAbaA")) (u #'char-upcase )) [x=u])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PART 2

;; In which we create a read macro which generates lisp code which invokes the above methods


(defun string-list (items sep)
  (with-output-to-string (s)
    (loop for (this . rest) on items
         do (format s "~A" this)
         when rest
         do (write-sequence sep s))))

;; This implementation is a bit naff. It would be better to not use regexes and just write a more sensible parser
;; It won't be difficult. This was just the quickest way to get it basically working.
(defun read-bracketed-expression (stream char)
  (declare (ignore char))
  (let ((expression (with-output-to-string (x)
                      (let ((open 1))
                        (loop for char = (read-char stream t nil t)
                           when (eql #\] char) do (decf open)
                           when (eql #\[ char) do (incf open)
                           while (> open 0)
                           do (write-char char x))))))

    ;; I could probably introspect all the definitions in this package to find ones beginning with op/
    (let ((operators '("\\.\\." "\\*" "\\+" "\\-" "\\/" "\\," " " "\\<" "\\>" "\\=")))
      (labels ((read-object (object)
                 (let ((x (cl-ppcre:split "\\.()" object :with-registers-p t)))
                   (result (read-from-string (first x))
                           (cdr x)))
                 #+nil(read-from-string object)
                 )
               (result (x rest)
                 (if rest
                     (result (list (intern (format nil "op/~A" (first rest)) :read-macro)
                                   x
                                   (read-object (second rest)))
                             (cddr rest))
                     x)))
        
        (let ((tokens (mapcar (lambda (x)
                                (cl-ppcre:regex-replace "\\s+$"
                                                        (cl-ppcre:regex-replace "^\\s+" x "")
                                                        ""))
                              (cl-ppcre:split (format nil "(~A|\\[|\\])"
                                                      (string-list (mapcar (lambda (x)
                                                                             (format nil "\\s*~A\\s*" x))
                                                                           operators) "|"))
                                              expression :with-registers-p t))))
          (result (read-object (first tokens)) (cdr tokens)))))))



(set-macro-character #\[ 'read-bracketed-expression)

;; (quote [a+b.c+d])
;; '([1+foo..end-start])
;; '([1 + foo   ..end-start])
;; '([1 + foo a   ..end-start])

;; (progn [1..5])
;; (progn [1,2,3,4])
;; (progn [2,4..10])
;; (quote [2,4..10])
;; (progn [1+2+3+4])
;; (quote [r])



;; ok. This is basically working now, and is pretty straightforward. We can start experimenting with it...

;; I think it's associating the wrong way. 
;; I've now got it working the right way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PART 3

;; In order to get the polymorphic operators WRT functions we effectively need things to be lisp-1-ish
;; we can (of course) introduce a notation to write functions, but it will get tedious

(defparameter min (lambda (numbers) (reduce #'min numbers)))
(defparameter max (lambda (numbers) (reduce #'max numbers)))

;; (let ((l '(5  2 4 56 7 56 4))) [min..max l])
;; (let ((l '(4 5 6 7 8))) [min..max l=l]) ; equivalent to the one below
;; (let ((l '(4 5 6 7 8))) [min..max=l]) ; *
;; (let ((l '(4 6 5 7 8))) [min..max=l])

;; * - this means 'is the list l equal to the range of numbers from the smallest in l to the largest'. Pretty concise eh?

(defparameter id #'identity)
(defparameter reverse #'reverse)
(defparameter length #'length)
;; (let ((l '(1 2 3 4 5))) [reverse l])

(defparameter first #'first)

;; to get this to do all I need I need to handle the dot as well
;; nested expressions would be useful to get working too. I wonder how hard that would be.


