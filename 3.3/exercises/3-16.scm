(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(count-pairs (cons (cons 1 '()) (cons 2 '())))

(define two (list 1 2))
(count-pairs (cons two (cdr two)))

(define one (list 1))
(define three (cons one one))
(count-pairs (cons three three))

(load "utils/last-pair.scm")
(define cycle (list 1 2 3))
(set-cdr! (last-pair cycle) cycle)
(count-pairs cycle)


