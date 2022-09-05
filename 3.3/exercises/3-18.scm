(load "utils/make-cycle.scm")

(define (cycle-list? items)
  (define (compare-half right-half)
    (cond ((null? right-half) false)
	  ((eq? items right-half) true)
	  (else
	   (compare-half (cdr right-half)))))
  (compare-half (cdr items)))

(define x (list 'a 'b 'c))
(define cycle (make-cycle x))

(cycle-list? cycle)

(cycle-list? (list 'a 'b 'c))
