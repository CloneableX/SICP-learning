(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
	((= x (car set)) set)
	(else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 2 (list 1 3))
(adjoin-set 3 (list 1 2 3))
(adjoin-set 4 (list 1 2 3 8 10))
(adjoin-set 10 (list 1 2 3))
(adjoin-set 10 '())