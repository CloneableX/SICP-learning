(define (fast-exp b n)
  (exp-iter b 1 n))

(define (exp-iter b product exp)
  (cond ((= exp 0) product)
	((even? exp) (exp-iter (square b)
			       product
			       (/ exp 2)))
	(else (exp-iter b
			(* b product)
			(- exp 1)))))

(define (even? x)
  (= (remainder x 2) 0))

(fast-exp 2 3)
(fast-exp 2 2)
(fast-exp 2 0)