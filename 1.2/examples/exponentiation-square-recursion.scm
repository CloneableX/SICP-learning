(define (fast-exp a n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-exp a (/ n 2))))
	(else (* a (fast-exp a (- n 1))))))

(define (even? x)
  (= (remainder x 2) 0))

(fast-exp 2 3)
(fast-exp 3 2)