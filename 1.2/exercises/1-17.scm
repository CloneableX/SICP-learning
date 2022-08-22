(define (fast-expt a b)
  (cond ((= b 0) 0)
	((even? b) (fast-expt (double a)
			      (halve b)))
	(else (+ a (fast-expt a (- b 1))))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(fast-expt 3 2)
(fast-expt 4 0)
(fast-expt 4 5)