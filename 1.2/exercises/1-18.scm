(define (fast-expt a b)
  (fast-expt-iter a 0 b))

(define (fast-expt-iter a sum b)
  (cond ((= b 0) sum)
	((even? b) (fast-expt (double a)
			      sum
			      (halve b)))
	(else (fast-expt a
			 (+ a sum)
			 (- b 1)))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

(fast-expt 3 2)
(fast-expt 4 0)
(fast-expt 4 5)