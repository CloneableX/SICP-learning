(define (cont-frac n d k)
  (define (accumulate-iter x result)
    (if (= x 0)
	result
	(accumulate-iter (- x 1)
			 (/ (n x)
			    (+ (d x) result)))))
  (accumulate-iter k 0))

