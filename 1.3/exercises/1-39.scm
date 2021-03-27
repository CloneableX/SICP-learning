(load "1-37.scm")

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
	x
	(- (square x))))
  (define (D i)
    (- (* i 2) 1))
  (cont-frac-iter N D k))

(tan 10)
(tan-cf 10.0 1000)
(tan 25)
(tan-cf 25.0 1000)