(load "utils/cont-frac.scm")

(define (tan-cf x k)
  (define (N i)
    (if (= i 1)
	x
	(- (square x))))
  (* (cont-frac N
	     (lambda (i) (- (* i 2) 1))
	     k)
     1.0))

(tan 10)
(tan-cf 10 100)

(tan 25)
(tan-cf 25 100)