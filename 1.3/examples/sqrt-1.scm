(load "utils/average.scm")
(load "fixed-point.scm")

(define (sqrt x)
  (fixed-point (lambda (y) (average (/ x y) y))
	       1.0))

(sqrt 4)
(sqrt 9)