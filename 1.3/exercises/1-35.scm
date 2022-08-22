(load "utils/fixed-point.scm")

(define varphi
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

(display varphi)