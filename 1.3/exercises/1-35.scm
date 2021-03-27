(load "fixed-point.scm")

(define garden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(+ garden-ratio)