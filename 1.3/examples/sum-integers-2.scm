(load "utils/sum.scm")

(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 3)
(sum-integers 1 10)
