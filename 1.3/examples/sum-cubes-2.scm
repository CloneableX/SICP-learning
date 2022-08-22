(load "utils/sum.scm")

(define (inc x) (+ x 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 3)
(sum-cubes 1 10)
