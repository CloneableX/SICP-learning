(load "utils/sum.scm")

(define (pi-sum a b)
  (define (pi-term x)
    (+ (/ 1.0 (* x (+ x 2)))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(pi-sum 1 3)
(pi-sum 1 10)
