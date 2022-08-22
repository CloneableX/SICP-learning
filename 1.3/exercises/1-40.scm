(load "utils/newton-method.scm")

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newton-method (cubic 1 2 3) 1)
(newton-method (cubic -9 -9 -10) 1)