(define (double f)
  (lambda (x) (f (f x))))

((double (lambda (x) (+ x 1))) 1)
(((double (double double)) (lambda (x) (+ x 1))) 5)