(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(= 1
   (car (cons 1 3)))
(= 3
   (cdr (cons 1 3)))