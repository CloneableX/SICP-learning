(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (average a b)
  (/ (+ a b) 2))