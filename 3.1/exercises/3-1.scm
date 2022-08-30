(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define A (make-accumulator 5))
(A 10)
(A 10)