(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
