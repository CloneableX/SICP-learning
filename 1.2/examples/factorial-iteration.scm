(define (factorial n)
  (factorial-iter 1 1 n))

(define (factorial-iter count product max-count)
  (if (> count max-count)
      product
      (factorial-iter (+ count 1)
		      (* product count)
		      max-count)))

(factorial 3)
(factorial 10)