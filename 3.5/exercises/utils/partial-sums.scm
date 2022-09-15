(define (add-streams s1 s2) (stream-map + s1 s2))

(define (partial-sums stream)
  (define p
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream) p)))
  p)
