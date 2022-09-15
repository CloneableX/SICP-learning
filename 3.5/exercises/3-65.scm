(load "utils/partial-sums.scm")
(load "utils/accelerated-sequence.scm")

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
	       (stream-map - (ln-summands (+ n 1)))))
(define ln-stream
  (partial-sums (ln-summands 1)))

(stream-head ln-stream 3) 



(stream-head (accelerated-sequence euler-transform ln-stream) 3)