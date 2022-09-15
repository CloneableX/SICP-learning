(load "utils/sqrt-stream.scm")

(define (stream-limit s tolerance)
  (define (iter s0 sr)
    (if (< (abs (- s0 (stream-car sr)))
	   tolerance)
	(stream-car sr)
	(iter (stream-car sr) (stream-cdr sr))))
  (iter (stream-car s) (stream-cdr s)))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.0001)
(sqrt 3 0.0000001)