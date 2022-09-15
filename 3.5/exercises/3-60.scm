(load "utils/add-streams.scm")
(load "utils/scale-stream.scm")
(load "3-59.scm")

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
			    (mul-series (stream-cdr s2) s1))))

(stream-head (add-streams (mul-series consine-series consine-series)
			  (mul-series sine-series sine-series))
	     10)
