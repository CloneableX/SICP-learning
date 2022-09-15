(load "utils/scale-stream.scm")
(load "3-59.scm")

(define (invert-unit-series stream)
  (cons-stream
   1
   (scale-stream (mul-series (stream-cdr stream)
			     (invert-unit-series stream))
		 -1)))

(stream-head (invert-unit-series consine-series) 10)