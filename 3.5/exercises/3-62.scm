(load "3-59.scm")
(load "3-60.scm")
(load "3-61.scm")

(define (div-series s1 s2)
  (mul-series s1
	      (invert-unit-series s2)))


(define tangent-series (div-series sine-series consine-series))

(stream-head tangent-series 10)