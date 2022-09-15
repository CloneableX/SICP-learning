(load "utils/integers.scm")
(load "utils/scale-stream.scm")
(load "utils/mul-streams.scm")

(define (div-streams s1 s2)
  (stream-map / s1 s2))
(define (integrate-series stream)
  (mul-streams (div-streams ones integers) stream))

(define consine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series (cons-stream 0 (integrate-series consine-series)))

(stream-head consine-series 10)
(stream-head sine-series 10)


