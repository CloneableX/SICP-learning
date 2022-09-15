(load "utils/integers.scm")
(load "utils/add-streams.scm")

(define (partial-sums stream)
  (define p
    (cons-stream (stream-car stream)
		 (add-streams (stream-cdr stream) p)))
  p)
(define partials (partial-sums integers))

(stream-ref partials 3)
(stream-ref partials 4)
