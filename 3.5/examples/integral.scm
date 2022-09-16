(load "utils/add-streams.scm")
(load "utils/scale-stream.scm")

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)