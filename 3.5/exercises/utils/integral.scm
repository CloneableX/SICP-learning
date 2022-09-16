(define (add-streams s1 s2) (stream-map + s1 s2))
(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
	      s))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 (add-streams (scale-stream integrand dt)
			      int)))
  int)
