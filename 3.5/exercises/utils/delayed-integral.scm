(define (add-streams s1 s2) (stream-map + s1 s2))

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
	      s))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
		    int))))
  int)

