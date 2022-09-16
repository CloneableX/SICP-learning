(load "utils/add-streams.scm")
(load "utils/scale-stream.scm")

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt)
		    int))))
  int)

(define (solve f v0 dt)
  (define y (integral (delay dy) v0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y)
		   1
		   0.001)
	    1000)