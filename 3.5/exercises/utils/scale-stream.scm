(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
	      s))
