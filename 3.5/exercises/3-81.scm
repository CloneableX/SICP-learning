(define random-init 1)
(define (random-update x) (+ x 1))

(define (rand request-stream)
  (define (dispatch request x)
    (let ((m (car request)))
      (cond ((eq? m 'generate) (random-update x))
	    ((eq? m 'reset) (cadr request))
	    (else
	     (error "Unkown operation: RAND" request)))))
  (define random-numbers
    (cons-stream
     random-init
     (stream-map dispatch request-stream random-numbers)))
  random-numbers)

(stream-head (rand (stream '(reset 100)
			   '(generate)
			   '(generate)
			   '(generate)
			   '(reset 10)
			   '(generate)
			   '(generate)))
	     7)

	    