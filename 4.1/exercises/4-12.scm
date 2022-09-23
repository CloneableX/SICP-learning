(define (scan-frame frame var null-proc target-proc)
  (define (scan vars vals)
    (cond ((null? vars) (null-proc))
	  ((eq? var (car vars)) (target-proc vals))
	  (else (scan (cdr vars) (cdr vals)))))
  (scan (frame-variables frame)
	(farme-values frame)))

(define (env-loop var env error-proc target-proc)
  (define (loop env)
    (if (eq? env the-empty-environment)
	(error-proc)
	(scan-frame (first-frame env)
		    var
		    (lambda () (loop (enclosing-environment env)))
		    target-proc)))
  (loop env))

(define (lookup-variable-value var env)
  (env-loop var env
	    (lambda () (error "Unbound variable" var))
	    (lambda (vals) (car vals))))

(define (set-variable-value! var val env)
  (env-loop var env
	    (lambda () (error "Unbound variable: SET!" var))
	    (lambda (vals) (set-car! vals val))))

(define (define-variable var val env)
  (let ((frame (first-frame env)))
    (scan-frame frame var
		(lambda () (add-binding-to-frame! var val frame))
		(lambda (vals) (set-car! vals val)))))


