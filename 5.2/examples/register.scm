(define (make-register)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (new-contents) (set! contents new-contents)))
	    (else
	     (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register contents)
  ((register 'set) contents))