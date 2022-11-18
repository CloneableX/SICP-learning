(define (instruction-execute-proc inst) (cdr inst))
(define (instruction-text inst) (car inst))
(define (update-instruction-execute-proc! inst proc)
  (set-cdr! inst proc))

(define (make-instruction text) (cons text '()))
(define (make-instruction-with-proc text proc) (cons text proc))
