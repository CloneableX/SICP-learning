(define (if-fail-consequent exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))


(define (analyze-if-fail exp)
  (lambda (env succeed fail)
    (let ((cproc (if-fail-consequent exp))
	  (aproc (if-fail-alternative exp)))
      (succeed
       (make-if cproc cproc aproc)
       fail))))