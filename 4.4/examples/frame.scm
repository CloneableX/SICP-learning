(load "expression.scm")

(define (make-binding variable value)
  (cons variable value))
(define (binding-value binding) (cdr binding))
(define (binding-variable binding) (car binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend-frame variable value frame)
  (let ((binding (make-binding variable value)))
    (cons binding frame)))

(define (instantiate exp frame unbund-var-handler)
  (define (copy exp)
    (cond ((var? exp)
	   (let ((binding (binding-in-frame exp frame)))
	     (if binding
		 (copy (binding-value binding))
		 (unbund-var-handler exp frame))))
	  ((pair? exp)
	   (cons (copy (car exp))
		 (copy (cdr exp))))
	  (else exp)))
  (copy exp))

(define (constract-question-mark var)
  (string->symbol
   (string-append "?" 
		  (symbol->string (cadr var)))))


(define (merge-if-balance frame1 frame2 exps)
  (let ((frame (merge-frames frame1 frame2)))
    (if (all-variables-in? frame (all-variables exps))
	frame
	'())))

(define (all-variables-in? frame variables)
  (define (variables-in? frame vars)
    (cond ((null? vars) true)
	  ((binding-in-frame (car vars) frame)
	   (variables-in? frame (cdr vars)))
	  (else false)))
  (variables-in? frame variables))

(define (all-variables exps)
  (define (find-variables exp variables)
    (cond ((var? exp)
	   (if (member exp variables)
	       variables
	       (cons exp variables)))
	  ((pair? exp)
	   (find-variables (cdr exp)
			   (find-variables (car exp) variables)))
	  (else variables)))
  (find-variables exps '()))

(define (merge-frames frame1 frame2)
  (define (migrate-frame frame result-frame)
    (if (null? frame)
	result-frame
	(let ((first-binding (car frame)))
	  (let ((binding (binding-in-frame (binding-variable first-binding)
					   result-frame)))
	    (if binding
		(if (equal? (binding-value binding)
			    (binding-value first-binding))
		    (migrate-frame (cdr frame) result-frame)
		    '())
		(migrate-frame (cdr frame)
			       (cons first-binding result-frame)))))))
  (migrate-frame (append frame1 frame2) '()))