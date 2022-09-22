(define (eval exp env)
  (cond ((and? exp) (eval-and exp env))
	((or? exp) (eval (or->and exp) env))))

(define (and? exp) (tagged-list? exp 'and))
(define (make-and operands)
  (cons 'and operands))

(define (or? exp) (tagged-list? exp 'or))


(define (eval-and exp env)
  (define (loop exps)
    (let ((first-value (eval (first-exp exps) env)))
      (cond ((true? first-value)
	     (if (last-exp? exps)
		 first-value
		 (eval-and (rest-exp exps) env)))
	    (else false))))
  (loop (operands exp)))

(define (or->and exp)
  (define (loop exps)
    (if (no-operands? exps)
	'()
	(lambda () 
	  (not 
	   (make-and (cons (lambda () (not (first-operand exps)))
			   (loop (rest-operands exps))))))))
  (loop (operands exp)))