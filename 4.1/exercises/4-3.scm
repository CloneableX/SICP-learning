(define (eval exp env)
  (define (exp-type exp) (car exp))
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	(else
	 (let ((eval-proc (get 'eval (exp-type exp))))
	   (if eval-proc
	       (eval-proc exp env)
	       (error "Unknown expression type: EVAL" exp))))))

(define (install-quoted-package)
  (define (text-of-quotation exp env)
    (cadr exp))
  (put 'eval 'quote text-of-quotation)
  'done)