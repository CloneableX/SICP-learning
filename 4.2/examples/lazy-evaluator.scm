(define (eval exp env)
  (cond ((application? exp)
	 (apply (actual-value (operator exp) env)
		(operands exp)
		env))
	((if? exp) (eval-if exp env))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure
	  procedure
	  (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence
	  (proceudre-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-delayed-args arguments env)
	   env)))
	(else (error "Unknown procedure type: APPLY"
		     procedure))))
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps) env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
	    (list-of-delayed-args (rest-operands exps) env))))

(define (actual-value exp env)
  (force-it (eval exp env)))



;;; thunk
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value (thunk-exp obj) (thunk-env obj))))
	   (set-car! thunk 'evaluated-thunk)
	   (set-car! (cdr thunk) result)
	   (set-car! (cddr thunk) '())
	   result))
	((evaluated-thunk? obj) (thunk-value obj))
	(else obj)))




(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequence exp) env)
      (eval (if-alternative exp) env)))




(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
	   (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output))
    (dirver-loop)))