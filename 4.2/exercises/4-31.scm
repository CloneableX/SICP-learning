(define (eval exp env)
  (cond ((application? exp)
	 (apply (actual-value (operator exp) env)
		(operands exp)
		env))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure (procedure-body procedure)
				    (list-of-arg-values arguments env)))
	((compound-procedure? procedure)
	 (eval-sequence
	  (procedure-body procedure)
	  (extend-environment
	   (procedure-parameters procedure)
	   (list-of-mixture-args arguments env)
	   env)))
	(else
	 (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (null-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
	    (list-of-arg-values (rest-operands exps) env))))

(define (list-of-mixture-args exps env)
  (if (null-operands? exps)
      '()
      (cons (delay-or-force-it (first-operand exps) env)
	    (list-of-mixture-args (rest-operands exps) env))))

(define (delay-or-force-it exp env)
  (cond ((lazy? exp) (delay-it (car exp) env false))
	((lazy-memo? exp) (delay-it (car exp) env true))
	(else (actual-value exp env))))

(define (actual-value exp env)
  (force-it (eval exp env)))


(define (force-it obj)
  (cond ((thunk? obj)
	 (let ((result (actual-value (thunk-exp obj)
				     (thunk-env obj))))
	   (thunk->evaluated-thunk obj result)
	   result))
	((evaluated-thunk? obj) (thunk-value obj))
	(else obj)))


;; thunk
(define (lazy? exp)
  (and (pair? exp) (eq? 'lazy (cadr exp))))
(define (lazy-memo? exp)
  (and (pair? exp) (eq? 'lazy-memo (cadr exp))))

(define (delay-it exp env memo)
  (list 'thunk exp env memo))
(define (thunk? obj) (tagged-list obj 'thunk))
(define (thunk-memo? thunk) (cadddr thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value thunk) (cadr thunk))

(define (thunk->evaluated-thunk thunk value)
  (if (thunk-memo? thunk)
      (begin
	(set-car! thunk 'evaluated-thunk)
	(set-car! (cdr thunk) value)
	(set-car! (cddr thunk) '()))
      thunk))