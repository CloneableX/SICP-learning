(define (eval exp env)
  (cond ((let? exp) (eval (let->combination exp) env))))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let ((let-exp (cons (car exp) (cddr exp))))
	(let ((arguments (let-arguments exp))
	      (lambda-exp (car (let->combination let-exp))))
	  (make-begin 
	   (list (make-define (cadr exp) lambda-exp)
		 (cons (cadr exp) arguments)))))
      (list (make-lambda (let-parameters exp)
			 (let-body exp))
	    (let-arguments exp))))

(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp)
  (map car (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let-arguments exp)
  (map cadr (cadr exp)))

(define (make-define var val)
  (list 'define var val))