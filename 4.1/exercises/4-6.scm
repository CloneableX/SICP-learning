(define (eval exp env)
  (cond ((let? exp) (eval (let->combination exp) env))))

(define (let->combination exp)
  (list (make-lambda (let-parameters exp)
		     (let-body exp))
	(let-arguments exp)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp)
  (map car (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let-arguments exp)
  (map cadr (cadr exp)))