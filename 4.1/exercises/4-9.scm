(load "4-8.scm")

(define (eval exp env)
  (cond ((for? exp) (eval (for->let exp) env))))

(define (for->let exp)
  (make-let 'for-iter
	    (list (list 'start (for-start exp))
		  (list 'end (for-end exp)))
	    (make-if '(< start end)
		     (make-begin
		      (list (for-proc exp)
			    '(for-iter (+ start 1) end))))))

(define (for? exp) (tagged-list? exp 'for))
(define (for-start exp) (cadr exp))
(define (for-end exp) (caddr exp))
(define (for-proc exp) (cdddr exp))