(define (eval exp env)
  (cond ((quoted? exp) (text-of-quotation exp env))))

(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
	(eval (quotation->list text) env)
	text)))

(define (quotation->list exp)
  (if (null? exp)
      (list 'quote '())
      (list 'cons
	    (list 'quote (car exp))
	    (quotation->list (cdr exp)))))