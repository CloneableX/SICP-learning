(define (deep-reverse items)
  (cond ((null? items) items)
	((not (pair? items)) items)
	(else (list (deep-reverse (car (cdr items)))
		    (deep-reverse (car items))))))

(deep-reverse (list (list 1 2) (list 3 4)))


