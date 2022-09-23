(define (let*->nested-lets exp)
  (expand-lets (let*-assignments exp)
	       (let*-body exp)))

(define (expand-lets assignments body)
  (define (loop assigns)
    (if (null-assignments? assigns)
	body
	(make-let (first-assignment assignments)
		  (loop (rest-assignments assignments)))))
  (loop assignments))

(define (let*-assignment exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (null-assignments? assignments) (null? assignments))
(define (first-assignment assignments) (car assignments))
(define (rest-assignments assignments) (cdr assignments))