(define (cond-recipient-action? exp) (tagged-list? exp '=>))
(define (recipient exp) (cadr exp))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
	    (rest (cdr clauses)))
	(if (cond-else-clause? first)
	    (if (null? rest)
		(sequence->exp (cond-actions first))
		(error "ELSE clause isn't last: COND->IF" clauses))
	    (if (cond-recipient-action? (cond-actions first))
		(make-if (cond-predicate first)
			 (sequence->exp (cons (recipient (cond-actions first))
					      (cond-predicate first)))
			 (expand-clauses rest))
		(make-if (cond-predicate first)
			 (sequence->exp (cond-actions first))
			 (expand-clauses rest)))))))


