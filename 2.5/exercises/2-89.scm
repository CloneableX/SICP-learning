(define (adjoin-term term term-list)
  (let ((t1 (first-term term-list)))
    (if (>= (order term) (order t1))
	(cons (coeff term) term-list)
	(cons (coeff t1)
	      (adjon-term term (rest-terms term-list))))))

(define (first-term term-list)
  (make-term (- (length term-list) 1)
	     (car term-list)))

