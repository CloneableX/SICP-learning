(define (same-parity x . items)
  (define (filter-parity items)
    (cond ((null? items) items)
	  ((or (and (even? x) (even? (car items)))
	       (and (odd? x) (odd? (car items))))
	   (cons (car items)
		 (filter-parity (cdr items))))
	  (else (filter-parity (cdr items)))))
  (cons x (filter-parity items)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


