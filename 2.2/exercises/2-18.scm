(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse (cdr items))
	      (list (car items)))))

(define (reverse items)
  (define (reverse-iter r-items last-index)
    (if (< last-index 0)
	r-items
	(reverse-iter (append r-items (list (list-ref items last-index)))
		      (- last-index 1))))
  (reverse-iter (list)
		(- (length items) 1)))

(reverse (list 1 4 9 16 25))


