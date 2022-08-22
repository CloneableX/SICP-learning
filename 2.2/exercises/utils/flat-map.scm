(define (flat-map proc sequence)
  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
	    (accumulate op initial (cdr sequence)))))
  (accumulate append '() (map proc sequence)))

