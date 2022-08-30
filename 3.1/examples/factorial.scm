(define (factorial n)
  (let ((counter 1)
	(product 1))
    (define (iter)
      (if (> counter n)
	  product
	  (begin
	    (set! product (* product counter))
	    (set! counter (+ counter 1))
	    (iter))))
    (iter)))

(factorial 5)
