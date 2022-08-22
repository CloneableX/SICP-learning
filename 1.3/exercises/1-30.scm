(define (sum term a next b)
  (define (sum-iter n result)
    (if (> n b)
	result
	(sum-iter (next n)
		  (+ result (term n)))))
  (sum-iter a 0))

(sum cube
     1
     (lambda (x) (+ x 1))
     3)
