(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(sum (lambda (x) x)
     1
     (lambda (x) (+ x 1))
     10)

(define (product term a next b)
  (accumulate * 1 term a next b))

(product (lambda (x) x)
	 1
	 (lambda (x) (+ x 1))
	 5)

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter n result)
    (if (> n b)
	result
	(accumulate-iter (next n)
			 (combiner result (term n)))))
  (accumulate-iter a null-value))