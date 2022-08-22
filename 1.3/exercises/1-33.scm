(load "utils/prime.scm")

(define (accumulate combiner null-value term a next b)
  (define (accumulate-iter n result)
    (if (> n b)
	result
	(accumulate-iter (next n)
			 (combiner result (term n)))))
  (accumulate-iter a null-value))

(define (accumulate-filter combiner null-value predicate term a next b)
  (define (filter-term n)
    (if (predicate n)
	(term n)
	null-value))
  (accumulate combiner null-value filter-term a next b))

(define (prime-sum-of-square a b)
  (if (< a 2)
      (error "Value must be greater than 1" a)
      (accumulate-filter +
			 0
			 prime?
			 (lambda (x) (square x))
			 a
			 (lambda (x) (+ x 1))
			 b)))

(prime-sum-of-square 2 10)

(define (relative-prime-product n)
  (define (relative-prime? x)
    (= 1 (gcd x n)))
  (accumulate-filter * 1 relative-prime? (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(relative-prime-product 5)
(relative-prime-product 10)