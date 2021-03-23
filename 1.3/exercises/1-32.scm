(load "sum.scm")

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

(define (identity x) x)

(define (sum a b)
  (accumulate + 0 identity a inc b))

(sum 1 10)

(define (product a b)
  (accumulate * 1 identity a inc b))

(product 1 3)


(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-iter a b)
  (accumulate-iter + 0 identity a inc b))

(sum-iter 1 10)

(define (product-iter a b)
  (accumulate-iter * 1 identity a inc b))