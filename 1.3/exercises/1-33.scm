(load "1-32.scm")
(load "prime.scm")

(define (filtered-accumulate check combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((check a) (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (iter a null-value))

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 identity a inc b))

(prime-sum 1 10)

(define (co-prime? a b)
  (= (gcd a b) 1))

(define (co-prime-product n)
  (define (co-prime-n? x)
    (co-prime? x n))
  (filtered-accumulate co-prime-n? * 1 identity 1 inc n))

(co-prime-product 3)
(co-prime-product 10)