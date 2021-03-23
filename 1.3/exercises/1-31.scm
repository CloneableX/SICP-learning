(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (identity x) x)

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product identity 1 inc n))

(factorial 3)
(factorial 4)

(define (gen-factor x)
  (define numerator
    (if (even? x)
	(+ x 2)
	(+ x 1)))
  (define denominator
    (if (even? x)
	(+ x 1)
	(+ x 2)))
  (/ numerator denominator))

(define (pi-product limit)
  (product gen-factor 1.0 inc limit))

(define pi
  (* 4 (pi-product 100000)))

(+ pi 0)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial-new n)
  (product-iter identity 1 inc n))

(factorial-new 3)
(factorial-new 4)