(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (product (lambda (x) x)
	   1
	   (lambda (x) (+ x 1))
	   n))

(factorial 3)
(factorial 10)

(define (pi-product n)
  (define (pi-term x)
    (/ (+ x (expt 2 (remainder (+ x 1) 2)))
       (+ x (expt 2 (remainder x 2)))))
  (product pi-term
	   1
	   (lambda (x) (+ x 1))
	   n))

(* 4.0 (pi-product 100))
(* 4.0 (pi-product 1000))

(define (product term a next b)
  (define (product-iter n result)
    (if (> n b)
	result
	(product-iter (next n)
		      (* result (term n)))))
  (product-iter a 1))


