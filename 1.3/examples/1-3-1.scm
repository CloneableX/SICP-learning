(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 1 3)

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(sum-cubes 1 10)

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

(pi-sum 1 10)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (sum-cubes-new a b)
  (sum cube a inc b))

(equal? (sum-cubes-new 1 10) (sum-cubes 1 10))

(define (sum-integers-new a b)
  (define (identity x)
    x)
  (sum identity a inc b))

(equal? (sum-integers-new 1 10) (sum-integers 1 10))

(define (pi-sum-new a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(equal? (pi-sum-new 1 10) (pi-sum 1 10))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2)) add-dx b)))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

