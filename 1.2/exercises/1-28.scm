(load "prime.scm")

(define (nontrivial-sqrt? a m)
  (and (not (= a 1))
       (not (= a (- m 1)))
       (= 1 (remainder (square a) m))))

(equal? #f (nontrivial-sqrt? 1 3))
(equal? #f (nontrivial-sqrt? 2 3))
(equal? #f (nontrivial-sqrt? 8 15))
(equal? #t (nontrivial-sqrt? 4 15))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((nontrivial-sqrt? base m) 0)
	((even? exp) (remainder (square (expmod base (/ exp 2) m))
				m))
	(else (remainder (* base (expmod base (- exp 1) m))
			 m))))

(equal? 1 (expmod-new 3 0 0))
(equal? 2 (expmod-new 2 3 3))
(equal? 0 (expmod-new 2 4 4))
(equal? 0 (expmod-new 4 15 15))

(define (rabin-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (non-zero-random (- n 1))))

(equal? #t (rabin-test 3))
(equal? #t (rabin-test 7))

(define (non-zero-random n)
  (let ((r (random n)))
    (if (= r 0)
	(non-zero-random n)
	r)))

(define (fast-prime? n)
  (define (iter-test times)
    (cond ((= times 0) #t)
	  ((rabin-test n) (iter-test (- times 1)))
	  (else #f)
	  ))
  (iter-test (ceiling (/ n 2))))

(equal? #t (fast-prime? 0))
(equal? #t (fast-prime? 3))

(equal? (fast-prime? 561) (prime? 561))
(equal? (fast-prime? 1105) (prime? 1105))
(equal? (fast-prime? 1729) (prime? 1729))
(equal? (fast-prime? 2465) (prime? 2465))
(equal? (fast-prime? 2821) (prime? 2821))





