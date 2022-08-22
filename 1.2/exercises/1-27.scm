(define (fast-prime? n times)
  (cond ((= times 0) true)
	((and (prime? n) (strict-prime? n)) (fast-prime? n (- times 1)))
	(else false)))

(define (prime? n)
  (define (fermat-test n a)
    (= (expmod a n n) a))
  (fermat-test n (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((= (remainder exp 2) 0)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else (remainder (* base (expmod base (- exp 1) m))
			 m))))

(define (strict-prime? n)
  (strict-prime-test n 2))

(define (strict-prime-test n a)
  (cond ((or (> a n) (= a n)) true)
	((= (expmod a n n) a) (strict-prime-test n (+ a 1)))
	(else false)))

(fast-prime? 561 12)
(fast-prime? 1105 12)