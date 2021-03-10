(define (prime? n)
  (define (smallest-divisor)
    (find-divisor 2))
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor)
	      n) n)
	  ((divides? test-divisor) test-divisor)
	  (else (find-divisor (+ test-divisor 1)))))
  (define (divides? test-divisor)
    (= 0
       (remainder n test-divisor)))
  (= n (smallest-divisor)))

(prime? 3)
(prime? 10)

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else (remainder (* base (expmod base (- exp 1) m))
			 m))))

(fast-prime? 3 2)
(fast-prime? 10 2)

