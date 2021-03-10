(load "1-22.scm")

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor)
	    n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor-new n (next test-divisor)))))

(define (next n)
  (if (even? n)
      (+ n 1)
      (+ n 2)))

(search-for-primes 1000000 1)
(search-for-primes 10000000 1)
(search-for-primes 100000000 1)
(search-for-primes 1000000000 1)

