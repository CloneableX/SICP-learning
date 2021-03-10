(load "1-22.scm")

(define (continue-primes n times start-time total-time)
  (cond ((= times 0) (display total-time))
	((fast-prime? n 3) (continue-primes (+ n 2)
				    (- times 1)
				    (runtime)
				    (+ total-time (- (runtime) start-time))))
	(else (continue-primes (+ n 2)
			       times
			       (runtime)
			       total-time))))

(search-for-primes 1000000 1)
(search-for-primes 10000000 1)
(search-for-primes 100000000 1)
(search-for-primes 1000000000 1)