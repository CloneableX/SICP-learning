(load "prime.scm")

(define (continue-primes n times start-time total-time)
  (cond ((= times 0) (display total-time))
	((prime? n) (continue-primes (+ n 2)
				    (- times 1)
				    (runtime)
				    (+ total-time (- (runtime) start-time))))
	(else (continue-primes (+ n 2)
			       times
			       (runtime)
			       total-time))))

(define (search-for-primes start-num times)
  (if (even? start-num)
      (continue-primes (+ start-num 1) times (runtime) 0)
      (continue-primes start-num times (runtime) 0)))

(search-for-primes 1000000 1)
(search-for-primes 10000000 1)
(search-for-primes 100000000 1)
(search-for-primes 1000000000 1)