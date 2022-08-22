(load "../examples/prime-smallest-divisor.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (false)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start)
  (if (even? start)
      (search-primes-iter (+ start 1) 3 (real-time-clock))
      (search-primes-iter start 3 (real-time-clock))))

(define (search-primes-iter num count start-time)
  (cond ((= count 0) 
	 (newline)
	 (display "Search End")
	 (newline)
	 (display "Spent ")
	 (display (- (real-time-clock) start-time)))
	((prime? num) 
	 (timed-prime-test num)
	 (search-primes-iter (+ num 2) (- count 1) start-time))
	(else (search-primes-iter (+ num 2) count start-time))))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divises? n test-divisor) test-divisor)
	(else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor test-divisor)
  (if (odd? test-divisor)
      (+ test-divisor 2)
      (+ test-divisor 1)))

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)


