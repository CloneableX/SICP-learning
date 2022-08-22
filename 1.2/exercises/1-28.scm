(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((nontrivial-sqrt? base m) 0)
	((even? exp)
	 (remainder (square 
		     (expmod base (/ exp 2) m))
		    m))
	(else (remainder (* base
			    (expmod base (- exp 1) m))
			 m))))

(define (nontrivial-sqrt? a n)
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= (remainder (square a) n)
	  1)))

(define (mr-prime? n)
  (mr-prime-iter n (ceiling (/ n 2))))

(define (mr-prime-iter n times)
  (cond ((= 0 times) true)
	((= 0
	 (expmod (non-zero-random n)
		 n
		 n)) false)
	(else (mr-prime-iter n (- times 1)))))

(define (non-zero-random n)
  (+ 1 (random (- n 1))))

(mr-prime? 561)
(mr-prime? 1105)




