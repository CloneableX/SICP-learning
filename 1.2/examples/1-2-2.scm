(define (fib-tree n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib-tree (- n 1))
		 (fib-tree (- n 2))))))

(fib-tree 6)

(define (fib-iter n)
  (define (iter a b n)
    (if (= n 0)
	b
	(iter (+ a b)
	      a
	      (- n 1))))
  (iter 1 0 n))

(fib-iter 6)

;;;
;;;

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0)
	     (= kinds-of-coins 0))
	 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(demonination kinds-of-coins))
		     kinds-of-coins)))))

(define (demonination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 100)