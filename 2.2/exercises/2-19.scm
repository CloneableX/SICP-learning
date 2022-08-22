(define (count-change amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0)
	     (no-more? coin-values))
	 0)
	(else (+ (coin-change amount
			      (except-first-denomination coin-values))
		 (coin-change (- amount (first-denomination coin-values))
			      coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define us-coins (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(count-change 100 us-coins)
(count-change 100 uk-coins)