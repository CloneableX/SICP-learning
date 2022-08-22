(define (count-change amount)
  (coin-change amount 5))

(define (coin-change amount kinds)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds 0)) 0)
	(else (+ (coin-change amount
			      (- kinds 1))
		 (coin-change (- amount
				 (current-coin kinds))
			      kinds)))))

(define (current-coin kinds)
  (cond ((= kinds 5) 1)
	((= kinds 4) 5)
	((= kinds 3) 10)
	((= kinds 2) 25)
	((= kinds 1) 50)))

(count-change 100)