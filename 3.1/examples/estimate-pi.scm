(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials expriment)
  (define (iter remaining-trials passing-trials)
    (cond ((= remaining-trials 0)
	   (/ passing-trials trials))
	  ((expriment)
	   (iter (- remaining-trials 1)
		 (+ passing-trials 1)))
	  (else
	   (iter (- remaining-trials 1)
		 passing-trials))))
  (iter tirals 0))



(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter remaining-trials passed-trials x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x2)))
	(cond ((= remaining-trials 0)
	       (/ passed-trials trials))
	      ((= (gcd x1 x2) 1)
	       (iter (- remaining-trials 1)
		     (+ passed-trials 1)
		     x2))
	      (else
	       (iter (- remaining-trials 1)
		     passed-trials
		     x2))))))
  (iter trials 0 initial-x))