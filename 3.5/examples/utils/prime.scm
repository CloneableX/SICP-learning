(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divises? n test-divisor) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divises? n test-divisor)
  (= (remainder n test-divisor) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

