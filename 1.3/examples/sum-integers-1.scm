(define (sum-integers a b)
  (if (> a b)
      0
      (+ a
	 (sum-integers (+ a 1) b))))

(sum-integers 1 10)