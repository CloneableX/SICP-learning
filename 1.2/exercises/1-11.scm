(define (recursive-f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

(recursive-f 3)
(recursive-f 4)

(define (iterative-f n)
  (f-iter 2 1 0 n))

(define (f-iter x y z n)
  (if (= n 0)
      z
      (f-iter (+ (* 3 z)
		 (* 2 y)
		 x)
	      x
	      y
	      (- n 1))))

(iterative-f 3)
(iterative-f 4)