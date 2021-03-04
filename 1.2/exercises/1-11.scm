(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
	 (* 2
	    (f-recur (- n 2)))
	 (* 3
	    (f-recur (- n 3))))))

(f-recur 5)

(define (f-iter n)
  (define (iter a b c max-counter)
    (cond ((< max-counter 0) (+ max-counter 2))
	  ((= max-counter 0) c)
	  (else (iter b
		      c
		      (+ (* 3 a)
			 (* 2 b)
			 c)
		      (- max-counter 1)))))
  (iter 0
	1
	2
	(- n 2)))

(f-iter 5)