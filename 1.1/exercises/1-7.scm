(load "../examples/1-1-7.scm)

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- old-guess new-guess))
	old-guess)))

(sqrt 0.00009)
(sqrt 90000000000000000000000000000000000000000000000000000000000000000)
