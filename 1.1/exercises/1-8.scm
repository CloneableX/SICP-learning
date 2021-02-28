(load "1-7.scm")

(define (curt-iter guess x)
  (if (good-enough? guess
		    (improve guess x))
      guess
      (curt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (/ (+ (/ x
	   (square guess))
	(* 2 guess))
     3))

(define (curt x)
  (curt-iter 1.0 x))

(curt 8)
(curt 27)

