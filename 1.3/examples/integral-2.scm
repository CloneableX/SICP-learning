(load "utils/sum.scm")

(define (integral f a b dx)
  (* (sum f 
	  (+ a (/ dx 2)) 
	  (lambda (x) (+ x dx))
	  b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
