(define f (let ((a 0)
		(b 0))
	    (lambda (x)
	      (set! a b)
	      (set! b (+ a x))
	      a)))

(+ (f 0) (f 1))
(+ (f 1) (f 0))
