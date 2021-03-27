(load "1-43.scm")

(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (multiple-smooth f n)
  ((repeated smooth n) f))

((multiple-smooth square 2) 2)