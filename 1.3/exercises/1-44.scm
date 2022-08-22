(define (compose g f)
  (lambda (x) (g (f x))))

(define (repeated f times)
  (define (repeated-iter func count)
    (if (= count 0)
	func
	(repeated-iter (compose f func) (- count 1))))
  (repeated-iter f (- times 1)))

(define dx 0.0001)
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
		    (f x)
		    (f (+ x dx)))
		 3)))

(define (fold-smooth f times)
  ((repeated smooth times) f))

((fold-smooth square 3) 2)