(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
	guess
	(sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess
	     (/ guess x)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (> 0.001
       (abs (- (square guess)
	       x))))
  (sqrt-iter 1.0))

(sqrt 3)

