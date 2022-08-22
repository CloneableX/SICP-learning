(define (iterative-improve enough improve)
  (lambda (guess)
    (if (enough guess)
	guess
	((iterative-improve enough improve) (improve guess)))))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt 4)

(define (fixed-point f first-guess)
  (define (close-enough? x)
    (< (abs (- x (f x))) 0.00001))
  ((iterative-improve close-enough? f) first-guess))

(fixed-point cos 1.0)

