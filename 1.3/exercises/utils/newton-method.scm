(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (try guess)
    (let ((next-value (f guess)))
      (if (close-enough? guess next-value)
	  next-value
	  (try next-value))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
	  (g x))
       dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

