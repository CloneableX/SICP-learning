(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (try guess)
    (let ((next-value (f guess)))
      (if (close-enough? guess next-value)
	  next-value
	  (try next-value))))
  (try first-guess))

