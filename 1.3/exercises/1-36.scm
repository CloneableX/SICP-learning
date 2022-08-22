(load "utils/average-damp.scm")

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (try guess)
    (newline)
    (display "Guess is ")
    (display guess)
    (let ((next-value (f guess)))
      (if (close-enough? guess next-value)
	  next-value
	  (try next-value))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x)))
	     2.0)
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x))))
	     2.0)
	