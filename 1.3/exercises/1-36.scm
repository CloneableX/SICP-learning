(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try guess)
    (display "fixed point: ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? next guess)
	  next
	  (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2.0))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) (average (/ (log 1000) (log x)) x)) 2.0)