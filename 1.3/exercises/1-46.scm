(load "1-42.scm")

(define (interative-improve good-enough? improve)
  (lambda (first-guess)
    (define (try guess)
      (let ((next (improve guess)))
	(if (good-enough? next guess)
	    next
	    (try next))))
    (try first-guess)))

(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  ((interative-improve close-enough? f) guess))

(fixed-point cos 1.0)

(define (sqrt x)
  (define (good-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((interative-improve good-enough? improve) 1.0))

(sqrt 4)
(sqrt 9)