(load "fixed-point.scm")

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

((newton-transform cube) 5)

(define (newton-method g guess)
  (fixed-point (newton-transform g)
	       guess))
