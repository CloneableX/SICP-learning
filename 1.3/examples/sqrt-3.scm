(load "fixed-point.scm")

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

(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x))
		 1.0))

(sqrt 4)


(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

(sqrt 4)