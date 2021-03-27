(load "fixed-point.scm")

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(sqrt 4)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
	       1.0))

(cube-root 8)

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

(define (sqrt-newton x)
  (newton-method (lambda (y) (- (square y) x))
		 1.0))

(sqrt-newton 4)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g)
	       guess))

(define (sqrt-transform x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(sqrt-transform 4)

(define (sqrt-newton-transform x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))

(sqrt-newton-transform 4)
