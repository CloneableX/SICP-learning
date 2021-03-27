(define (average a b)
  (/ (+ a b) 2.0))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point))
	(test-value (f (average neg-point pos-point))))
    (cond ((close-enough? neg-point pos-point) midpoint)
	  ((positive? test-value) (search f neg-point midpoint))
	  ((negative? test-value) (search f midpoint pos-point))
	  (else midpoint))))

(= 0 (search (lambda (x) x) -1 1))
(= 0 (search (lambda (x) x) -1 3))
(= 0 (search (lambda (x) x) -3 1))
(= (average 1 1.0009) (search (lambda (x) x) 1 1.0009))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
	  ((and (positive? a-value) (negative? b-value)) (search f b a))
	  (else (error "Values are not of opposite sign" a b)))))

(= 0 (half-interval-method (lambda (x) x) -1 1))
(= 0 (half-interval-method (lambda (x) x) 1 -1))
;(half-interval-method (lambda (x) x) 1 3) => error


(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
	  next
	  (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
  (define (average x y)
    (/ (+ x y) 2))
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 4)
(sqrt 3)