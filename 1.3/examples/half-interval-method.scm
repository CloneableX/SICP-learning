(load "utils/average.scm")

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
	midpoint
	(let ((test-value (f midpoint)))
	  (cond ((> (f midpoint) 0) (search f neg-point midpoint))
		((< (f midpoint) 0) (search f midpoint pos-point))
		(else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
	(b-value (f b)))
    (cond ((and (positive? a-value) (negative? b-value)) (search f b a))
	  ((and (positive? b-value) (negative? a-value)) (search f a b))
	  (else
	   (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 4.0 2.0)
(half-interval-method (lambda (x)
			(- (cube x)
			   (* 2 x)
			   3))
		      1.0
		      2.0)