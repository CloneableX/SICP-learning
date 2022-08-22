(load "utils/average-damp.scm")
(load "utils/fixed-point.scm")

(define (compose g f)
  (lambda (x) (g (f x))))

(define (repeated f times)
  (define (repeated-iter func count)
    (if (= count 0)
	func
	(repeated-iter (compose f func) (- count 1))))
  (repeated-iter f (- times 1)))

(define (repeated-damp f times)
  ((repeated average-damp times) f))

(define (lg n)
  (cond ((> (/ n 2) 1) (+ 1 (lg (/ n 2))))
	((< (/ n 2) 1) 0)
	(else 1)))

(define (nth-root x n)
  (fixed-point (repeated-damp (lambda (y) (/ x (expt y (- n 1))))
			      (lg n))
	       1.0))

(define (sqrt x)
  (nth-root x 2))

(sqrt 4)
(sqrt 9)

(define (cube-root x)
  (nth-root x 3))

(cube-root 27)
(cube-root 8)

(define (forth-root x)
  (nth-root x 4))

(forth-root 16)

(define (fifth-root x)
  (nth-root x 5))

(fifth-root 32)