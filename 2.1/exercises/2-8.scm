(load "2-7.scm")

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
		    (upper-bound y))
		 (- (upper-bound x)
		    (lower-bound y))))

(define (print-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))

(define x (make-interval 2 3))
(define y (make-interval 4 5))

(print-interval (sub-interval x y))
(print-interval (sub-interval y x))
