(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (print-point point)
  (newline)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

(define origin-point (make-point 0 0))

(print-point origin-point)
(print-point (make-point 1 3))

(define (midpoint-segment segment)
  (div-point (sum-point (start-segment segment)
			(end-segment segment))
	     2))

(define (div-point point n)
  (make-point (/ (x-point point) n)
	      (/ (y-point point) n)))

(define (sum-point p1 p2)
  (make-point (+ (x-point p1) (x-point p2))
	      (+ (y-point p1) (y-point p2))))

(define (equal-point? p1 p2)
  (and (= (x-point p1) (x-point p2))
       (= (y-point p1) (y-point p2))))

(equal-point? origin-point origin-point)
(equal-point? (make-point 1 1)
	      (div-point (make-point 2 2)
			 2))
(equal-point? (make-point 3 3)
	      (sum-point (make-point 1 2)
			 (make-point 2 1)))

(equal-point? (midpoint-segment (make-segment origin-point
				   (make-point 2 0)))
   (make-point 1 0))
(equal-point? (midpoint-segment (make-segment origin-point
					      (make-point 2 2)))
	      (make-point 1 1))