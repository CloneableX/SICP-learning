(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment s)
  (average-point (start-segment s)
		 (end-segment s)))

(define (average-point a b)
  (make-point (average (x-point a) (x-point b))
	      (average (y-point a) (y-point b))))

(define (average x y)
  (/ (+ x y) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; (define line-segment (make-segment (make-point 1 2)
;				   (make-point 3 2)))
; (print-point (midpoint-segment line-segment))