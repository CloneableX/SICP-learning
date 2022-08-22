(load "2-2.scm")

(define (segment-length s)
  (point-distance (start-segment s)
		  (end-segment s)))

(define (point-distance a b)
  (sqrt (+ (square (- (x-point a)
		      (x-point b)))
	   (square (- (y-point a)
		      (y-point b))))))

(define (make-rect b l) (cons b l))
(define (breadth-rect r) (car r))
(define (length-rect r) (cdr r))

(define (breadth-value-rect r)
  (segment-length (breadth-rect r)))
(define (length-value-rect r)
  (segment-length (length-rect r)))

(define (rect-area r)
  (* (breadth-value-rect r)
     (length-value-rect r)))
(define (rect-perimeter r)
  (* (+ (breadth-value-rect r)
	(length-value-rect r))
     2))

(define a-point (make-point 2 0))
(define b-point (make-point 0 1))
(define o-point (make-point 0 0))

(define rect-a (make-rect (make-segment o-point a-point)
			  (make-segment o-point b-point)))

(rect-area rect-a)
(rect-perimeter rect-a)