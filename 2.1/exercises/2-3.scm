(load "2-2.scm")

(define (length-of-segment segment)
  (distance-of-point (start-segment segment)
		     (end-segment segment)))

(define (distance-of-point p1 p2)
  (sqrt (+ (square (- (x-point p1)
		      (x-point p2)))
	   (square (- (y-point p1)
		      (y-point p2))))))

(= 5
   (distance-of-point origin-point
		      (make-point 3 4)))

(= 5
   (length-of-segment (make-segment origin-point
				    (make-point 3 4))))

(define (make-rectangle width-segment length-segment)
  (cons width-segment length-segment))

(define (width-segment rectangle)
  (car rectangle))

(define (length-segment rectangle)
  (cdr rectangle))

(define (width-of-rectangle rectangle)
  (length-of-segment (width-segment rectangle)))

(define (length-of-rectangle rectangle)
  (length-of-segment (length-segment rectangle)))

(define (perimeter rectangle)
  (* 2
     (+ (width-of-rectangle rectangle)
	(length-of-rectangle rectangle))))

(define (area rectangle)
  (* (width-of-rectangle rectangle)
     (length-of-rectangle rectangle)))

(define (make-rectangle width length)
  (cons width length))

(define (width-of-rectangle rectangle)
  (width-segment rectangle))

(define (length-of-rectangle rectangle)
  (length-segment rectangle))

(define test-rectangle (make-rectangle (make-segment origin-point
						     (make-point 0 2))
				       (make-segment origin-point
						     (make-point 4 0))))

(define test-distance-rect (make-rectangle 2 4))

(= 2
   (width-of-rectangle test-rectangle))
(= 4 (length-of-rectangle test-rectangle))

(= 12 (perimeter test-rectangle))
(= 8 (area test-rectangle))

(= 12 (perimeter test-distance-rect))
(= 8 (area test-distance-rect))


