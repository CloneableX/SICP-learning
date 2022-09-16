(load "utils/monte-carlo.scm")

(define (p-stream x1 x2 y1 y2)
  (let ((center-point (center x1 x2 y1 y2))
	(r (radius x1 x2 y1 y2))
	(points (random-points x1 x2 y1 y2)))
    (define (point-in-square? point)
      (<= (point-distance-square center-point point) (square r)))
    (stream-map point-in-square? points)))

(define (random-stream-in-range low high)
  (cons-stream (+ low (random (- high low)))
	       (random-stream-in-range low high)))

(define (make-point x y) (cons x y))
(define (x-coord point) (car point))
(define (y-coord point) (cdr point))

(define (point-distance-square p1 p2)
  (+ (square (- (x-coord p2) (x-coord p1)))
     (square (- (y-coord p2) (y-coord p1)))))
(define (random-points x1 x2 y1 y2)
  (stream-map make-point
	      (random-stream-in-range x1 x2)
	      (random-stream-in-range y1 y2)))


(define (center x1 x2 y1 y2)
  (make-point (- x2 x1) (- y2 y1)))
(define (radius x1 x2 y1 y2)
  (/ (min (- x2 x1) (- y2 y1)) 2))


(define (estimate-integrals x1 x2 y1 y2)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (stream-map (lambda (p) (* p area))
		(monte-carlo (p-stream x1 x2 y1 y2) 0 0))))
(define estimate-integral-results (estimate-integrals 2 8 4 10))

(stream-head estimate-integral-results 10)





