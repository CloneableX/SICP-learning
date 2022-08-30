(define (estimate-integral trials x1 x2 y1 y2 P)
  (define (area)
    (* (- x2 x1) (- y2 y1)))
  (* (area) (monte-carlo trials (P x1 x2 y1 y2))))

(define (monte-carlo trials experiment)
  (define (iter trials-remained trials-passed)
    (cond ((= trials-remained 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remained 1)
		 (+ trials-passed 1)))
	  (else
	   (iter (- trials-remained 1)
		 trials-passed))))
  (iter trials 0))

(define (P x1 x2 y1 y2)
  (lambda ()
    (let ((center-point (center x1 x2 y1 y2))
	  (r (radius x1 x2 y1 y2))
	  (point (random-point x1 x2 y1 y2)))
      (<= (point-distance-square center-point point)
	  (square r)))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (make-point x y) (cons x y))
(define (x-coord point) (car point))
(define (y-coord point) (cdr point))

(define (point-distance-square p1 p2)
  (+ (square (- (x-coord p2) (x-coord p1)))
     (square (- (y-coord p2) (y-coord p1)))))
(define (random-point x1 x2 y1 y2)
  (make-point (random-in-range x1 x2)
	      (random-in-range y1 y2)))

(define (center x1 x2 y1 y2)
  (make-point (- x2 x1) (- y2 y1)))
(define (radius x1 x2 y1 y2)
  (/ (min (- x2 x1) (- y2 y1)) 2))

(estimate-integral 15 2 8 4 10 P)





