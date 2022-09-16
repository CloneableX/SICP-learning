(load "utils/delayed-integral.scm")

(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(stream-ref (solve-2nd (lambda (x y) (+ (* 10 x) (* 20 y)))
		       1 5 0.001)
	    1000)