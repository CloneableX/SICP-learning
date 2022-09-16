(load "utils/delayed-integral.scm")

(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy
    (add-streams (scale-stream dy a)
		 (scale-stream y b)))
  y)

(stream-ref (solve-2nd 10 20 1 5 0.001)
	    1000)