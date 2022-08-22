(load "utils/cont-frac.scm")

(define (d i)
  (if (= (remainder (+ i 1) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

(define (e x)
  (+ 2
     (cont-frac (lambda (i) 1.0)
		d
		x)))

(e 2)
(e 5)
(e 10)
(e 100)
