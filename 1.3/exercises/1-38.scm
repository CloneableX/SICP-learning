(load "1-37.scm")

(define (e k)
  (define (N i) 1.0)
  (define (D i)
    (if (= 0 (remainder (+ i 1) 3))
	(* 2 (/ (+ i 1) 3))
	1))
  (+ 2 (cont-frac-iter N D k)))

(e 1)
(e 2)
(e 10)
(e 100)
