(load "3-70.scm")
(load "utils/integers.scm")

(define (cube-pair-weight pair)
  (+ (cube (car pair))
     (cube (cadr pair))))
(define sum-of-cubes
  (stream-map cube-pair-weight 
	      (weighted-pairs integers integers cube-pair-weight)))

(define (ramanujan-stream s)
  (let ((next (stream-cdr s)))
    (if (= (stream-car s) (stream-car next))
	(cons-stream (stream-car s)
		     (ramanujan-stream next))
	(ramanujan-stream (stream-cdr s)))))

(define raman-numbers
  (ramanujan-stream sum-of-cubes))
(stream-head (ramanujan-stream sum-of-cubes) 5)
