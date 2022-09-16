(load "3-70.scm")
(load "utils/integers.scm")

(define (square-pair-weight pair)
  (+ (square (car pair))
     (square (cadr pair))))
(define sum-of-squares
  (stream-map square-pair-weight 
	      (weighted-pairs integers integers square-pair-weight)))

(define (diff-way-stream s)
  (let ((next (stream-cdr s))
	(next-next (stream-cdr (stream-cdr s))))
    (if (= (stream-car s) (stream-car next) (stream-car next-next))
	(cons-stream (stream-car s)
		     (diff-way-stream next))
	(diff-way-stream next))))

(stream-head (diff-way-stream sum-of-squares) 5)


