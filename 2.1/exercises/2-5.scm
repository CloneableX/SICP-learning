(define (cons x y)
  (* (expt 2 x) (expt 3 y)))

(define (div-count end-enough? x n)
  (define (iter counter div-result)
    (if (end-enough? div-result)
	counter
	(iter (+ counter 1) (/ div-result n))))
  (iter 0 x))

(define (car z)
  (div-count odd? z 2))

(define (cdr z)
  (div-count (lambda (x) (< 0
			    (remainder x 3)))
	     z
	     3))

(div-count (lambda (x) (< 0
			  (remainder x 3))) 18 3)

(= 1 (car (cons 1 0)))
(= 1 (car (cons 1 3)))
(= 0 (car (cons 0 3)))
(= 0 (cdr (cons 3 0)))
(= 1 (cdr (cons 3 1)))
(= 3 (cdr (cons 3 3)))
(= 1 (cdr (cons 0 1)))