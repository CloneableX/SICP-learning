(define (cont-frac n d k)
  (define (recursion counter)
    (if (> counter k)
	0
	(/ (n counter) (+ (d counter) (recursion (+ counter 1))))))
  (recursion 1))

(/ 1 (cont-frac (lambda (i) 1.0)
		(lambda (i) 1.0)
		13))

(define (cont-frac-iter n d k)
  (define (iter counter result)
    (if (< counter 1)
	result
	(iter (- counter 1) (/ (n counter) (+ (d counter) result)))))
  (iter k 0))

(/ 1 (cont-frac-iter (lambda (i) 1.0)
		    (lambda (i) 1.0)
		    13))
