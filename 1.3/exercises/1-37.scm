(define (cont-frac n d k)
  (cont-frac-accumulate n d k 0))

(define (cont-frac-accumulate n d k result)
  (if (< k 1)
      result
      (cont-frac-accumulate n
			    d
			    (- k 1)
			    (/ (n k)
			       (+ (d k) result)))))

(define (cont-frac-accumulate n d k result)
  (define (accumulate-iter x accum-result)
    (if (< x 1)
	accum-result
	(accumulate-iter (- x 1)
			 (/ (n x)
			    (+ (d x) accum-result)))))
  (accumulate-iter k result))

(define (varphi x)
  (cont-frac (lambda (i) 1.0)
	     (lambda (i) 1.0)
	     x))

(varphi 1)
(varphi 2)
(varphi 3)
(varphi 4)
(varphi 5)
(varphi 6)
(varphi 7)
(varphi 8)
(varphi 9)
(varphi 10)
