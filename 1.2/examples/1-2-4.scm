(define (expt-recur b n)
  (if (= n 0)
      1
      (* b (expt-recur b (- n 1)))))

(expt-recur 3 4)

(define (expt-iter b n)
  (define (iter counter product)
    (if (= counter 0)
	product
	(iter (- counter 1) (* b product))))
  (iter n 1))

(expt-iter 3 4)

(define (expt-recur-fast b n)
  (define (even?)
    (= (remainder n 2) 0))
  (cond ((= n 0) 1)
	((even?) (square (expt-recur-fast b (/ n 2))))
	(else (* b (expt-recur-fast b (- n 1))))))

(expt-recur-fast 3 5)
