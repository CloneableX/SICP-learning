(load "1-42.scm")

(define (repeated f n)
  (define (iter counter result)
    (if (= counter n)
	result
	(iter (+ counter 1) (compose f result))))
  (iter 1 f))

((repeated square 2) 5)