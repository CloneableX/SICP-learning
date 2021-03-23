(load "sum.scm")

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ result (term a)))))
  (iter a 0))

(sum-cubes-new 1 10)
(sum-integers-new 1 10)