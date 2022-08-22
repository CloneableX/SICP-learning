(define (exp a n)
  (exp-iter a 1 n))

(define (exp-iter a product count)
  (if (= count 0)
      product
      (exp-iter a
		(* a product)
		(- count 1))))

(exp 3 2)
(exp 2 3)