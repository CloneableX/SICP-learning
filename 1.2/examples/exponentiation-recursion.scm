(define (exp a n)
  (if (= n 0)
      1
      (* a (exp a (- n 1)))))

(exp 3 2)
(exp 2 3)