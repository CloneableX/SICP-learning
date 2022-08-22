(load "prime-sum-pairs.scm")

(define (remove item s)
  (filter (lambda (x) (not (= item x))) s))

(define (permutations s)
  (if (null? s)
      (list '())
      (flat-map (lambda (x)
		  (map (lambda (p) (cons x p))
		       (permutations (remove x s))))
		s)))

(permutations (list 1 2 3))