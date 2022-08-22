(load "utils/accumulate.scm")

(define (count-leaves tree)
  (accumulate + 0
	      (map (lambda (subtree)
		     (if (pair? subtree)
			 (count-leaves subtree)
			 1))
		   tree)))

(count-leaves (list 1 (list 2 (list 3 4) 5) (list 6 7)))