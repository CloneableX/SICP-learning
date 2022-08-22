(define (fringe tree)
  (if (not (pair? tree))
      (list tree)
      (append (fringe (car tree))
	    (fringe (cadr tree)))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)