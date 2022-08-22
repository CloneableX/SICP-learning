(cons 1
      (cons 2
	    (cons 3
		  (cons 4 '()))))

(list 1 2 3 4)

(define one-through-four (list 1 2 3 4))
(car one-through-four)
(cdr one-through-four)
(car (cdr one-through-four))

(cons 10 one-through-four)
(cons 5 one-through-four)