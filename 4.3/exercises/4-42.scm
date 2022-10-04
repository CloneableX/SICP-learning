(define (degree-examination)
  (let ((betty (amb 1 3)) (ethel (amb 1 5))
	(joan  (amb 2 3)) (kitty (amb 2))
	(mary  (amb 4)))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)  (list 'ethel ethel)
	  (list 'joan joan)    (list 'kitty kitty)
	  (list 'mary mary))))