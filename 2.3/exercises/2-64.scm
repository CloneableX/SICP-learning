(load "utils/set-tree.scm")

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-results (partial-tree elts left-size)))
	  (let ((left-tree (car left-results))
		(non-left-elts (cdr left-results))
		(right-size (- n (+ left-size 1))))
	    (let ((right-results (partial-tree (cdr non-left-elts) right-size))
		  (this-entry (car non-left-elts)))
	      (let ((right-tree (car right-results))
		    (remaining-elts (cdr right-results)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))

(define (list->tree elements)
  (partial-tree elements (length elements)))

(list->tree (list 1 3 5 7 9 11))

