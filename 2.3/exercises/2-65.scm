(load "utils/set-ordered-list.scm")
(load "utils/set-tree.scm")
(load "2-62.scm")

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list
			     (right-branch tree)
			     result-list)))))
  (copy-to-list tree '()))

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
  (car (partial-tree elements (length elements))))

(define (union-tree tree1 tree2)
  (let ((set1 (tree->list tree1))
	(set2 (tree->list tree2)))
    (list->tree (union-set set1 set2))))

(define (intersection-tree tree1 tree2)
  (let ((set1 (tree->list tree1))
	(set2 (tree->list tree2)))
    (list->tree (intersection-set set1 set2))))

(define a (list->tree (list 1 3 5 7 9 11)))

(union-tree a (list->tree (list 13 14 15)))
(union-tree a (list->tree (list 5 9 13)))
(union-tree a (list->tree (list 5 7 9)))


(intersection-tree a (list->tree (list 3 5 7)))
(intersection-tree a (list->tree (list 11 13 14)))
(intersection-tree a (list->tree (list 13 14 15)))


