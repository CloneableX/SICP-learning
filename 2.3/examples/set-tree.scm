(define (make-tree entry left-branch right-branch)
  (list entry left-branch right-branch))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define a
  (make-tree 7
	     (make-tree 3
			(make-tree 1 '() '())
			(make-tree 5 '() '()))
	     (make-tree 9
			'()
			(make-tree 11 '() '()))))
(entry a)
(left-branch a)
(right-branch a)


(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	(else (element-of-set? x (right-branch set)))))

(element-of-set? 1 a)
(element-of-set? 9 a)
(element-of-set? 10 a)


(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(adjoin-set 4 a)
(adjoin-set 9 a)