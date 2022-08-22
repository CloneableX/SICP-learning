(load "utils/set-unordered-list.scm")

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(else
	 (cons (car set1)
	       (union-set (cdr set1) set2)))))

(union-set (list 1 2 3) (list 5 4))
(union-set (list 1 2 3) (list 1))
(union-set (list 1 2 3) '())