(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

(element-of-set? 1 (list 1 2 1 3))
(element-of-set? 2 (list 1 3 3))

(define (unique-set set)
  (cond ((null? set) '())
	((element-of-set? (car set) (cdr set))
	 (unique-set (cdr set)))
	(else
	 (cons (car set) (unique-set (cdr set))))))

(define (adjoin-set x set)
  (let ((new-set (unique-set set)))
    (if (element-of-set? x new-set)
	new-set
	(cons x new-set))))

(adjoin-set 3 (list 1 2 1 3))
(adjoin-set 2 (list 1 3 3))

(define (intersection-set set1 set2)
  (let ((new-set1 (unique-set set1))
       (new-set2 (unique-set set2)))
    (cond ((null? new-set1) '())
	  ((element-of-set? (car new-set1) new-set2)
	   (cons (car new-set1) (intersection-set (cdr new-set1) new-set2)))
	  (else (intersection-set (cdr new-set1) new-set2)))))

(intersection-set (list 1 1 2 3 2) (list 4 3 3 2 4))
(intersection-set (list 1 1 2 3 2) (list 3 3 3)) 
(intersection-set (list 1 1 2 3 2) (list 4 5 5 5 4))

(define (union-set set1 set2)
  (let ((new-set1 (unique-set set1))
	(new-set2 (unique-set set2)))
    (cond ((null? new-set1) new-set2)
	  ((element-of-set? (car new-set1) new-set2)
	   (union-set (cdr new-set1) new-set2))
	  (else
	   (cons (car new-set1)
		 (union-set (cdr new-set1) new-set2))))))

(union-set (list 1 1 2 3 3 2) (list 5 5 4 5 4))
(union-set (list 1 1 2 3 3 2) (list 1 2))
(union-set (list 1 1 2 3 3 2) '())
