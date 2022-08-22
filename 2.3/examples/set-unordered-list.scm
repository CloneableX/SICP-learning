(define (element-of-set? x set)
  (cond ((null? set) false)
	((equal? x (car set)) true)
	(else (element-of-set? x (cdr set)))))

; (element-of-set? 1 (list 1 2 3))
; (element-of-set? 2 (list 1 3))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

; (adjoin-set 3 (list 1 2 3))
; (adjoin-set 2 (list 1 3))

(define (intersection-set set1 set2)
  (cond ((null? set1) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))

; (intersection-set (list 1 2 3) (list 4 3 2))
; (intersection-set (list 1 2 3) (list 3)) 
; (intersection-set (list 1 2 3) (list 4 5))