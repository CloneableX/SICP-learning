(define (element-of-set? x set)
  (cond ((null? set) false)
	((< x (car set)) false)
	((= x (car set)) true)
	(else (element-of-set? x (cdr set)))))
       
; (element-of-set? 3 (list 1 2 3))       
; (element-of-set? 3 (list 1 2 4))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))
	      ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1) (cdr set2))))))))

; (intersection-set (list 1 2 3) (list 2 3 4))
; (intersection-set (list 1 2 3) (list 4 5 6))
; (intersection-set (list 1 2 3) (list 2))
