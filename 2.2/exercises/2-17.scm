(define (last-pair items)
  (cond ((null? items) items)
	((null? (cdr items)) (car items))
	(else (last-pair (cdr items)))))

(define (last-pair items)
  (let ((size (length items)))
    (if (= size 0)
	items
	(list-ref items (- size 1)))))

(last-pair (list 2 4 5 7))
(last-pair '())
