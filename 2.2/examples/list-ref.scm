(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref (list 1 2 3 4) 1)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length (list 1 2 3 4))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
	count
	(length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(length (list 1 2 3))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
	    (append (cdr list1) list2))))

(append (list 1 2) (list 3 4))