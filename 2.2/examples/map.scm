(define (map proc items)
  (if (null? items)
      items
      (cons (proc (car items))
	    (map proc (cdr items)))))

(map abs (list -1 2 3 -4 -3))
(map (lambda (x) (* x x)) (list 1 2 3 4 5))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(scale-list (list 1 2 3 4 5) 10)