(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (predicate-count x predicate next)
  (define (count-iter x count)
    (if (predicate x)
	(count-iter (next x) (+ count 1))
	count))
  (count-iter x 0))

(define (car z)
  (predicate-count z 
		   (lambda (x) (= (remainder x 2) 0))
		   (lambda (x) (/ x 2))))
(define (cdr z)
  (predicate-count z
		   (lambda (x) (= (remainder x 3) 0))
		   (lambda (x) (/ x 3))))

(car (cons 3 4))
(cdr (cons 3 4))