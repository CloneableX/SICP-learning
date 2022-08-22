(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
	(append rest
		(map (lambda (subset) (cons (car s) subset)) rest)))))

(subsets (list 1 2 3))