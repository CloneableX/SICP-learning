(define (daughter-names)
  (let ((barnacle (list 'gabrielle 'mellisa)))
    (let ((moore (list 'lorna 'mary)))
      (let ((downing (list 'mellisa (amb 'gabrielle 'lorna 'rosalind)))
	    (hall (list 'rosalind (amb 'gabrielle 'lorna))))
	(if (eq? 'gabrielle (cadr downing))
	    (set! parker (list 'mary (car downing)))
	    (set! parker (list 'mary (car hall))))
	(require
	 (distinct? (map cadr (list barnacle moore downing hall parker))))
	(list (list 'barnacle barnacle)  (list 'moore moore)
	      (list 'downing downing)    (list 'hall hall)
	      (list 'parker parker))))))