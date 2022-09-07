(define (make-table) (list '*table*))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))
(define (assoc key records)
  ( cond ((null? records) false)
	 ((eq? key (caar records)) (car records))
	 (else
	  (assoc key (cdr records)))))
  
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(let ((new-record (cons key value)))
	  (set-cdr! table
		    (cons new-record (cdr table)))))))


(define t1 (make-table))
(insert! 'a 1 t1)
(lookup 'a t1)

(insert! 'a 2 t1)
(lookup 'a t1)

(insert! 'b 1 t1)
(lookup 'b t1)