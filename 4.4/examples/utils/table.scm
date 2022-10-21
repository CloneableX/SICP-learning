(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record (cdr record) false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
	     (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record
		   (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1 (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation: TABLE" m))))
    dispatch))



;;; Test
; (define test-table (make-table))
; ((test-table 'insert-proc!) 'job 'assertion-stream '(job (Hanks Jimmy) (computer programmer)))
; ((test-table 'lookup-proc) 'job 'assertion-stream)
; ((test-table 'insert-proc!) 'job 'assertion-stream (cons-stream '(job (Sofia Tom) (adminstration secrety)) ((test-table 'lookup-proc) 'job 'assertion-stream)))
; (stream-car ((test-table 'lookup-proc) 'job 'assertion-stream))