(define (make-record key name) (list key name))
(define (key record) (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (car set-of-records)))
	 (car set-of-records))
	(else (lookup given-key (cdr set-of-records)))))

; (lookup 3
; 	(list (make-record 1 'Jeff)
; 	      (make-record 2 'John)
; 	      (make-record 3 'Tim)))
