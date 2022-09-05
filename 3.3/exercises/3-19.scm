(load "utils/make-cycle.scm")

(define (cycle-list? lst)
  (let ((low-ptr lst)
	(fast-ptr (cdr lst)))
    (define (ptr-run)
      (cond ((or (null? fast-ptr) (null? (cddr fast-ptr))) false)
	    ((eq? low-ptr fast-ptr) true)
	    (else
	     (set! low-ptr (cdr low-ptr))
	     (set! fast-ptr (cddr fast-ptr))
	     (ptr-run))))
    (ptr-run)))

(define x (list 'a 'b 'c))
(define cycle (make-cycle x))

(cycle-list? cycle)

(cycle-list? (list 'a 'b 'c))