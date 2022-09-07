(load "2-83.scm")

(define (apply-generic op . args)
  (define (map-coercion objects target-object)
    (map
     (lambda (object) (raise object (type-tag target-object)))
     objects))
  (define (iter before-ref ref after-ref)
    (let ((args (append before-ref ref after-ref)))
      (let ((type-tags (map type-tag args)))
	(let ((proc (get op type-tags)))
	  (cond (proc
		 (apply proc (map contents args)))
		((null? ref)
		 (error "No method for this type: APPLY-GENERIC" (list op args)))
		(else
		 (let ((before-coercion (map-coercion before-ref ref)))
		   (if (null? after-ref)
		       (iter (append before-coercion ref)
			     '()
			     '())
		       (iter (append before-coercion ref)
			     (list (car after-ref))
			     (cdr after-ref))))))))))
  (iter '() (list (car args)) (cdr args)))