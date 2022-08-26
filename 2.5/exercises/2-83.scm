(define (raise object target-type)
  (let ((current-type (type-tag object)))
    (if (same-type? current-type target-type)
	object
	(let ((proc (get-coercion current-type target-type))
	      (super-proc (get-coercion current-type (super-type object))))
	  (cond (proc
		 (proc object))
		(super-proc
		 (raise (super-proc object) target-type))
		(else
		 (error "No coercion for this type: RAISE" (list object target-type))))))))