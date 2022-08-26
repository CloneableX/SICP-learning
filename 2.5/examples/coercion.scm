(define (scheme-number->complex x)
  (make-complex-from-real-imag (contents x) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
	  (apply proc (map contents args))
	  (if (= (length args) 2)
	      (let ((type1 (car type-tags))
		    (type2 (cadr type-tags)))
		(let ((type1->type2 (get-coercion type1 type2))
		      (type2->type1 (get-coercion type2 type1))
		      (a1 (car args))
		      (a2 (cadr args)))
		  (cond (type1->type2
			 (apply-generic op 
					(type1->type2 a1)
					a2))
			(type2->type1
			 (apply-generic op
					a1
					(type2->type1 a2)))
			(else
			 (error "No method for this type: APPLY-GENERIC" (list op args))))))
	      (error "No method for this type: APPLY-GENERIC" (list op args)))))))