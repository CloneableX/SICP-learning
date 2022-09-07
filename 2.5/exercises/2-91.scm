(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
	    (t2 (first-term L2)))
	(if (> (order t2) (order t1))
	    (list (the-empty-termlist) L1)
	    (let ((new-c (div (coeff t1) (coeff t2)))
		  (new-o (- (order t1) (order t2))))
	      (let ((result-term (make-term new-o new-c)))
		(let ((new-L1 (sub-poly L1 (mul-term-by-all-terms result-term L2))))
		  (let ((rest-of-result (div-terms new-L1 L2)))
		    (list (adjoin-term result-term car rest-of-result)
			  (cadr rest-of-result))))))))))