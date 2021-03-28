(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define + (lambda (m)
	    (lambda (n)
	      (lambda (f)
		(lambda (x)
		  (m f (n f x)))))))