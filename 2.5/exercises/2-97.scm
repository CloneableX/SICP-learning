(load "2-96.scm")

(define (install-polynomial-package)
  (define (reduce-terms L1 L2)
    (let ((gcd-L (gcd-terms L1 L2)))
      (list (div-terms L1 gcd-L)
	    (div-terms L2 gcd-L))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(let ((reduce-result (reduce-terms (term-list p1) (term-list p2))))
	  (list (make-poly (variable p1)
			   (car reduce-result))
		(make-poly (variable p2)
			   (cadr reduce-result))))
	(error "Polys not same variable: REDUCE-POLY" (list p1 p2))))

  ;; interfaces to rest of the system
  (put 'reduce-factors '(polynomial polynomial)
       (lambda (p1 p2)
	 (let ((reduce-result (reduce-poly p1 p2)))
	   (list (tag (car reduce-result))
		 (tag (cadr reduce-result))))))
  'done)

(define (install-scheme-number-package)
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))

  ;; interfaces to rest of the system
  (put 'reduce-factors '(scheme-number scheme-number)
       (lambda (x y)
	 (let ((reduce-result (reduce-integers x y)))
	   (list (tag (car reduce-result))
		 (tag (cadr reduce-result))))))
  'done)

(define (reduce-factors n d)
  (apply-generic 'reduce-factors n d))

(define (install-rational-package)
  (define (make-rat n d)
    (let ((reduce-result (reduce-factors n d)))
      (cons (car reduce-result)
	    (cadr reduce-result))))
  'done)