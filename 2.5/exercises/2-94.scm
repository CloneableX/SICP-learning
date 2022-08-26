(define (install-polynomial-package)
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
	L1
	(gcd-terms L2 (remainder-terms L1 L2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (gcd-terms (term-list p1) (term-list p2)))
	(error "Polys not same variable: GCD-POLY" (list p1 p2))))

  ;; interfaces to rest of the system
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))