(load "2-56.scm")

(define (ordered-sum a1 a2)
      (list a1 '+ a2))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list a1 '+ a2))))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))


(define (make-product m1 m2) 
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (order-operation exp)
  (if (and (sum? exp) (memq '* exp) (memq '+ exp))
      (make-sum (cddr exp) (car exp))
      exp))

(define (deriv exp var)
  (let ((exp (order-operation exp)))
    (cond ((number? exp) 0)
	  ((variable? exp) (if (same-variable? exp var) 1 0))
	  ((sum? exp) (make-sum (deriv (addend exp) var)
				(deriv (augend exp) var)))
	  ((product? exp)
	   (make-sum
	    (make-product (multiplier exp)
			  (deriv (multiplicand exp) var))
	    (make-product (multiplicand exp)
			  (deriv (multiplier exp) var))))
	  ((exponentiation? exp)
	   (make-product (exponent exp)
			 (make-product
			  (make-exponentiation (base exp)
					       (make-sum (exponent exp) -1))
			  (deriv (base exp) var))))
	  (else (error "unknown expression type: DERIV" exp)))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * x) 'x)
(deriv '(x + (3 * x)) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)
(deriv '(x + y + 3 * (x + y + 2)) 'x)


