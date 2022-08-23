(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (install-sum-package)
  ;; interal procedures
  (define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (attach-tag '+ (cons a1 a2)))))
  (define (addend operands) (car operands))
  (define (augend operands) (cdr operands))

  ;; interface to rest of the system
  (put 'make-sum '+ make-sum)
  (put 'deriv '+
       (lambda (operands var)
	 (make-sum (deriv (addend operands) var)
		   (deriv (augend operands) var))))
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (make-product m1 m2) 
    (cond ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (attach-tag '* (cons m1 m2)))))
  (define (multiplier operands) (car operands))
  (define (multiplicand operands) (cdr operands))

  ;; interface to rest of the system
  (put 'make-product '* make-product)
  (put 'deriv '*
       (lambda (operands var)
	 (make-sum
	  (make-product (multiplier operands)
			(deriv (multiplicand operands) var))
	  (make-product (multiplicand operands)
			(deriv (multiplier operands) var)))))
  'done)

(define (install-exponentiation-package)
  ;; internal procedures
  (define (make-exponentiation base exponent) 
    (cond ((=number? exponent 1) base)
	  ((=number? exponent 0) 1)
	  ((and (number? base) (number? exponent)) (expt base exponent))
	  (else (attach-tag '** (cons base exponent)))))
  (define (base operands) (car operands))
  (define (exponent operands) (cdr operands))

  ;; interface to rest of the system
  (put 'deriv '**
       (lambda (operands var)
	 (make-product (exponent operands)
		       (make-product
			(make-exponentiation (base operands)
					     (make-sum (exponent operands) -1))
			(deriv (base operands) var)))))
  'done)

(define (make-sum a1 a2)
  ((get 'make-sum '+) a1 a2))
(define (make-product m1 m2)
  ((get 'make-product '*) m1 m2))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
	((variable? exp) (if (same-variable? exp var) 1 0))
	(else ((get 'deriv (operator exp))
	       (operands exp) var))))


(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)



