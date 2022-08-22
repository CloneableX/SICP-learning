(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))


(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (list '+ a1 a2))))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))


(define (make-product m1 m2) 
  (cond ((=number? m1 1) m2)
	((=number? m2 1) m1)
	((or (=number? m1 0) (=number? m2 0)) 0)
	((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))


(define (make-exponentiation base exponent) 
  (cond ((=number? exponent 1) base)
	((=number? exponent 0) 1)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list '** base exponent))))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (deriv exp var)
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
	(else (error "unknown expression type: DERIV" exp))))


; (deriv '(** x 3) 'x)
; (deriv '(** x 2) 'x)
; (deriv '(** x 1) 'x)
; (make-exponentiation 3 4)
