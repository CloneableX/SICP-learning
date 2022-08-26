(define (install-scheme-number-package)
  (put 'equ? '(shceme-number scheme-number)
       (lambda (x y) (= x y)))
  'done)

(define (install-rational-number-package)
  ;; internal-procedures
  (define (rat-equal? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

  ;; interfaces to rest of the system
  (put 'equ? '(rational rational) rat-equal?)
  'done)

(define (install-complex-number-package)
  ;; internal procedures
  (define (complex-equal? z1 z2)
    (and (= (real-part z1) (real-part z2))
	 (= (imag-part z1) (imag-part z2))))

  ;; interfaces to rest of the system
  (put 'equ? '(complex complex) complex-equal?)
  'done)