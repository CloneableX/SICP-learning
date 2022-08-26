(define (=zero? x)
  ((apply-generic '=zero? x) x))

(define (install-scheme-number-package)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-number-package)
  ;; internal procedures
  (define (=zero? x) (= (numer x) 0))

  ;; interfaces to rest of the system
  (put '=zero? '(rational) =zero?)
  'done)

(define (install-complex-number-package)
  ;; internal procedures
  (define (=zero? x) (and (= (real-part x) 0)
			  (= (iamg-part x) 0)))

  ;; interfaces to rest of the system
  (put '=zero? '(complex) =zero?)
  'done)