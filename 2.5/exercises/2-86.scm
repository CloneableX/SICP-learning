(define (sine x)
  (apply-generic 'sine x))
(define (cosine x)
  (apply-generic 'cosine x))

(define (make-complex-from-real-imag x y)
  (apply-generic 'make-from-real-imag x y))

(define (install-complex-number-package)
  ;; interfaces to rest of the system
  (put 'make-from-real-imag '(rational rational)
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)