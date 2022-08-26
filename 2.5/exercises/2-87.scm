(define (install-polynomial-package)
  (define (=zero? p)
    (empty-termlist? (term-list p)))

  (put '=zero? '(polynomial) =zero?)
  'done)