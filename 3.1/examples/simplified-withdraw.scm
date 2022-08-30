(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define W (make-simplified-withdraw 100))
(W 10)


(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

(define D (make-decrementer 100))
(D 10)
