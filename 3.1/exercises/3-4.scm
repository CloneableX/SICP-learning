(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient founds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define encrypt-dispatch
    (let ((count 0))
      (lambda (enter-password m)
	(cond ((>= count 7) (lambda (x) "Account Locked!!!"))
	      ((eq? enter-password password)
	       (begin (set! count 0)
		      (dispatch m)))
	      (else (begin (set! count (+ count 1))
			   (lambda (x) "Incorrect password")))))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
	  ((eq? m 'deposit) deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m))))
  encrypt-dispatch)

(define acc (make-account 100 '123456))

((acc '123456 'withdraw) 50)
((acc '123455 'deposit) 25)