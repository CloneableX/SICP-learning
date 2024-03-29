(define (make-account balance number)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'balance) balance)
	    ((eq? m 'serializer) balance-serializer)
	    ((eq? m 'number) number)
	    (else (error "Unknown request: MAKE-ACCOUNT"
			 m))))
    dispatch))


(define (deposit account amount)
  (let ((s (account 'serializer))
	(d (s (account 'deposit))))
    ((s d) amount)))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
		      (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
	(serializer2 (account2 'serializer))
	(number1 (account1 'number))
	(number2 (account2 'number)))
    (if (number1 < number2)
	((serializer2 (serializer1 exchange))
	 account1 account2)
	((serializer1 (serializer2 exchange))
	 account1 account2))))