
(define (stream-withdraw balance amount-stream)
  (cons-stream balance
	       (stream-withdraw (- balance (stream-car amount-stream))
				(stream-cdr amount-stream))))
   

(stream-head (stream-withdraw 100
			      (stream 5 10 20 10 5 5))
	     6)