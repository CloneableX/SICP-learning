(define rand (let ((x random-init))
	       (lambda ()
		 (set! x (rand-update x))
		 x)))