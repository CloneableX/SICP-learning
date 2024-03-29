(define (make-samephore n)
  (let ((lock (make-mutex))
	(taken 0))
    (define (samephore m)
      (cond ((eq? m 'acquire)
	     (lock 'acquire)
	     (if (< taken n)
		 (begin (set! taken (+ 1 taken))
			(lock 'release))
		 (begin (lock 'release)
			(samephore 'acquire))))
	    ((eq? m 'release)
	     (lock 'acquire)
	     (set taken (- taken 1))
	     (lock 'release))))
    samephore))