(define (make-monitored proc)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset-count)
      (set! count 0)
      count)
    (define (monitored-proc arg)
      (set! count (+ count 1))
      (proc arg))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
	    ((eq? m 'reset-count) (reset-count))
	    (else (monitored-proc m))))
    dispatch))

; (define s (make-monitored sqrt))
; 
; (s 100)
; (s 'how-many-calls?)
; (s 'reset-count)
