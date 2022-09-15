;; accelerate
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1))
	(s2 (stream-ref s 2)))
    (cons-stream
     (- s2
	(/ (square (- s2 s1))
	   (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))


;; super accelerate
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

