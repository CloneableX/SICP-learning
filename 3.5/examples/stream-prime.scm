(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
		   (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
	((pred (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred (stream-cdr s))))
	(else (stream-filter pred (stream-cdr s)))))

(stream-ref (stream-enumerate-interval 10 1000) 50)

(stream-ref (stream-filter even? (stream-enumerate-interval 11 100)) 7)

(stream-car
 (stream-cdr
  (stream-filter prime?
		 (stream-enumerate-interval
		  10000 1000000))))