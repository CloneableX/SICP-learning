(define (stream-flatmap proc stream)
  (flatten-stream (stream-map proc stream)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (interleave-delayed s1 delayed-s2)
  (cond ((stream-null? s1) (force delayed-s2))
	((stream-pair? s1)
	 (cons-stream
	  (stream-car s1)
	  (interleave-delayed
	   (force delayed-s2)
	   (delay (stream-cdr s1)))))
	(else
	 (cons-stream
	  s1
	  (force delayed-s2)))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed
	(stream-cdr s1)
	delayed-s2))))

