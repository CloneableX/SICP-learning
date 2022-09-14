(define (stream-ref stream n)
  (if (= n 0)
      (stream-car stream)
      (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
		   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      the-empty-stream
      (begin (proc (stream-car s))
	     (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (define (display-line x) (newline) (display x))
  (stream-for-each display-line s))

(define ones (cons-stream 1 ones))

(stream-ref ones 10)

(stream-ref (stream-map (lambda (x) (+ x 1))
			ones)
	    10)

(stream-for-each display ones)

(display-stream ones)