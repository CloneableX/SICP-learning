(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))


(define ones (cons-stream 1 ones))
(define twos (cons-stream 2 twos))

(stream-ref (stream-map + ones twos) 10)