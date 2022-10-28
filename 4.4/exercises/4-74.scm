(define (simple-flatten stream)
  (stream-map
   (lambda (frame-stream)
     (stream-car frame-stream))
   (stream-filter
    (lambda (frame-stream) (not (stream-null? frame-stream)))
    stream)))

;;; it should flatten nexted stream
(let ((nested-stream (stream 
		      (stream 'a)
		      the-empty-stream
		      (stream 'c))))
  (equal? (stream->list (simple-flatten nested-stream))
	  '(a c)))


;;;
;;;
;;;
(define (simple-stream-flatmap proc stream)
  (simple-flatten (stream-map proc stream)))

;;; it should map stream and flat it
(let ((s (stream 'a 'b 'c)))
  (let ((result-stream (simple-stream-flatmap
		       (lambda (x) (stream x))
		       s)))
    (equal? (stream->list result-stream)
	    '(a b c))))