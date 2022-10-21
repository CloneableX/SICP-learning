(load "stream-util.scm")

;;;
;;; stream-append-delayed
;;;

;;; it should append stream when stream appended is delayed
(equal? (stream->list (stream-append-delayed
		       (stream 'a)
		       (delay (stream 'b))))
	'(a b))
	

;;;
;;; flatten-stream
;;;

;;; it should change stream from multiple layers to single layer
(begin
  (let ((nested-s (cons-stream (cons-stream 'a the-empty-stream) (cons-stream 'b the-empty-stream))))
    (equal? (stream->list (flatten-stream nested-s))
	    '(a b))))


;;;
;;; stream-flatmap
;;;

;;; it should return stream-map result flatten
(begin
  (let ((actual
	 (stream-flatmap
	  (lambda (datum) (cons-stream datum the-empty-stream))
	  (list->stream '(a b c)))))
    (equal? (stream->list actual)
	    '(a b c))))


;;;
;;; singleton-stream
;;;

;;; it should return a stream with single element
(begin
  (let ((stream (singleton-stream 'a)))
    (equal? (stream->list stream)
	    '(a))))