(load "utils/add-streams.scm")
(load "utils/integers.scm")

(define (make-zero-crossings input-stream smooth-proc)
  (let ((smooth-stream (smooth-proc input-stream)))
    (stream-map sign-change-detector
		smooth-stream
		(cons-stream 0 smooth-stream))))

(define (smooth s)
  (stream-map
   (lambda (x) (/ x 2))
   (add-streams s (cons-stream 0 s))))

; (define zero-crossings (make-zero-crossings sense-data smooth))
