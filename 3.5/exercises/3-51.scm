(load "utils/stream-enumerate-interval.scm")

(define (display-line x) (newline) (display x))
(define (show x)
  (display-line x)
  x)

(define x
  (stream-map show
	      (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
