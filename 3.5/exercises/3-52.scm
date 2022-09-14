(load "utils/stream-enumerate-interval.scm")

(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
	      (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
		 seq))

(define (display-stream s)
  (define (display-line x) (newline) (display x))
  (stream-for-each display-line s))

(stream-ref y 7)
(display-stream z)
