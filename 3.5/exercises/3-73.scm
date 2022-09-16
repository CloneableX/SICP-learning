(load "utils/integral.scm")
(load "utils/integers.scm")

(define (rc r c dt)
  (lambda (s v0)
    (add-streams
     (scale-stream s r)
     (integral (scale-stream s (/ 1 c))
	       v0 dt))))

(define rc1 (rc 5 1 0.5))

(stream-head (rc1 integers 10) 10)