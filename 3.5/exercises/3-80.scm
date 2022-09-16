(load "utils/delayed-integral.scm")

(define (rlc r l c dt)
  (lambda (vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1 c)))
    (define dil (add-streams (scale-stream vc (/ 1 l))
			     (scale-stream il (/ (* -1 r) l))))
    (stream-map cons vc il)))

(define rlc1 (rlc 1 1 0.2 0.1))
(stream-head (rlc1 10 0) 10)