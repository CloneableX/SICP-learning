(define x 10)
(parallel-execute
 (lambda () (* x x))
 (lambda () (+ x 1)))

(define s (make-serializer))
(parallel-execute
 (s (lambda () (* x x)))
 (s (lambda () (+ x 1))))