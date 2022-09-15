(load "utils/interleave.scm")
(load "utils/integers.scm")

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (interleave
     (stream-map (lambda (x) (list x (stream-car t)))
		 (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))

(define int-pairs (pairs integers integers))

(stream-head int-pairs 10)
