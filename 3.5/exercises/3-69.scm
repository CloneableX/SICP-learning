(load "utils/integers.scm")
(load "utils/interleave.scm")
(load "utils/pairs-stream.scm")

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (pair) (append (list (stream-car s)) pair))
		(stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(stream-head 	     
 (stream-filter (lambda (triple) (= (+ (square (car triple))
				       (square (cadr triple)))
				    (square (caddr triple))))
		int-triples)
 5)


