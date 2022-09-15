(load "utils/prime.scm")
(load "utils/integers.scm")

(define (append-streams s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (append-streams (stream-cdr s1) s2))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
		   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
;  (append-streams				
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
		(stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers (stream-cdr integers)))

(stream-head
 (stream-filter 
  (lambda (pair) (prime? (+ (car pair) (cadr pair))))
  int-pairs)
 8)
	       