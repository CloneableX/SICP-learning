(load "utils/integers-starting-from.scm")
(load "utils/divisible.scm")

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers
    (cons-stream 1
	       (add-streams ones integers)))

(stream-ref integers 10)



(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(stream-ref fibs 10)

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor))
	      s))
(define double (cons-stream 1 (scale-stream double 2)))

(stream-ref double 10)

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
	  ((divisible? n (stream-car ps)) false)
	  (else (iter (stream-cdr ps)))))
  (iter primes))
(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(stream-ref primes 50)