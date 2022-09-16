(load "utils/pairs-stream.scm")
(load "utils/integers.scm")
(load "utils/divisible.scm")

(define (pair-weight pair)
  (define (prime-factor? x)
    (or (divisible? x 2)
	(divisible? x 3)
	(divisible? x 5)))
  (let ((i (car pair)) (j (cadr pair)))
    (if (or (prime-factor? i) (prime-factor? j))
	(+ (* 2 i) (* 3 j) (* 5 i j))
	(+ i j))))

;; (= (pair-weight (list 7 11)) 18)  
;; (= (pair-weight (list 7 10)) 394)
;; (= (pair-weight (list 6 11)) 375)

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
	((stream-null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2))
	       (s1weight (weight (stream-car s1)))
	       (s2weight (weight (stream-car s2))))
	   (cond ((< s1weight s2weight)
		  (cons-stream
		   s1car
		   (merge-weighted (stream-cdr s1) s2 weight)))
		 (else
		  (cons-stream
		   s2car
		   (merge-weighted s1 (stream-cdr s2) weight))))))))


;; (define no-sevens (stream-filter (lambda (x) (not (divisible? x 7))) integers))
;; (define double (cons-stream 1 (stream-map (lambda (x) (* x 2)) double)))
;; (stream-head
;;   (merge-weighted no-sevens double
;;		 (lambda (x) x))
;;    10)

(define (weighted-pairs s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s1) x)) (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

(stream-head (weighted-pairs integers integers pair-weight) 10)
