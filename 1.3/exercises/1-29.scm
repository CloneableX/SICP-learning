(load "utils/sum.scm")

(define (simpson-integral f a b n)
  (define (factor x)
    (cond ((or (= x 0) (= x n)) 1)
	  ((even? x) 2)
	  (else 4)))
  (let ((h (/ (- b a) n)))
    (define (integral-term x)
      (* (factor x) (f (+ a (* x h)))))
    (* (sum integral-term
	 0
	 (lambda (x) (+ x 1))
	 n)
       (/ h 3.0))))

(simpson-integral cube 0 1 100)
(simpson-integral cube 0 1 1000)
