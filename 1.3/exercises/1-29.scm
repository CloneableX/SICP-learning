(define (simpson-integral f a b n)
  (define h
    (/ (- b a) n))
  (define (term x)
    (* (factor x) (y x)))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
	  ((even? k) 2)
	  (else 4)))
  (define (y k)
    (f (+ a (* k h))))
  (define (next x)
    (+ x 1))
  (* (/ h 3)
     (sum term 0 next n)))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(simpson-integral cube 0 1 100.0)
(simpson-integral cube 0 1 1000.0)