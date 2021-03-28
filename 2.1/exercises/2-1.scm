(load "rat.scm")

(define (make-rat n d)
  (let ((g (gcd n d))
	(n-abs (abs n))
	(d-abs (abs d)))
    (if (< (* n d) 0)
	(cons (/ (- n-abs) g) (/ d-abs g))
	(cons (/ n-abs g) (/ d-abs g)))))

(define (rat-equal? x y)
  (and (= (numer x) (numer y))
       (= (denom x) (denom y))))

(rat-equal? (make-rat -1 3)
	    (make-rat 1 -3))
(rat-equal? (make-rat -1 -3)
	    (make-rat 1 3))
