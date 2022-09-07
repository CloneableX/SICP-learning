(load "utils/rational-number.scm")

(define (make-rat n d)
  (let ((g (gcd (abs n)
		(abs d))))
    (if (positive? (/ n d))
	(cons (/ (abs n) g)
	      (/ (abs d) g))
	(cons (/ (* -1 (abs n)) g)
	      (/ (abs d) g)))))

(print-rat (make-rat -3 4))
(print-rat (make-rat 3 -4))
(print-rat (make-rat -3 -4))
(print-rat (make-rat 3 4))