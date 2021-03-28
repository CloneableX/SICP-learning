(define x (cons 1 2))

(car x)
(cdr x)

(define y (cons 3 4))

(define z (cons x y))

(car (car z))
(cdr (cdr z))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define one-half (make-rat 1 2))

(numer x)
(denom x)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat one-half)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (demon y))
     (* (denom x) (numer y))))

(define one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
(print-rat (sub-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (div-rat one-half one-third))
(equal-rat? one-half one-third)

(print-rat (make-rat 6 4))




