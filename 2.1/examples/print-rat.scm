(load "rational-number.scm")

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (sub-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (div-rat one-half one-third))

(print-rat (add-rat one-third one-third))

(equal-rat? one-third one-third)