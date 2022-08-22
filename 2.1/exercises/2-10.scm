(load "2-7.scm")
(load "2-8.scm")

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(print-interval (mul-interval (make-interval 2 3)
			      (make-interval -1 2)))

(define (span-zero? interval)
  (let ((lower (lower-bound interval))
	(upper (upper-bound interval)))
    (or (<= (* lower upper) 0)
	(= lower upper))))

(define (div-interval x y)
  (if (span-zero? y)
      (error "Division Error (interval span 0)" y)
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
		      (/ 1.0 (lower-bound y))))))

(print-interval (div-interval (make-interval 2 3)
			      (make-interval 1 2)))

(print-interval (div-interval (make-interval 2 3)
			      (make-interval -2 2)))
(print-interval (div-interval (make-interval 2 3)
			      (make-interval 2 2)))