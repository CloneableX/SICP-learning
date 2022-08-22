(load "2-7.scm")

(define (make-center-percent c p)
  (let ((width (* c (/ p 100.0))))
    (make-interval (- c width)
		   (+ c width))))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (percent i)
  (let ((c (center i))
	(w (/ (- (upper-bound i) (lower-bound i)) 2)))
    (* (/ w c) 100)))


(center (make-center-percent 10 11))
(percent (make-center-percent 10 11))
     