(define (max x y)
  (if (< x y)
      y
      x))

(define (assert-max x y expected)
  (= (max x y) expected))

(define (sum-max-two-nums x y z)
  (cond ((and (assert-max x y x) (assert-max y z y)) (+ x y))
	((and (assert-max x y x) (assert-max y z z)) (+ x z))
	((and (assert-max x y y) (assert-max y z z)) (+ y z))))

(sum-max-two-nums 2 3 4)
(sum-max-two-nums 2 2 2)