(define (sum-of-lager-two-squares x y z)
  (define (sum-of-squares x y)
    (+ (square x) (square y)))
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
	((and (< y x) (< y z)) (sum-of-squares x z))
	(else (sum-of-squares x y))))

