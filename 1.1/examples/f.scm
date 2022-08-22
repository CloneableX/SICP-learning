(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f x)
  (sum-of-squares (+ x 1) (* x 2)))

(f 5)







