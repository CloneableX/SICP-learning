(load "2-36.scm")

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(define matrix (list (list 1 2 3 4)
		     (list 4 5 6 6)
		     (list 6 7 8 9)))
(define vector (list 1 2 3 4))

(dot-product vector (list 4 5 6 6))
(matrix-*-vector matrix vector)
(transpose matrix)
(matrix-*-matrix (list (list 1 0 2) (list -1 3 1))
		 (list (list 3 1) (list 2 1) (list 1 0)))
