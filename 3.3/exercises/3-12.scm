(define x (list 'a 'b))
(define y (list 'c 'd))

(define z (append x y))
(display z)
(cdr x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define w (append! x y))
(display w)
(cdr x)