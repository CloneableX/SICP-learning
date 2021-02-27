(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (abs a)
  (if (< a 0)
      (- a)
      a))

(define (a-plus-abs-b-ii a b)
  (+ a (abs b)))

(= (a-plus-abs-b 3 -1) (a-plus-abs-b-ii 3 -1))