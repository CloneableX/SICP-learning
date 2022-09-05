(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)
(set-to-wow! z1)
(set-to-wow! z2)

(eq? (car z1) (cdr z1))
(eq? (car z2) (cdr z2))