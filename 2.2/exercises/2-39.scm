(load "utils/accumulate.scm")

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(reverse (list 1 2 3))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) '() sequence))

(reverse (list 1 2 3))
