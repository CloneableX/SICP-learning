(load "utils/accumulate.scm")

(define (map p sequence)
  (accumulate (lambda (item subsequence) (cons (p item) subsequence)) '() sequence))

(map square (list 1 2 3 4))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append (list 1 2 3) (list 4 5))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(length (list 3 4))