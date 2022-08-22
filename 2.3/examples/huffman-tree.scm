(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

(define (leaf? tree) (eq? (car tree) 'leaf))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit tree)
  (cond ((= bit 0) (left-branch tree))
	((= bit 1) (right-branch tree))
	(else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair) (cadr pair))
		    (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(else
	 (cons (car set) (adjoin-set x (cdr set))))))


(define a (make-code-tree (make-leaf 'A 3)
			  (make-leaf 'B 1)))
(define b (make-code-tree a (make-leaf 'C 1)))

(decode '(0 0 0 1 1) b)

(make-leaf-set '((B 2) (C 1) (D 1) (A 4)))
