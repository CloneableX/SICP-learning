(load "utils/huffman-tree.scm")

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
	 (if (eq? (symbol-leaf tree) symbol)
	     '()
	     (error "bad symbol, ENCODE-SYMBOL" symbol)))
	((memq symbol (symbols (left-branch tree)))
	 (cons 0 (encode-symbol symbol (left-branch tree))))
	((memq symbol (symbols (right-branch tree)))
	 (cons 1 (encode-symbol symbol (right-branch tree))))
	(else
	 (error "bad symbol, ENCODE-SYMBOL" symbol))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree
		    (make-leaf 'D 1)
		    (make-leaf 'C 1)))))

(encode '(A D A B B C A) sample-tree)

