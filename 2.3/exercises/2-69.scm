(load "utils/huffman-tree.scm")

(define (successive-merge pairs)
  (cond ((= (length pairs) 1)
	 (car pairs))
	((= (length pairs) 0)
	 '())
	(else (successive-merge
	       (adjoin-set (make-code-tree (car pairs) (cadr pairs))
			   (cddr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(successive-merge (list (make-leaf 'A 3) (make-leaf 'B 4)))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))