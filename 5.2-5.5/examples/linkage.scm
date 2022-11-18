(load "compile-combiners.scm")

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'next) (empty-instruction-sequence))
	((eq? linkage 'return)
	 (make-instruction-sequence
	  '(continue) '()
	  '((goto (reg continue)))))
	((symbol? linkage)
	 (make-instruction-sequence
	  '() '()
	  `((goto (label ,linkage)))))))
