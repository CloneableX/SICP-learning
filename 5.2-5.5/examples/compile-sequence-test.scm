(load "compiler.scm")

;;;
;;; compile-sequence
;;;

;;; it should compile evry exp in sequence
(let ((exps '(1 2 x)))
  (let ((inst-seq (compile-sequence exps 'val-n 'next)))
    (equal? (statements inst-seq)
	    '((assign val-n (const 1))
	      (assign val-n (const 2))
	      (assign val-n (op lookup-variable-value) (const x) (reg env))))))