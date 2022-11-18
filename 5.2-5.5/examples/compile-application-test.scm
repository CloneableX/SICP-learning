(load "compiler.scm")

;;;
;;; compile-application
;;;

;;; it should compile primitive procedure expression
(let ((inst-seq (compile-application '(+ 1 3) 'val 'next)))
  (equal? (statements inst-seq)
	  '((assign proc (op lookup-variable-value) (const +) (reg env))
	    (assign val (const 3))
	    (assign argl (op list) (reg val))
	    (assign val (const 1))
	    (assign argl (op cons) (reg val) (reg argl))
	    (assign val (op apply-primitive-procedure) (reg proc) (reg argl)))))

;;; it should preserve register 'proc'
(let ((inst-seq (compile-application '(+ (+ 1 2) 3) 'val 'next)))
  (equal? (statements inst-seq)
	  '((assign proc (op lookup-variable-value) (const +) (reg env))
	    (save proc)
	    (assign val (const 3))
	    (assign argl (op list) (reg val))
	    (save argl)
	    (assign proc (op lookup-variable-value) (const +) (reg env))
	    (assign val (const 2))
	    (assign argl (op list) (reg val))
	    (assign val (const 1))
	    (assign argl (op cons) (reg val) (reg argl))
	    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	    (restore argl)
	    (assign argl (op cons) (reg val) (reg argl))
	    (restore proc)
	    (assign val (op apply-primitive-procedure) (reg proc) (reg argl)))))

;;;
;;; construct-arglist
;;;

;;; it should return empty arguments list without operands
(let ((inst-seq (construct-arglist '())))
  (equal? inst-seq
	  (list '() '(argl)
	   '((assign argl (const ()))))))

;;; it should return only one element argument list when there is one operand
(let ((operand-code (compile '3 'val 'next)))
  (let ((inst-seq (construct-arglist (list operand-code))))
    (equal? (statements inst-seq)
	    '((assign val (const 3))
	      (assign argl (op list) (reg val))))))

;;; it should return reverse cons operand codes
(let ((operand-codes
       (list (compile '1 'val 'next)
	     (compile '3 'val 'next))))
  (let ((inst-seq (construct-arglist operand-codes)))
    (equal? (statements inst-seq)
	    '((assign val (const 3))
	      (assign argl (op list) (reg val))
	      (assign val (const 1))
	      (assign argl (op cons) (reg val) (reg argl))))))

;;; it should preserv registers argl if argl is used
(let ((seq1 (compile '(+ 1 3) 'val 'next))
      (seq2 (compile '3 'val 'next)))
  (let ((inst-seq (construct-arglist (list seq1 seq2))))
    (equal? (statements inst-seq)
	    (append (statements seq2)
		    '((assign argl (op list) (reg val))
		      (save argl))
		    (statements seq1)
		    '((restore argl)		
		      (assign argl (op cons) (reg val) (reg argl)))))))
	    
