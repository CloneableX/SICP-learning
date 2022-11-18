(load "compiler.scm")

;;;
;;; compile-lambda
;;;

;;; it should compile lambda expression when linkage is label
(begin
  (load "compiler.scm")
  (let ((exp '(lambda (x) x)))
    (let ((inst-seq (compile-lambda exp 'proc 'next-entry)))
      (and
       (list-contain? '(proc) (registers-modified inst-seq))
       (equal? (statements inst-seq)
	       '((assign proc (op make-compiled-procedure)
			 (label entry2) (reg env))
		  (goto (label next-entry))
		 entry2
		  (assign env (op compiled-procedure-env) (reg proc))
		  (assign env (op extend-environment)
			  (const (x)) (reg argl) (reg env))
		  (assign val (op lookup-variable-value) (const x) (reg env))
		  (goto (reg continue))
		 after-lambda1))))))

;;; it should goto label after-lambda when linkage is next
(begin
  (load "compiler.scm")
  (let ((exp '(lambda (x) x)))
    (let ((inst-seq (compile-lambda exp 'proc 'next)))
      (equal? (statements inst-seq)
	      '((assign proc (op make-compiled-procedure)
		       (label entry2) (reg env))
 		 (goto (label after-lambda1))
		entry2
		 (assign env (op compiled-procedure-env) (reg proc))
		 (assign env (op extend-environment)
			 (const (x)) (reg argl) (reg env))
		 (assign val (op lookup-variable-value) (const x) (reg env))
		 (goto (reg continue))
		after-lambda1)))))