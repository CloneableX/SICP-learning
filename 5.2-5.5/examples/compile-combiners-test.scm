(load "compile-combiners.scm")

;;;
;;; tack-on-instruction-sequence
;;;

;;; it should ignore second used registers in second sequence
(let ((seq1 (make-instruction-sequence '(env) '(val) '((assign val (op lookup-variable-value) (const x) (reg env)))))
      (seq2 (make-instruction-sequence '(proc) '(env) '((assign env (op compiled-procedure-env) (reg proc))))))
  (let ((inst-seq (tack-on-instruction-sequence seq1 seq2)))
    (and
     (list-contain? '(env) (registers-needed inst-seq))
     (list-contain? '(val) (registers-modified inst-seq))
     (equal? (statements inst-seq)
	     '((assign val (op lookup-variable-value) (const x) (reg env))
	       (assign env (op compiled-procedure-env) (reg proc)))))))

;;;
;;; append-instruction-sequences
;;;

;;; it should append mulitple instruction sequences include their information of registers
(let ((seq1 (make-instruction-sequence '(val) '(argl exp) '((assign argl (op list) (reg val)))))
      (seq2 (make-instruction-sequence '(env exp) '(val) '((assign val (op lookup-variable-value) (const x) (reg env)))))
      (seq3 (make-instruction-sequence '() '() '((goto (reg continue))))))
  (let ((inst-seq (append-instruction-sequences seq1 seq2 seq3)))
    (and (list-contain? '(val env)
			(registers-needed inst-seq))
	 (list-contain? '(argl val exp)
			(registers-modified inst-seq))
	 (equal? (statements inst-seq)
		 '((assign argl (op list) (reg val))
		   (assign val (op lookup-variable-value) (const x) (reg env))
		   (goto (reg continue)))))))

;;;
;;; preserving
;;;

;;; it should simply append instruction sequences without preserved resigers
(let ((seq-1 (make-instruction-sequence '() '(val) '((assign val (const 1)))))
      (seq-2 (make-instruction-sequence '() '(val) '((assign val (const 2))))))
  (let ((inst-seq (preserving '() seq-1 seq-2)))
    (equal? (statements inst-seq)
	    '((assign val (const 1))
	      (assign val (const 2))))))

;;; it should preserve registers when registers are modified in sequence one and needed in sequence two
(let ((seq1-codes '((assign env (op extend-env) (reg unev) (reg argl) (reg env))
		    (assign proc (op lookup-variable-value) (const +) (reg env))))
      (seq2-codes '((assign proc (op lookup-variable-value (const *) (reg env)))
		    (assign val (op apply-primitive-procedure) (reg proc) (reg argl)))))
  (let ((seq1 (make-instruction-sequence '(env unev argl) '(env proc) seq1-codes))
	(seq2 (make-instruction-sequence '(proc argl env) '(val proc) seq2-codes)))
    (let ((inst-seq (preserving '(proc env) seq1 seq2)))
      (and
       (list-contain? '(env unev argl proc)
		      (registers-needed inst-seq))
       (list-contain? '(val proc)	
		      (registers-modified inst-seq))
       (equal? (statements inst-seq)
	       (append '((save env) (save proc))
		       seq1-codes
		       '((restore proc) (restore env))
		       (statements seq2)))))))

(let ((seq1 (make-instruction-sequence '(env) '(proc) '()))
      (seq2 (make-instruction-sequence '(argl) '(val) '())))
  (let ((inst-seq (preserving '(env proc argl val) seq1 seq2)))
    (and
     (list-contain? '(env argl)
		    (registers-needed inst-seq))
     (list-contain? '(proc val)
		    (registers-modified inst-seq)))))
