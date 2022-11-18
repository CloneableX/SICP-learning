(load "linkage.scm")

;;;
;;; end-with-linkage
;;;

;;; it should not append any instructions when linkage is 'next'
(let ((inst-seq
       (make-instruction-sequence '() '(val) '((assign val (const 3))))))
  (let ((linkage-inst-seq (end-with-linkage 'next inst-seq)))
    (equal? linkage-inst-seq
	    inst-seq)))

;;; it should return by register continue when linkage is 'return'
(let ((inst-seq
       (make-instruction-sequence '() '(val) '((assign val (const 3))))))
  (let ((linkage-inst-seq (end-with-linkage 'return inst-seq)))
    (and
     (list-contain? '(continue)
		    (registers-needed linkage-inst-seq))
     (equal? (statements linkage-inst-seq)
	     '((assign val (const 3))
	       (goto (reg continue)))))))

(let ((inst-seq
       (make-instruction-sequence '() '(continue) '((assign continue (label next-entry))))))
  (let ((linkage-inst-seq (end-with-linkage 'return inst-seq)))
    (equal? (statements linkage-inst-seq)
	    '((save continue)
	      (assign continue (label next-entry))
	      (restore continue)
	      (goto (reg continue))))))

;;; it should goto label when linkage is a symbol exclude 'next' and 'return'
(let ((inst-seq
       (make-instruction-sequence '() '(val) '((assign val (const 3))))))
  (let ((linkage-inst-seq (end-with-linkage 'next-entry inst-seq)))
    (equal? (statements linkage-inst-seq)
	    '((assign val (const 3))
	      (goto (label next-entry))))))