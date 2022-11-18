(load "instruction-sequence.scm")

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq)
	   (statements body-seq))))

(define (append-instruction-sequences . seqs)
  (define (append-seq-list seqs)
    (if (null? seqs)
	(empty-instruction-sequence)
	(append-seqs
	 (car seqs)
	 (append-seq-list (cdr seqs)))))
  (define (append-seqs seq1 seq2)
    (make-instruction-sequence
     (list-union
      (registers-needed seq1)
      (list-difference (registers-needed seq2)
		       (registers-modified seq1)))
     (list-union (registers-modified seq1)
		 (registers-modified seq2))
     (append (statements seq1)
	     (statements seq2))))
  (append-seq-list seqs))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (make-instruction-sequence
       (list-union (registers-needed seq1)
		   (registers-needed seq2))
       (list-union (registers-modified seq1)
		   (registers-modified seq2))
       (append (statements seq1)
	       (statements seq2)))
      (let ((reg (car regs)))
	(if (and (modified-register? seq1 reg)
		 (needed-register? seq2 reg))
	    (preserving (cdr regs)
			(make-instruction-sequence
			 (list-union (list reg)
				     (registers-needed seq1))
			 (list-difference (registers-modified seq1)
					  (list reg))
			 (append `((save ,reg))
				 (statements seq1)
				 `((restore ,reg))))
			seq2)
	    (preserving (cdr regs) seq1 seq2)))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
	       (registers-needed seq2))
   (list-union (registers-modified seq1)
	       (registers-modified seq2))
   (append (statements seq1)
	   (statements seq2))))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
	((memq (car s1) s2) (list-union (cdr s1) s2))
	(else
	 (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
	((memq (car s1) s2) (list-difference (cdr s1) s2))
	(else
	 (cons (car s1) (list-difference (cdr s1) s2)))))

(define (list-contain? s1 s2)
  (define (loop-compare s1 s2)
    (cond ((null? s1) true)
	  ((memq (car s1) s2) (loop-compare (cdr s1) s2))
	  (else false)))
  (if (= (length s1) (length s2))
      (loop-compare s1 s2)
      false))