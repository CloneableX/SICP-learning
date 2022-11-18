(define (make-instruction-sequence
	 regs-needed regs-modified statements)
  (list regs-needed regs-modified statements))
(define (empty-instruction-sequence) (list '() '() '()))

(define (registers-needed seq)
  (if (symbol? seq) '() (car seq)))
(define (registers-modified seq)
  (if (symbol? seq) '() (cadr seq)))
(define (statements seq)
  (if (symbol? seq) (list seq) (caddr seq)))

(define (needed-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modified-register? seq reg)
  (memq reg (registers-modified seq)))