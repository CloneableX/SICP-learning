(load "utils/tagged-list.scm")
(load "utils/explicit-control-utils.scm")
(load "utils/label.scm")
(load "instruction-sequence.scm")
(load "compile-combiners.scm")
(load "linkage.scm")

(define all-regs '(val continue proc argl env))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((variable? exp)
	 (compile-variable exp target linkage))
	((lambda? exp)
	 (compile-lambda exp target linkage))
	((if? exp)
	 (compile-if exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((begin? exp)
	 (compile-sequence (cdr exp) target linkage))
	((application? exp)
	 (compile-application exp target linkage))
	(else
	 (error "Unknown expresion type: COMPILE" exp))))

;;;
;;; compile-lambda
;;;

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
	   (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
	(end-with-linkage
	 lambda-linkage
	 (make-instruction-sequence
	  '(env) (list target)
	  `((assign ,target
		    (op make-compiled-procedure) (label ,proc-entry) (reg env)))))
	(compile-proc-body exp proc-entry))
       after-lambda))))

(define (compile-proc-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env argl proc) '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env (op extend-environment)
		(const ,formals) (reg argl) (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;;;
;;; compile-if
;;;

(define (compile-if exp target linkage)
  (let ((true-branch (make-label 'true-branch))
	(false-branch (make-label 'false-branch))
	(after-if (make-label 'after-if)))
    (let ((if-linkage
	   (if (eq? linkage 'next) after-if linkage)))
      (let ((pred-code (compile (if-predicate exp) 'val 'next))
	    (consequent-code (compile (if-consequent exp) 'val linkage))
	    (alternative-code (compile (if-alternative exp) 'val if-linkage)))
	(preserving '(env continue)
	 pred-code
	 (append-instruction-sequences
	  (make-instruction-sequence
	   '(val) '()
	   `((test (op true?) (reg val))
	     (goto (label ,true-branch))))
	  (parallel-instruction-sequences
	   (append-instruction-sequences false-branch alternative-code)
	   (append-instruction-sequences true-branch consequent-code))
	  after-if))))))

;;;
;;; compile-assignment
;;;

(define (compile-assignment exp target linkage)
  (let ((val-code (compile (assignment-value exp) 'val 'next))
	(variable (assignment-variable exp)))
    (end-with-linkage
     linkage
     (preserving '(env)
       val-code
       (make-instruction-sequence
	'(val env) (list target)
	`((perform (op set-variable-value!) (const ,variable) (reg val) (reg env))
	  (assign ,target (const ok))))))))

;;;
;;; compile-definition
;;;

(define (compile-definition exp target linkage)
  (let ((variable (definition-variable exp))
	(val-code (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      val-code
      (make-instruction-sequence
       '(env val) (list target)
       `((perform (op add-binding-to-frame!) (const ,variable) (reg val) (reg env))
	 (assign ,target (const ok))))))))

;;;
;;; compile-sequence
;;;

(define (compile-sequence exps target linkage)
  (if (last-exp? exps)
      (compile (car exps) target linkage)
      (preserving '(env continue)
       (compile (car exps) target 'next)
       (compile-sequence (cdr exps) target linkage))))

;;;
;;; compile-self-evaluating
;;;

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

;;;
;;; compile-variable
;;;

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence 
    '(env) (list target)
    `((assign ,target (op lookup-variable-value) (const ,exp) (reg env))))))

;;;
;;; compile procedure application
;;;

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
	(operand-codes
	 (map (lambda (operand)
		(compile operand 'val 'next))
	      (operands exp))))
    (end-with-linkage
     linkage
     (preserving '(env continue)
      proc-code
      (preserving
       '(proc continue)
       (construct-arglist operand-codes)
       (compile-procedure-call target linkage))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(after-call (make-label 'after-call)))
    (let ((compile-linkage
	   (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
	'(proc) '()
	`((test (op primitive-procedure?) (reg proc))
	  (goto (label ,primitive-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compiled-branch
	 (compile-proc-appl target compile-linkage))
	(append-instruction-sequences
	 primitive-branch
	 (make-instruction-sequence 
	  '(proc argl) (list target)
	  `((assign ,target
		    (op apply-primitive-procedure)
		    (reg proc)
		    (reg argl))))))
       after-call))))

(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
	      (eq? linkage 'return))
	 (make-instruction-sequence
	  '(proc continue) all-regs
	  '((assign val (op compiled-procedure-entry) (reg proc))
	    (goto (reg val)))))
	((and (eq? target 'val)
	      (not (eq? linkage 'return)))
	 (make-instruction-sequence
	  '(proc) all-regs
	  `((assign continue (label ,linkage))
	    (assign val (op compiled-procedure-entry) (reg proc))
	    (goto (reg val)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence
	    '(proc) all-regs
	    `((assign continue (label ,proc-return))
	       (assign val (op compiled-procedure-entry) (reg proc))
	       (goto (reg val))
	      ,proc-return
	       (assign ,target (reg val))
	       (goto (label ,linkage))))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val: COMPILE" target))))

	 

(define (construct-arglist operand-codes)
  (let ((arg-codes (reverse operand-codes)))
    (if (null? arg-codes)
	(make-instruction-sequence '() '(argl)
	 '((assign argl (const ()))))
	(let ((last-arg-code
	       (make-instruction-sequence '(val) '(argl)
					  (append (statements (car arg-codes))
						  '((assign argl (op list) (reg val)))))))
	  (if (null? (cdr arg-codes))
	      last-arg-code
	      (preserving '(env)
	       last-arg-code
	       (construct-rest-arglist (cdr arg-codes))))))))

(define (construct-rest-arglist arg-codes)
  (let ((arg-code
	 (preserving '(argl)
	  (car arg-codes)
	  (make-instruction-sequence '(val argl) '(argl)
	  '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr arg-codes))
	arg-code
	(preserving '(env)
	 arg-code
	 (construct-rest-arglist (cdr arg-codes))))))

