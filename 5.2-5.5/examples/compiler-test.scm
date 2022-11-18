(load "compiler.scm")

;;;
;;; simple expression
;;;

;;; it should compile self-evaluating expression
(let ((inst-seq (compile '3 'val 'next)))
  (equal? inst-seq
	  (list '()
		'(val)
		'((assign val (const 3))))))

(let ((inst-seq (compile '"hello" 'val 'next)))
  (equal? inst-seq
	  (list '()
		'(val)
		'((assign val (const "hello"))))))

;;; it should compile variable expression
(let ((inst-seq (compile 'x 'val 'next)))
  (equal? inst-seq
	  (list '(env)
		'(val)
		'((assign val (op lookup-variable-value) (const x) (reg env))))))

;;;
;;; compound expression
;;;

;;; it should compile primitive procedure express
(begin
  (load "compiler.scm")
  (let ((inst-seq (compile '(+ 1 3) 'val 'next)))
    (equal? (statements inst-seq)
	    '((assign proc (op lookup-variable-value) (const +) (reg env))
	       (assign val (const 3))
	       (assign argl (op list) (reg val))
	       (assign val (const 1))
	       (assign argl (op cons) (reg val) (reg argl))
	       (test (op primitive-procedure?) (reg proc))
	       (goto (label primitive-branch3))
	      compiled-branch2
	       (assign continue (label after-call1))
	       (assign val (op compiled-procedure-entry) (reg proc))
	       (goto (reg val))
	      primitive-branch3
	       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
	      after-call1))))

;;;
;;; special form
;;;

;;;
;;; compile-begin
;;;

(let ((exp '(begin 3 4 x)))
  (let ((inst-seq (compile exp 'val 'next)))
    (equal? (statements inst-seq)
	    '((assign val (const 3))
	      (assign val (const 4))
	      (assign val (op lookup-variable-value) (const x) (reg env))))))

;;;
;;; compile-lambda
;;;

(begin
  (load "compiler.scm")
  (let ((exp '(lambda (x y) x)))
    (let ((inst-seq (compile exp 'val 'next)))
      (equal? (statements inst-seq)
	      '((assign val (op make-compiled-procedure) (label entry2) (reg env))
		(goto (label after-lambda1))
		entry2
		(assign env (op compiled-procedure-env) (reg proc))
		(assign env (op extend-environment)
			(const (x y)) (reg argl) (reg env))
		(assign val (op lookup-variable-value) (const x) (reg env))
		(goto (reg continue))
		after-lambda1)))))

;;;
;;; compile-if
;;;

(begin
  (load "compiler.scm")
  (let ((exp '(if x 3 4)))
    (let ((inst-seq (compile exp 'val 'next)))
      (equal? (statements inst-seq)
	      '((assign val (op lookup-variable-value) (const x) (reg env))
		 (test (op true?) (reg val))
		 (goto (label true-branch3))
		false-branch2
		 (assign val (const 4))
		 (goto (label after-if1))
	        true-branch3
		 (assign val (const 3))
		after-if1)))))

;;;
;;; compile-assignment
;;;

(let ((exp '(set! x 3)))
  (let ((inst-seq (compile exp 'val 'next)))
    (equal? (statements inst-seq)
	    '((assign val (const 3))
	      (perform (op set-variable-value!) (const x) (reg val) (reg env))
	      (assign val (const ok))))))

;;;
;;; compile-definition
;;;

(let ((exp '(define x 3)))
  (let ((inst-seq (compile exp 'val 'next)))
    (equal? (statements inst-seq)
	    '((assign val (const 3))
	      (perform (op add-binding-to-frame!) (const x) (reg val) (reg env))
	      (assign val (const ok))))))
		 
	      
  
			    