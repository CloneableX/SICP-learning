(load "utils/tagged-list.scm")
(load "instruction.scm")
(load "register.scm")
(load "machine-util.scm")
(load "simulator-util.scm")

(define (assign? exp)
  (tagged-list? exp 'assign))
(define (goto? exp)
  (tagged-list? exp 'goto))
(define (test? exp)
  (tagged-list? exp 'test))
(define (branch? exp)
  (tagged-list? exp 'branch))
(define (save? exp)
  (tagged-list? exp 'save))
(define (restore? exp)
  (tagged-list? exp 'restore))
(define (perform? exp)
  (tagged-list? exp 'perform))

(define (const? exp)
  (tagged-list? exp 'const))
(define (reg? exp)
  (tagged-list? exp 'reg))
(define (operation? exp)
  (tagged-list? (car exp) 'op))
(define (label? exp)
  (tagged-list? exp 'label))

(define (assemble controller-text machine)
  (let ((description (extract-labels controller-text)))
    (let ((insts (car description)))
      (for-each
       (lambda (inst)
	 (update-instruction-execute-proc!
	  inst
	  (make-execute-procedure
	   (instruction-text inst)
	   machine)))
       insts)
      description)))


(define (extract-labels text)
  (if (null? text)
      (cons '() '())
      (let ((inst (car text))
	    (rest-insts (cdr text)))
	(let ((description (extract-labels rest-insts)))
	  (let ((insts (car description))
		(labels (cdr description)))
	    (if (symbol? inst)
		(cons insts
		      (cons (make-label-entry inst insts labels)
			    labels))
		(cons (cons (make-instruction inst)
			    insts)
		      labels)))))))

(define (make-label-entry label-name insts labels)
  (let ((label (assoc label-name labels)))
    (if label
	(error "Not allow same label: ASSEMBLE" label-name)
	(cons label-name insts))))

(define (make-execute-procedure inst-text machine)
  (cond ((assign? inst-text)
	 (make-assign inst-text machine))
	((goto? inst-text)
	 (make-goto inst-text machine))
	((test? inst-text)
	 (make-test inst-text machine))
	((branch? inst-text)
	 (make-branch inst-text machine))
	((save? inst-text)
	 (make-save inst-text machine))
	((restore? inst-text)
	 (make-restore inst-text machine))
	((perform? inst-text)
	 (make-perform inst-text machine))
	(else
	 (error "Unknown instruction type: ASSEMBLE" inst-text))))

(define (make-assign inst-text machine)
  (let ((register (get-register machine (cadr inst-text)))
	(val-expression (cddr inst-text)))
    (let ((val-proc
	   (if (operation? val-expression)
	       (make-operation-proc val-expression machine)
	       (make-primitive-proc (car val-expression) machine))))
      (lambda ()
	(set-contents! register (val-proc))
	(advance-pc machine)))))

(define (goto-label-name label)
    (cadr label))
(define (make-goto inst-text machine)
  (let ((dest (cadr inst-text)))
    (if (reg? dest)
	(let ((register (get-register machine (cadr dest))))
	  (lambda ()
	    (let ((label-name (goto-label-name (get-contents register))))
	      (let ((inst-sequence (lookup-label-insts machine label-name)))
		(advance-pc-to machine inst-sequence)))))
	(let ((label-name (goto-label-name dest)))
	  (lambda ()
	    (let ((inst-sequence (lookup-label-insts machine label-name)))
	      (advance-pc-to machine inst-sequence)))))))

(define (make-test inst-text machine)
  (let ((predicate-proc
	 (make-operation-proc (cdr inst-text) machine)))
    (lambda ()
      (set-register-contents! machine 'flag (predicate-proc))
      (advance-pc machine))))

(define (make-branch inst-text machine)
  (let ((label-name (goto-label-name (cadr inst-text))))
    (lambda ()
      (if (get-register-contents machine 'flag)
	  (let ((inst-sequence (lookup-label-insts machine label-name)))
	    (advance-pc-to machine inst-sequence))
	  (advance-pc machine)))))

(define (make-save inst-text machine)
  (lambda ()
    (let ((val (get-register-contents machine (cadr inst-text))))
      (push-stack machine val)
      (advance-pc machine))))

(define (make-restore inst-text machine)
  (let ((register-name (cadr inst-text)))
    (lambda ()
      (let ((value (pop-stack machine)))
	(set-register-contents! machine register-name value)
	(advance-pc machine)))))

(define (make-perform inst-text machine)
  (let ((operation-proc
	 (make-operation-proc (cdr inst-text) machine)))
    (lambda ()
      (operation-proc)
      (advance-pc machine))))
	

;;;
;;; primitive instruction
;;;
      
(define (make-primitive-proc exp machine)
  (cond ((const? exp)
	 (lambda ()
	   (cadr exp)))
	((reg? exp)
	 (lambda ()
	   (get-register-contents machine (cadr exp))))
	((label? exp)
	 (lambda () exp))
	(else
	 (error "Unknown primitive express type: ASSEMBLE" exp))))

(define (make-operation-proc exp machine)
  (let ((op-type (cadar exp)))
    (let ((op-proc (get-operation-proc machine op-type))
	  (operand-procs
	   (map
	    (lambda (operand)
	      (if (label? operand)
		  (error "Not allow label as operand: ASSEMBLE" exp)
		  (make-primitive-proc operand machine)))
	    (cdr exp))))
      (lambda ()
	(apply op-proc (map (lambda (p) (p)) operand-procs))))))
    