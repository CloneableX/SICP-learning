(load "instruction.scm")
(load "register.scm")
(load "assemble.scm")
(load "stack.scm")

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (name)
       ((machine 'allocate-register) name))
     register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-controller-description)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register))
	(flag (make-register))
	(stack (make-stack))
	(the-instruction-sequence '())
	(the-labels '()))
    (let ((register-table
	   (list (list 'pc pc) 
		 (list 'flag flag)
		 (list 'stack stack)))
	  (the-ops '()))
      (define (allocate-register register-name)
	(let ((register (find-register register-name)))
	  (if register
	      (error "Multiply defined register: " register-name)
	      (set! register-table
		    (cons (list register-name (make-register))
			  register-table)))
	  'register-allocated))
      (define (lookup-register register-name)
	(let ((register (find-register register-name)))
	  (if register
	      register
	      (error "Unknown register: " register-name))))
      (define (find-register register-name)
	(let ((register-entry (assoc register-name register-table)))
	  (if register-entry
	      (cadr register-entry)
	      false)))
      (define (lookup-operation op-name)
	(let ((op (assoc op-name the-ops)))
	  (if op
	      (cadr op)
	      (error "Unknown operation: " op-name))))
      (define (lookup-label label-name)
	(let ((label (assoc label-name the-labels)))
	  (if label
	      (cdr label)
	      (error "Unknown label: " label-name))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (let ((next-inst (car insts)))
		((instruction-execute-proc next-inst))
		(execute)))))
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'execute) (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq) (set! the-instruction-sequence seq)))
	      ((eq? message 'install-controller-description)
	       (lambda (description)
		 (set! the-instruction-sequence (car description))
		 (set! the-labels (cdr description))))
	      ((eq? message 'install-operations)
	       (lambda (ops) (set! the-ops (append the-ops ops))))
	      ((eq? message 'advance-pc-to)
	       (lambda (seq) (set-contents! pc seq)))
	      ((eq? message 'advance-pc)
	       (set-contents! pc (cdr (get-contents pc))))
	      ((eq? message 'allocate-register)
	       allocate-register)
	      ((eq? message 'get-register)
	       lookup-register)
	      ((eq? message 'get-stack)
	       stack)
	      ((eq? message 'lookup-operation)
	       lookup-operation)
	      ((eq? message 'lookup-label)
	       lookup-label)
	      (else
	       (error "Unknown request: MACHINE"))))
      dispatch)))