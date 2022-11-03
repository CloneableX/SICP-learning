(load "machine.scm")
(load "simulator-util.scm")

;;;
;;; assemble
;;;

;;; it should parse 'assign' instruction
(let ((text '(controller
	      (assign a (const 1)))))
  (let ((machine (make-machine '(a) '() text)))
    (let ((insts (car (assemble text machine))))
      ((machine 'advance-pc-to) insts)
      ((instruction-execute-proc (car insts)))
      (equal? (get-register-contents machine 'a)
	      1))))

;;; it should parse 'goto' instruction
(let ((text '(controller
	        (assign continue (label label-a))
	        (goto (label label-b))
	      label-a
	        (assign a (const 1))
		(goto (label controller-done))
	      label-b
	        (assign a (const 2))
		(goto (reg continue))
	      controller-done)))
  (let ((machine (make-machine '(a continue) '() text)))
    (start machine)			
    (equal? (get-register-contents machine 'a)
	    1)))

;;; it should parse 'test' & 'branch' instruction
(let ((text '(controller
	        (assign a (const 2))
	      test-a
	        (test (op <) (reg a) (const 3))
		(branch (label label-b))
		(assign b (const 1))
		(goto (label controller-done))
	      label-b
	        (assign b (const 2))
	      controller-done)))
  (let ((machine (make-machine '(a b)
			       (list (list '< <))
			       text)))
    (start machine)
    (equal? (get-register-contents machine 'b)
	    2)))

;;; it should parse 'save'&'restore' instruction
(let ((text '(controller
	        (assign a (const 1))
		(save a)
		(assign a (const 2))
		(restore a))))
  (let ((machine (make-machine '(a) '() text)))
    (start machine)
    (equal? (get-register-contents machine 'a)
	    1)))

;;; it should parse 'perform' instruction
(let ((text '(controller
	        (assign a (const 1))
		(perform (op assign-x) (reg a))))
      (x 0))
  (let ((machine (make-machine '(a)
			       (list
				(list 'assign-x (lambda (val) (set! x val))))
			       text)))
    (start machine)
    (equal? x 1)))
					   

;;; it should parse label
(let ((text '(controller
	      label-a
	        (assign a (const 1)))))
  (let ((machine (make-machine '(a) '() text)))
    (let ((labels (cdr (assemble text machine))))
      (not (equal? (assoc 'label-a labels) false)))))

;;;
;;; extract-labels
;;;

;;; it should associate label with corresponding instruction sequence
(let ((text '(controller
	      label-a
                (assign a (const 1)))))
  (let ((description (extract-labels text)))
    (let ((insts (cdr (assoc 'label-a (cdr description)))))
      (equal? (instruction-text (car insts))
	      '(assign a (const 1))))))


;;;
;;; make-assign
;;;

;;; it should parse text to assign instruction
(let ((text '(assign a (const 1)))
      (machine
       (make-machine '(a)
		     '()
		     '(controller
		       (assign a (const 1))))))
  (let ((execute-proc (make-assign text machine)))
    ((machine 'advance-pc-to)
     (list (make-instruction-with-proc text execute-proc)))
    (execute-proc)
    (equal? (get-register-contents machine 'a)
	    1)))

;;; it should parse text to assign by resulting operation
(let ((text '(assign a (op +) (const 1) (const 2)))
      (machine
       (make-machine '(a)
		     (list (list '+ +))
		     '())))
  (let ((execute-proc (make-assign text machine)))
    ((machine 'advance-pc-to)
     (list (make-instruction-with-proc text execute-proc)))
    (execute-proc)
    (equal? (get-register-contents machine 'a) 3)))

;;;
;;; make-goto
;;;

;;; it should parse text to goto instruction
(let ((text '(goto (label label-a)))
      (machine
       (make-machine '(a)
		     '()
		     '(controller
		       label-a
		         (assign a (const 1))))))
  (let ((execute-proc (make-goto text machine)))
    (execute-proc)
    (machine 'execute)
    (equal? (get-register-contents machine 'a) 1)))

;;; it should goto label stored in register
(let ((text '(goto (reg a)))
      (machine
       (make-machine '(a b)
		     '()
		     '(controller
		       label-b
		         (assign b (const 1))))))
  (let ((execute-proc (make-goto text machine)))
    (set-register-contents! machine 'a '(label label-b))
    (execute-proc)
    (machine 'execute)
    (equal? (get-register-contents machine 'b)
	    1)))

;;;
;;; make-test
;;;

;;; it should store test's result into register flag
(let ((text '(test (op <) (const 1) (const 2)))
      (machine (make-machine '(a) (list (list '< <)) '())))
  (let ((execute-proc (make-test text machine)))
    (set-register-contents! machine 'pc (make-instruction-with-proc '(assign a (const 1)) (lambda () 1)))
    (execute-proc)
    (equal? (get-register-contents machine 'flag)
	    true)))

;;;
;;; make-branch
;;;

;;; it should go to label when register flag's value is true
(let ((text '(branch (label label-a)))
      (machine (make-machine '(a) '()
			     '(controller
			       label-a
			         (assign a (const 1))
				 (goto (label controller-done))
			       label-b
			         (assign a (const 2))
			       controller-done))))
  (let ((execute-proc (make-branch text machine)))
    (set-register-contents! machine 'flag true )
    (execute-proc)
    (machine 'execute)
    (equal? (get-register-contents machine 'a)
	    1)))

;;; it should advance pc when register flag's value is false
(let ((controller-text '(controller
			 label-a
			   (test (op <) (const 2) (const 1))
			   (branch (label label-b))
			   (assign a (const 1))
			   (goto (label controller-done))
			 label-b
			   (assign a (const 2))
			 controller-done)))
  (let ((machine (make-machine '(a) (list (list '< <)) controller-text)))
    (start machine)
    (equal? (get-register-contents machine 'a)
	    1)))

;;;
;;; make-save
;;;

;;; it should store register's content into stack
(let ((text '(controller
	        (assign a (const 1))
		(save a)
		(assign a (const 2))
		(save a))))
  (let ((machine (make-machine '(a) '() text)))
    (start machine)
    (equal? (get-stack-contents machine)
	    (list 2 1))))

;;;
;;; make-restore
;;;

;;; it should pop value from the top of stack
(let ((text '(controller
	        (assign a (const 1))
		(save a)
		(assign a (const 2))
		(restore a))))
  (let ((machine (make-machine '(a) '() text)))
    (start machine)
    (equal? (get-register-contents machine 'a) 1)))

;;; 

;;;
;;; make-primitive-proc
;;;

;;; it should transform 'const' expression
(let ((exp '(const 1))
      (machine (make-machine '(a) '() '())))
  (equal? ((make-primitive-proc exp machine))
	  1))

;;; it should transform 'reg' expression
(let ((exp '(reg a))
      (machine (make-machine '(a) '() '())))
  (set-register-contents! machine 'a 1)
  (equal? ((make-primitive-proc exp machine))
	  1))

;;; it should transform 'label' expression
(let ((exp '(label label-a))
      (machine (make-machine '(a) '() '())))
  (equal? ((make-primitive-proc exp machine))
	  '(label label-a)))

;;;
;;; make-operation-proc
;;;

;;; it should compute in operations installed in machine
(let ((exp '((op +) (const 1) (const 1)))
      (machine
       (make-machine '(a)
		     (list (list '+ +))
		     '())))
  (equal? ((make-operation-proc exp machine)) 2))

