(load "machine.scm")
(load "simulator-util.scm")

;;;
;;; make-machine
;;;

;;; it should install registers
(let ((machine (make-machine '(a) '() '())))
  (let ((register ((machine 'get-register) 'a)))
    (eq? (get-contents register) '*unassigned*)))

;;; it should execute in controller text
(let ((machine
       (make-machine '(a)
		     '()
		     '(controller
		       (assign a (const 1))))))
  (machine 'start)
  (equal? (get-register-contents machine 'a)
	  1))

;;; it should execute in controller text include operations
(let ((machine
       (make-machine '(a)
		     (list (list '+ +))
		     '(controller
		       (assign a (op +) (const 1) (const 2))))))
  (machine 'start)
  (equal? (get-register-contents machine 'a)
	  3))

;;;
;;; make-new-machine
;;;

;;;
;;; start
;;;

;;; it should done when instruction sequence is ending
(let ((machine (make-new-machine)))
  (eq? (machine 'start) 'done))

;;; it should execute instruction in order of sequence
(let ((machine (make-new-machine))
      (result 0))
  ((machine 'install-instruction-sequence)
   (list 
    (make-instruction-with-proc 
     '(assign result (const 1))
     (lambda ()
       (set! result 1)
       (machine 'advance-pc)))))
  (machine 'start)
  (equal? result 1))

;;; it should execute repeatly
(let ((machine (make-new-machine))
      (result 0))
  ((machine 'install-instruction-sequence)
   (list
    (make-instruction-with-proc 
     '(assign result (const 1)) 
     (lambda ()
       (set! result 1)
       (machine 'advance-pc)))))
  (machine 'start)
  (set! result 0)
  (machine 'start)
  (equal? result 1))

;;; it should go to other instruction sequence and execute
(let ((machine (make-new-machine))
      (result 0))
  ((machine 'install-instruction-sequence)
   (list
    (make-instruction-with-proc 
     '(assign result (const 1))
     (lambda ()
       (set! result 1)
       ((machine 'advance-pc-to)
	(list 
	 (make-instruction-with-proc
	  '(assign result (const 2))
	  (lambda ()
	    (set! result 2)
	    (machine 'advance-pc)))))))))
  (machine 'start)
  (equal? result 2))

;;;
;;; allocate-register
;;;

;;; it should allocate register in register table
(let ((machine (make-new-machine)))
  ((machine 'allocate-register) 'a)
  (eq? (get-contents ((machine 'get-register) 'a))
       '*unassigned*))
		  
;;; it should not multiple allocate same register
(let ((machine (make-new-machine)))
  ((machine 'allocate-register) 'a)
  ((machine 'allocate-register) 'a))

;;;
;;; get-register
;;;

;;; it should not look up register
(let ((machine (make-new-machine)))
  ((machine 'get-register) 'a))
