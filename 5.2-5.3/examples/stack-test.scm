(load "stack.scm")

;;;
;;; push
;;;

;;; it should push value into stack
(let ((stack (make-stack)))
  ((stack 'push) 1)
  ((stack 'push) 2)
  (equal? (stack 'get-contents)
	  (list 2 1)))

;;; pop

;;; it should pop value from stack top
(let ((stack (make-stack)))
  ((stack 'push) 1)
  ((stack 'push) 2)
  (equal? (stack 'pop) 2))		

;;; it should remove the value poped
(let ((stack (make-stack)))
  ((stack 'push) 1)
  ((stack 'push) 2)
  (stack 'pop)
  (equal? (stack 'get-contents)
	  (list 1)))

;;; it should throw error when stack is empty
(let ((stack (make-stack)))
  (stack 'pop))