(load "register.scm")

;;;
;;; get-contents
;;;

;;; it should not have any contents
(let ((register (make-register)))
  (eq? (get-contents register) '*unassigned*))


;;;
;;; set-contents
;;;

;;; it should set register contents
(let ((register (make-register)))
  (set-contents! register 'saved)
  (eq? (get-contents register) 'saved))