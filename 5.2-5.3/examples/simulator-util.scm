(load "machine-util.scm")
(load "register.scm")

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents ((machine 'get-register) register-name)))
(define (set-register-contents! machine register-name contents)
  (let ((register (get-register machine register-name)))
    (set-contents! register contents)))
