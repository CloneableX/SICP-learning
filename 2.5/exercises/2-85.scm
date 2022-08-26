(load "2-83.scm")

(define (project object)
  ((get 'project (type-tag object)) object))

(define (drop object)
  (let ((project-obj (project object)))
    (if (equ? (raise project-obj) object)
	(drop project-obj)
	object)))

(define (subtype-make object)
  (get-coercion (type-tag object) (subtype object)))

(define (install-complex-number-package)
  ;; internal procedures
  (define (project z)
    (real-part z))

  ;;interfaces to rest of the system
  (put 'project 'complex
       (lambda (z) ((subtype-make z) (real-part z))))
  'done)