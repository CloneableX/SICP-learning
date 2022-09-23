(define (make-frame variables values)
  (map (lambda (var val) (cons var val))
       variables values))
(define (frame-variables frame) (map car frame))
(define (frame-values frame) (map cdr frame))
(define (add-binding-to-frame! variable value frame)
  (set-car! frame (cons variable value)))

