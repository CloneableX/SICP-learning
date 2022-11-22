(define (lexical-address-lookup address env)
  (let ((target-env (offset-env (env-offset address) env)))
    (let ((binding (offset-pos (pos-offset address) target-env)))
      (if (eq? (binding-value binding) '*unassigned)
          (error "Variable is unbound: LEXICAL" (binding-variable binding))
          (binding-value binding)))))

(define (offset-env offset base-env)
  (cond ((= 0 offset) base-env)
        ((null? base-env)
         (error "Target environment is not exist: LEXICAL"))
        (else
          (offset-env (- offset 1) (enclosing-environment base-env)))))

(define (offset-pos offset env)
  (let ((frame (first-frame env)))
    (let ((variables (frame-variables frame))
          (values (frame-values frame)))
      (let ((var (list-ref variables offset))
            (value (list-ref values offset)))
        (if value
            (make-binding var value)
            (make-binding var '*unassigned*)))))) 
