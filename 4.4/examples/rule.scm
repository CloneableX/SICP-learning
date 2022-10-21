(load "utils/tagged-list.scm")
(load "operation-table.scm")
(load "expression.scm")
(load "frame.scm")

(define the-rules the-empty-stream)
(define (reset-rules!) (set! the-rules the-empty-stream))
(define (get-all-rules) the-rules)

(define (conclusion rule) (cadr rule))
(define (rule-body rule) (caddr rule))

(define (rule? assertion) (tagged-list? assertion 'rule))

(define (add-rule! rule)
  (store-index-of-rule rule)
  (let ((old-rules the-rules))
    (set! the-rules (cons-stream rule old-rules))
    'ok))

(define (store-index-of-rule rule)
  (let ((pattern (conclusion rule)))
    (if (use-index? pattern)
	(let ((current-stream (get-indexed-rules pattern)))
	  (put (index-key-of pattern) 'rule-stream
	       (cons-stream rule current-stream))))))

(define (unify-match pattern conclusion frame)
  (cond ((eq? frame 'failed) 'failed)
	((eq? pattern conclusion) frame)
	((var? pattern) (extend-if-balance pattern conclusion frame))
	((var? conclusion) (extend-if-balance conclusion pattern frame))
	((and (pair? pattern) (pair? conclusion))
	 (unify-match (cdr pattern) (cdr conclusion)
		      (unify-match (car pattern) (car conclusion) frame)))
	(else 'failed)))

(define (extend-if-balance variable value frame)
  (let ((binding (binding-in-frame variable frame)))
    (cond (binding
	   (unify-match (binding-value binding) value frame))
	  ((var? value)
	   (let ((binding (binding-in-frame value frame)))
	     (if binding
		 (unify-match variable (binding-value binding) frame)
		 (extend-frame variable value frame))))
	  ((depends-on? value variable frame) 'failed)
	  (else
	   (extend-frame variable value frame)))))

(define (depends-on? exp variable frame)
  (define (tree-walk e)
    (cond ((var? e)
	   (if (equal? e variable)
	       true
	       (let ((binding (binding-in-frame e frame)))
		 (if binding
		     (tree-walk (binding-value binding))
		     false))))
	  ((pair? e)
	   (or (tree-walk (car e))
	       (tree-walk (cdr e))))
	  (else false)))
  (tree-walk exp))

(define (fetch-rules pattern)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-indexed-rules exp)
  (get-stream (index-key-of exp) 'rule-stream))


(define (rename-variables-in rule)
  (let ((apply-id (new-rule-apply-id)))
    (define (tree-walk exp)
      (cond ((var? exp) (rename-variable exp apply-id))
	    ((pair? exp)
	     (cons
	      (tree-walk (car exp))
	      (tree-walk (cdr exp))))
	    (else exp)))
    (tree-walk rule)))

(define (rename-variable variable apply-id)
  (cons '? (cons apply-id (cdr variable))))

(define the-rule-apply-counter 0)
(define (reset-rule-apply-counter!)
  (set! the-rule-apply-counter 0))
(define (new-rule-apply-id)
  (let ((apply-id (+ the-rule-apply-counter 1)))
    (set! the-rule-apply-counter apply-id)
    apply-id))
