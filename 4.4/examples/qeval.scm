(load "stream-util.scm")
(load "assertion.scm")
(load "rule.scm")

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
	(qproc (cdr query) frame-stream)
	(stream-flatmap
	 (lambda (frame) 
	   (stream-append-delayed
	    (find-assertions query frame)
	    (delay (find-rules query frame))))
	 frame-stream))))

(define (find-rules pattern frame)
  (stream-flatmap
   (lambda (rule) (apply-a-rule rule pattern frame))
   (fetch-rules pattern)))

(define (apply-a-rule rule pattern frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((match-result (unify-match pattern (conclusion clean-rule) frame)))
      (if (eq? match-result 'failed)
	  the-empty-stream
	  (qeval (rule-body clean-rule)
		 (singleton-stream match-result))))))

;;;
;;; special form - add
;;;

(define (rest-conjuncts conjuncts) (cdr conjuncts))
(define (first-conjunct conjuncts) (car conjuncts))

(define (conjoin conjuncts frame-stream)
  (if (null? conjuncts)
      frame-stream
      (stream-map
       (lambda (frame1 frame2)
	 (merge-if-balance frame1 frame2 conjuncts))
       (qeval (first-conjunct conjuncts) frame-stream)
       (conjoin (rest-conjuncts conjuncts) frame-stream))))
(put 'and 'qeval conjoin)


;;;
;;; special form - or
;;;

(define (first-disjunct disjuncts) (car disjuncts))
(define (rest-disjuncts disjuncts) (cdr disjuncts))

(define (disjoin disjuncts frame-stream)
  (if (null? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)


;;;
;;; special form - not
;;;

(define (negate-query operands) (car operands))

(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null?
	  (qeval (negate-query operands)
		 (singleton-stream frame)))
	 frame
	 the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

;;;
;;; special form - lisp-value
;;;

(define (predicate exp) (car exp))
(define (args exp) (cdr exp))

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
	  (instantiate
	   call
	   frame
	   (lambda (v f) (error "Unknown pattern variable: LISP-VALUE" v))))
	 frame
	 the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
	 (args exp)))

(put 'lisp-value 'qeval lisp-value)


;;;
;;; special form - unique
;;;

(define (unique-query operands) (car operands))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((result-stream (qeval (unique-query operands)
				 (singleton-stream frame))))
       (if (= (stream-length result-stream) 1)
	   result-stream
	   the-empty-stream)))
   frame-stream))

(put 'unique 'qeval uniquely-asserted)
