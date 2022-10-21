(load "test-util.scm")
(load "qeval.scm")

;;;
;;; find-rules
;;;

;;; it should find results apply rules
(begin
  (before-test-reset!)
  (reset-rules!)
  (reset-rule-apply-counter!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(rule '(rule (over-salary (? person) (? money)) (and (job (? person) (computer programmer)) (salary (? person) (? money)))))
	(pattern '(over-salary (? x) 10000)))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (add-rule! rule)
    (equal? (stream-first (find-rules pattern '()))
	    (list (make-binding '(? 1 person) '(Jimmy Hanks))
		  (make-binding '(? 1 money) 10000)
		  (make-binding '(? x) '(? 1 person))))))

;;;
;;; apply-a-rule
;;;

;;; it should apply rule when pattern unify match rule
(begin
  (before-test-reset!)
  (reset-rules!)
  (reset-rule-apply-counter!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(rule '(rule (over-salary (? person) (? money)) (and (job (? person) (computer programmer)) (salary (? person) (? money)))))
	(pattern '(over-salary (? x) 10000)))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first (apply-a-rule rule pattern '()))
	    (list (make-binding '(? 1 person) '(Jimmy Hanks))
		  (make-binding '(? 1 money) 10000)
		  (make-binding '(? x) '(? 1 person))))))

;;; it should return empty stream when apply rule failed
(begin
  (before-test-reset!)
  (reset-rules!)
  (load "qeval.scm")
  (let ((rule '(rule (over-salary (? person) (? money))))
	(pattern '(lives-near (? person-1) (? person-2))))
    (stream-null? (apply-a-rule rule pattern '()))))

;;;
;;; qeval
;;;

;;; it should match assertion in query without variables
(begin
  (reset-assertions!)
  (reset-operation-table!)
  (load "qeval.scm")
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(frame-stream (singleton-stream '())))
    (add-assertion! assertion)
    (stream-null? (qeval assertion frame-stream))))

;;; it should match assertion in query with one variables
(begin
  (reset-assertions!)
  (reset-operation-table!)
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(query '(job (? x) (computer programmer)))
	(frame-stream (singleton-stream '()))
	(result-frame
	 (list (make-binding '(? x) '(Jimmy Hanks)))))
    (add-assertion! assertion)
    (equal? (stream-first (qeval '(job (? x) (computer programmer)) frame-stream)) result-frame)))


;;; it should match assertion in 'and' query
(begin
  (before-test-reset!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(query '(and (job (? person) (computer programmer)) (salary (? person) 10000)))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? person) '(Jimmy Hanks))))))

;;; it should match assertion in 'or' query
(begin
  (before-test-reset!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(job (Bruce Eleven) (computer programmer)))
	(query '(or (job (? person-1) ((? division) programmer))
		    (job (? person-2) ((? division) programmer))))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? division) 'computer)
		  (make-binding '(? person-1) '(Bruce Eleven))))))

;;; it should match assertion in 'not' query
(begin
  (before-test-reset!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(job (Bruce Eleven) (computer programmer)))
	(a-3 '(salary (Jimmy Hanks) 10000))
	(a-4 '(salary (Bruce Eleven) 8000))
	(query '(and (job (? x) (computer programmer)) (not (salary (? x) 10000))))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (add-assertion! a-3)
    (add-assertion! a-4)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? x) '(Bruce Eleven))))))

;;; it should match assertion in 'lisp-value' query
(begin
  (before-test-reset!)
  (load "qeval.scm")
  (let ((a-1 '(salary (Jimmy Hanks) 10000))
	(a-2 '(salary (Bruce Eleven) 8000))
	(query '(and (salary (? x) (? money)) (lisp-value < (? money) 10000)))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? money) 8000)
		  (make-binding '(? x) '(Bruce Eleven))))))

;;; it should match assertion in 'unique' query
(begin
  (before-test-reset!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(job (Bruce Eleven) (computer programmer)))
	(a-3 '(job (John Hook) (computer programmer trainee)))
	(query '(and (job (? x) (? j)) (unique (job (? anyone) (? j)))))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (add-assertion! a-3)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? anyone) '(John Hook))
		  (make-binding '(? j) '(computer programmer trainee))
		  (make-binding '(? x) '(John Hook))))))

;;; it should match rules in pattern query
(begin
  (before-test-reset!)
  (reset-rules!)
  (load "qeval.scm")
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(rule '(rule (over-salary (? person) (? money)) (and (job (? person) (computer programmer)) (salary (? person) (? money)))))
	(query '(over-salary (? x) 10000))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (add-rule! rule)
    (equal? (stream-first (qeval query frame-stream))
	    (list (make-binding '(? person) '(Jimmy Hanks))
		  (make-binding '(? money) 10000)
		  (make-binding '(? x) '(? person))))))
	    

;;; it should return empty stream when match failed
(begin
  (before-test-reset!)
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(query '(and (job (? person) (computer)) (salary (? person) 10000)))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (stream-null? (qeval query frame-stream))))
	    
;;;
;;; conjoin
;;;

;;; it should query conjuncts in qeval everyone
(begin
  (before-test-reset!)
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(salary (Jimmy Hanks) 10000))
	(conjuncts '((job (? person) (computer programmer)) (salary (? person) (? money))))
	(frame-stream (singleton-stream '())))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first (conjoin conjuncts frame-stream))
	    (list (make-binding '(? money) 10000)
		  (make-binding '(? person) '(Jimmy Hanks))))))


;;;
;;; negate
;;;
(begin
  (before-test-reset!)
  (let ((a-1 '(job (Jimmy Hanks) (computer programmer)))
	(a-2 '(job (Bruce Eleven) (computer programmer trainee)))
	(operands '((job (? x) (computer programmer))))
	(frame-stream (list->stream 
		       (list
			(list (make-binding '(? x) '(Jimmy Hanks)))
			(list (make-binding '(? x) '(Bruce Eleven)))))))
    (add-assertion! a-1)
    (add-assertion! a-2)
    (equal? (stream-first
	     (negate operands frame-stream))
	    (list (make-binding '(? x) '(Bruce Eleven)))))) 

;;;
;;; lisp-value
;;;

(begin
  (before-test-reset!)
  (let ((call '(< (? x) 10000))
	(frame-stream (list->stream
		       (list
			(list (make-binding '(? x) '10000))
			(list (make-binding '(? x) '8000))))))
    (equal? (stream-first
	     (lisp-value call frame-stream))
	    (list (make-binding '(? x) '8000)))))

;;;
;;; unique
;;;

;;; it should return unique item from data base
(begin
  (before-test-reset!)
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(operands '((job (? x) (computer programmer))))
	(frame-stream (singleton-stream '())))
    (add-assertion! assertion)
    (equal? (stream-first
	     (uniquely-asserted operands frame-stream))
	    (list (make-binding '(? x) '(Jimmy Hanks))))))




