(load "rule.scm")

;;;
;;; rename-variable
;;;

;;; it should rename variable by prepend apply in to it
(begin
  (reset-rule-apply-counter!)
  (equal? (rename-variable '(? x) 1)
	  '(? 1 x)))

;;;
;;; rename-variables-in
;;;

;;; it should rename variables in rule
(begin
  (reset-rule-apply-counter!)
  (let ((rule '(rule (lives-near (? person-1) (? person-2)))))
    (equal? (rename-variables-in rule)
	    '(rule (lives-near (? 1 person-1) (? 1 person-2))))))

(begin
  (reset-rule-apply-counter!)
  (let ((rule '(rule (salary-over (? person) (? money)) (and (job (? person) (computer programmer)) (salary (? person) (? money))))))
    (equal? (rename-variables-in rule)
	    '(rule (salary-over (? 1 person) (? 1 money)) (and (job (? 1 person) (computer programmer)) (salary (? 1 person) (? 1 money)))))))

;;;
;;; unify-match
;;;

;;; it should match when two patterns don't have any variables
(let ((pattern '(lives-near (Jimmy Hanks) (Bruce Eleven)))
      (conclusion '(lives-near (Jimmy Hanks) (Bruce Eleven))))
  (equal? (unify-match pattern conclusion '())
	  '()))
	  
;;; it should match when one side of patterns have variables
(let ((pattern '(lives-near (Jimmy Hanks) (Bruce Eleven)))
      (conclusion '(lives-near (? person-1) (? person-2))))
  (equal? (unify-match pattern conclusion '())
	  (list (make-binding '(? person-2) '(Bruce Eleven))
		(make-binding '(? person-1) '(Jimmy Hanks)))))

(let ((pattern '(lives-near (? person-1) (? person-2)))
      (conclusion '(lives-near (Jimmy Hanks) (Bruce Eleven))))
  (equal? (unify-match pattern conclusion '())
	  (list (make-binding '(? person-2) '(Bruce Eleven))
		(make-binding '(? person-1) '(Jimmy Hanks)))))

;;; it should match when both of patterns have different variables
(let ((pattern '(lives-near (? staff) (Bruce Eleven)))
      (conclusion '(lives-near (? person-1) (? person-2))))
  (equal? (unify-match pattern conclusion '())
	  (list (make-binding '(? person-2) '(Bruce Eleven))
		(make-binding '(? staff) '(? person-1)))))

;;; it should match failed when pattern and conclusion depend on
(eq? (unify-match '(? x) '((? x) programmer) '())
     'failed)

;;; it should match failed when pattern and conclusion are not same
(eq? (unify-match '(Jimmy Bruce) '(Bruce Eleven) '())
     'failed)

;;;
;;; extend-if-balance
;;;

;;; it should extend frame
(equal? (extend-if-balance '(? x) 10000 '())
	(list (make-binding '(? x) 10000)))

(equal? (extend-if-balance '(? x) '(? y) '())
	(list (make-binding '(? x) '(? y))))

;;; it should not extend frame if variable bound in frame is same as value extended
(let ((frame (extend-frame '(? x) 10000 '())))
  (eq? (extend-if-balance '(? x) 10000 frame)
       frame))

;;; it should not extend frame if frame binded value is same as variable extended when variable and value both are variables
(let ((frame (extend-frame '(? y) 10000 '())))
  (equal? (extend-if-balance '(? x) '(? y) frame)
	  (extend-frame '(? x) 10000 frame)))

;;; it should extend failed if two side of variables depend on
(eq? (extend-if-balance '(? x) '((? x) programmer) '())
     'failed)

;;;
;;; depends-on?
;;;

;;; it shoud not depend on when expression is not include variable
(equal? (depends-on? '(? y) '(? x) '())
	false)

;;; it should depend on when expression is include variable
(equal? (depends-on? '((? x) programmer) '(? x) '())
	true)

;;; it should depend on when the value bund in frame of expression's variable include the variable
(let ((frame (extend-frame '(? y) '((? x) programmer) '())))
  (equal? (depends-on? '(? y) '(? x) frame)
	  true))


;;;
;;; add-rule!
;;;

;;; it should  add a rule
(begin
  (reset-rules!)
  (let ((rule '(rule (lives-near ?person-1 ?person-2))))
    (add-rule! rule)
    (equal? (stream-car (get-all-rules)) rule)))


;;;
;;; rule?
;;;

;;; it should be rule when it starts by 'rule
(begin
  (equal? true
	  (rule? '(rule (lives-near ?person-1 ?person-2)))))

;;;
;;; fetch-rules
;;;

;;; it should get rules by index when rule's index is usable
(begin
  (reset-operation-table!)
  (reset-rules!)
  (let ((rule-1 '(rule (lives-near (? person-1) (? person-2))))
	(rule-2 '(rule (over-salary (? person) (? salary)))))
    (add-rule! rule-1)
    (add-rule! rule-2)
    (equal? (stream-first (fetch-rules '(lives-near (Jimmy Hanks) (Bruce Eleven))))
	    rule-1)))

;;;
;;; store-index-of-rule
;;;
(begin
  (reset-operation-table!)
  (reset-rules!)
  (let ((rule '(rule (lives-near (? person-1) (? person-2)))))
    (add-rule! rule)
    (equal? (stream-first (get 'lives-near 'rule-stream))
	    rule)))
	    