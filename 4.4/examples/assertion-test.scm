(load "test-util.scm")
(load "assertion.scm")


;;;
;;; extend-if-consistent
;;;

;;; it should extend frame when variable is unbund in this frame
(equal? (extend-if-consistent '(? x) '(Jimmy Hanks) '())
	(list (make-binding '(? x) '(Jimmy Hanks)))))

;;; it should return check assertion result when variable is bund in frame
(let ((frame (list (make-binding '(? x) '(Jimmy Hanks)))))
  (eq? (extend-if-consistent '(? x) '(Jimmy Hanks) frame)
       frame))

;;;
;;; find-assertions
;;;

;;; it should find assertion matched pattern
(begin
  (before-test-reset!)
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(query '(job (? x) (computer programmer)))
	(frame '()))
    (add-assertion! assertion)
    (equal? (stream-first
	     (find-assertions query frame))
	    (list (make-binding '(? x) '(Jimmy Hanks))))))

;;;
;;; add-assertion!
;;;

;;; add one assertion
(begin
  (reset-assertions!)
  (let ((assertion '(job (Jimmy Hanks) (computer programmer))))
    (add-assertion! assertion)
    (equal? (stream-car (get-all-assertions)) assertion)))


;;;
;;; check-an-assertion
;;;

;;; it should be checked pass when assertion is consist with pattern same as assertion
(begin
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(frame '()))
    (equal? (check-an-assertion 
	     assertion 
	     '(job (Jimmy Hanks) (computer programmer))
	     frame)
	    '())))

;;; it should match pattern when pattern has variable in same site as assertion datum
(begin
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(pattern '(job (? x) (computer programmer)))
	(frame '()))
    (equal? (check-an-assertion assertion pattern frame)
	    (extend-frame '(? x) '(Jimmy Hanks) '()))))

;;; it should match pattern with multiple variables
(begin
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(pattern '(job (? x) (? job)))
	(frame '())
	(result-frame
	 (list (make-binding '(? job) '(computer programmer))
	       (make-binding '(? x) '(Jimmy Hanks)))))
    (equal? (check-an-assertion assertion pattern frame) result-frame)))

;;; it should match pattern with nested varialbes
(begin
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(pattern '(job (? x) ((? division) (? type))))
	(frame '())
	(result-frame
	 (list (make-binding '(? type) 'programmer)
	       (make-binding '(? division) 'computer)
	       (make-binding '(? x) '(Jimmy Hanks)))))
    (equal? (check-an-assertion assertion pattern frame) result-frame)))

;;; it should return empty stream when match failed
(begin
  (stream-null? (check-an-assertion
		 '(job (Jimmy Hanks) (computer programmer))
		 '(job (Jimmy Hanks))
		 '())))

;;; it should check assertions in frame with variables
(begin
  (let ((assertion '(job (Jimmy Hanks) (computer programmer)))
	(pattern '(job (? x) (computer programmer)))
	(frame (list (make-binding '(? x) '(Jimmy Hanks)))))
    (eq? (check-an-assertion assertion pattern frame)
	 frame)))

;;; it should return 'failed when match failed
(let ((assertion '(job (Jimmy Hanks) (computer programmer)))
      (pattern '(salary (? x)))
      (frame '()))
  (eq? (check-an-assertion assertion pattern frame)
       the-empty-stream))


;;;
;;; fetch-assertions
;;;

;;; it should fetch assertions by assertion index
(begin
  (reset-operation-table!)
  (reset-assertions!)
  (let ((assertion-1 '(job (Jimmy Hanks) (computer programmer)))
	(assertion-2 '(salary (Jimmy Hanks) 10000))
	(query '(job (? x) (? y))))
    (add-assertion! assertion-1)
    (add-assertion! assertion-2)
    (equal? (stream-length (fetch-assertions query)) 1)))

;;; it should fetch all assertions when pattern starts with a variable
(begin
  (reset-operation-table!)
  (reset-assertions!)
  (let ((assertion-1 '(job (Jimmy Hanks) (computer programmer)))
	(assertion-2 '(salary (Jimmy Hanks) 10000))
	(query '((? type) (Jimmy Hanks) (? job))))
    (add-assertion! assertion-1)
    (add-assertion! assertion-2)
    (equal? (fetch-assertions query) (get-all-assertions))))









