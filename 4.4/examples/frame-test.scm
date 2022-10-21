(load "frame.scm")

;;;
;;; merge-if-balance
;;;

;;; it should merge if merged frame has all variables
(let ((frame1 (extend-frame '(? x) '(Jimmy Hanks) '()))
      (frame2 (extend-frame '(? y) '(Bruce Eleven) '()))
      (exps '((job (? x)) (salary (? y)))))
  (equal? (merge-if-balance frame1 frame2 exps)
	  (list (make-binding '(? y) '(Bruce Eleven))
		(make-binding '(? x) '(Jimmy Hanks)))))

;;; it should not merge if merged frame locks variables


;;;
;;; merge-frames
;;;

;;; it should merge frame if two frames has different variables
(let ((frame1 (extend-frame '(? x) '(Jimmy Hanks) '()))
      (frame2 (extend-frame '(? y) '(Bruce Eleven) '())))
  (equal? (merge-frames frame1 frame2)
	  (list (make-binding '(? y) '(Bruce Eleven))
		(make-binding '(? x) '(Jimmy Hanks)))))

;;; it should merge frame if two frames has same variables and their values also are same
(let ((frame1 (list (make-binding '(? x) '(Jimmy Hanks))
		    (make-binding '(? y) '(Bruce Eleven))))
      (frame2 (extend-frame '(? y) '(Bruce Eleven) '())))
  (equal? (merge-frames frame1 frame2)
	  (list (make-binding '(? y) '(Bruce Eleven))
		(make-binding '(? x) '(Jimmy Hanks)))))

(let ((frame1 (extend-frame '(? x) '(Jimmy Hanks) '()))
      (frame2 '()))
  (equal? (merge-frames frame1 frame2)
	  (list (make-binding '(? x) '(Jimmy Hanks)))))

;;; it should not merge if two frames has same variables but their values are different
(let ((frame1 (extend-frame '(? x) '(Jimmy Hanks) '()))
      (frame2 (extend-frame '(? x) '(Bruce Eleven) '())))
  (null? (merge-frames frame1 frame2)))

;;;
;;; instantiate
;;;

;;; it should return origin expression if there's not any variables
(begin
  (let ((actual
	 (instantiate
	  '(job (Jimmy Hanks) (computer programmer))
	  (extend-frame '(? x) '(Jimmy Hanks) '())
	  (lambda (v f) v))))
    (equal? actual
	    '(job (Jimmy Hanks) (computer programmer)))))

;;; it should return expression by same frame varialbes
(begin
  (let ((exp '(job (? x) (computer programmer)))
	(frame (list (make-binding '(? x) '(Jimmy Hanks)))))
    (equal? (instantiate exp frame (lambda (v f) v))
	    '(job (Jimmy Hanks) (computer programmer)))))

;;; it should handle unbind variable as self-definition procedure
(begin
  (let ((exp '(job (? x) (computer programmer)))
	(frame '()))
    (equal? (instantiate exp frame
			 (lambda (v f) (constract-question-mark v)))
	    '(job ?x (computer programmer)))))

;;; it should find final value if there's middle variable
(let ((exp '(over-salary (? x) 10000))
      (frame (list (make-binding '(? x) '(? person))
		   (make-binding '(? person) '(Jimmy Hanks)))))
  (equal? (instantiate exp frame
		       (lambda (v f) v))
	  '(over-salary (Jimmy Hanks) 10000)))