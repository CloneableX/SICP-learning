(load "rule-or-assertion.scm")

;;;
;;; add-rule-or-assertion!
;;;

;;; add assertion
(begin
  (reset-assertions!)
  (let ((assertion '(job (Jimmy Hanks) (computer programmer))))
    (add-rule-or-assertion! assertion)
    (equal? (stream-car (get-all-assertions)) assertion)))

;;; add rule
(begin
  (reset-rules!)
  (let ((rule '(rule (lives-near ?person-1 ?person-2))))
    (add-rule-or-assertion! rule)
    (equal? (stream-car (get-all-rules)) rule)))


;;;
;;; assertion-to-be-added?
;;;

;;; it should be assertion to be added when it starts by 'assert!
(begin
  (let ((added-assertion '(assert! (job (Jimmy Hanks) (computer programmer)))))
    (equal? (assertion-to-be-added? added-assertion) true)))


;;;
;;; add-assertion-body
;;;

;;; it should extract assertion body added
(begin
  (let ((added-assertion '(assert! (job (Jimmy Hanks) (computer programmer)))))
    (equal? (add-assertion-body added-assertion) '(job (Jimmy Hanks) (computer programmer)))))