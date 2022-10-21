(load "expression.scm")

;;;
;;; type
;;;

;;; it should get type of expression
(begin
  (let ((exp '(assert! '(salary Tom 100))))
    (eq? (type exp) 'assert!)))

;;; it should throw error when expression is not pair
(begin
  (type 'asssert!))


;;;
;;; content
;;;

;;; it should get content of expression
(begin
  (let ((exp '(assert! (salary Tom 100))))
    (equal? (content exp) '(salary Tom 100))))


;;;
;;; var?
;;;

;;; it should be variable when it starts by '?
(begin
  (let ((exp '(? x)))
    (equal? (var? exp) true)))


;;;
;;; query-syntax-process
;;;

;;; it should return result same as expression when there's not any variable in expresion
(begin
  (let ((exp '(job (Jimmy Hanks) (computer programmer))))
    (equal? (query-syntax-process exp)
	    '(job (Jimmy Hanks) (computer programmer)))))

;;; it should transform variable from symbol to symbol list starts by question mark
(begin
  (let ((exp '(job ?x (computer programmer))))
    (equal? (query-syntax-process exp)
	    '(job (? x) (computer programmer)))))

(begin
  (let ((exp '(job ?x (?division  ?type))))
    (query-syntax-process exp)))