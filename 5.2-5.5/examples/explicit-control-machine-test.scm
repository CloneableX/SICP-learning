(load "explicit-control-machine.scm")

(define evaluate-endpoint '(label evaluate-done))

;;;
;;; simple expression
;;;

;;; it should return expression self when it's number or string
(let ((machine (make-explicit-control-machine)))
  (let ((result (machine-evaluate machine 3 the-empty-environment evaluate-endpoint)))
    (equal? result 3)))

(let ((machine (make-explicit-control-machine)))
  (let ((result (machine-evaluate machine "hello" the-empty-environment evaluate-endpoint)))
    (equal? result "hello")))

;;; it should lookup value in environment by variable
(let ((machine (make-explicit-control-machine))
      (env (extend-environment '(x) '(3) the-empty-environment)))
  (let ((result (machine-evaluate machine 'x env evaluate-endpoint)))
    (equal? result 3)))

;;; it should text of quote when expression is quote
(let ((machine (make-explicit-control-machine))
      (env the-empty-environment))
  (let ((result (machine-evaluate machine '(quote x) env evaluate-endpoint)))
    (eq? result 'x)))

;;; it should transfer lambda expression to procedure
(let ((machine (make-explicit-control-machine))
      (env the-empty-environment)
      (lambda-exp '(lambda (x) x)))
  (let ((result (machine-evaluate machine lambda-exp env evaluate-endpoint)))
    (equal? result
	    '(procedure (x) (x) ()))))


;;;
;;; procedure and compound expression
;;;

;;; it should compute primitive procedure
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(+ 1 2)))
  (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
    (equal? result 3)))

;;; it should compute operands subexpression
(let ((machine (make-explicit-control-machine))
      (env (extend-environment '(x) '(3) the-global-environment))
      (exp '(+ x x)))
  (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
    (equal? result 6)))

;;; it should compute express sequences
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(begin
	      (+ 1 2)
	      (+ 2 3)
	      (+ 3 4))))
  (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
    (equal? result 7)))

;;; it should compute compound express without parameters
(let ((comp-proc (make-procedure '() '((quote hello)) the-global-environment)))
  (let ((machine (make-explicit-control-machine))
	(env the-global-environment)
	(exp '(say-hello)))
    (define-variable! 'say-hello comp-proc the-global-environment)
    (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
      (eq? result 'hello))))

;;; it should compute compound procedure
(let ((comp-proc (make-procedure '(x y)
				 '((+ x y)
				   (y))
				 the-global-environment)))
  (define-variable! 'add comp-proc the-global-environment)
  (let ((machine (make-explicit-control-machine))
	(env the-global-environment)
	(exp '(add 3 4)))
    (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
      (equal? result 4))))


;;;
;;; special form
;;;

;;; it should execute consequent expression when predicate condition is true
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(if (< 0 1)
		(+ 1 2)
		(+ 2 2))))
  (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
    (equal? result 3)))

;;; it should execute alternative expression when predicate condition is false
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(if (< 1 0)
		(+ 1 2)
		(+ 2 2))))
  (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
    (equal? result 4)))

;;; it should modify variable's value into environment
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(set! x (+ 1 2))))
  (define-variable! 'x '*unassigned* the-global-environment)
  (machine-evaluate machine exp env evaluate-endpoint)
  (equal? (lookup-variable-value 'x the-global-environment)
	  3))

;;; it should create association of variable with value in environment
(let ((machine (make-explicit-control-machine))
      (env the-global-environment)
      (exp '(define x (+ 1 2))))
  (machine-evaluate machine exp env evaluate-endpoint)
  (equal? (lookup-variable-value 'x the-global-environment)
	  3))
(begin
  (load "explicit-control-machine.scm")
  (let ((machine (make-explicit-control-machine))
	(env the-global-environment)
	(exp '(begin
		(define (add x y) (+ x y))
		(add 3 4))))
    (let ((result (machine-evaluate machine exp env evaluate-endpoint)))
      (equal? result 7))))

	  