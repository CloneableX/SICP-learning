(load "utils/tagged-list.scm")

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (content exp)
  (if (pair? exp)
      (cadr exp)
      (error "Unknown expression CONTENT" exp)))

(define (var? exp)
  (tagged-list? exp '?))

(define (query-syntax-process exp)
  (cond ((pair? exp)
	 (cons (query-syntax-process (car exp))
	       (query-syntax-process (cdr exp))))
	((symbol? exp) (expand-question-mark exp))
	(else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
	(list '?
	      (string->symbol
	       (substring chars 1 (string-length chars))))
	symbol)))

(define (index-key-of exp) (type exp))
(define (use-index? exp) (symbol? (car exp)))



  

