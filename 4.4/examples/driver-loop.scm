(load "rule-or-assertion.scm")
(load "qeval.scm")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (annouce-output string)
  (newline) (display string) (newline))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (query-driver-loop)
  (prompt-for-input ";;; Query input: ")
  (let ((query (query-syntax-process (read))))
    (if (assertion-to-be-added? query)
	(begin
	  (add-rule-or-assertion! (add-assertion-body query))
	  (newline)
	  (display "Assertion added to data base")
	  (query-driver-loop))
	(begin
	  (annouce-output ";;; Query results: ")
	  (display-stream
	   (stream-map
	    (lambda (frame)
	      (instantiate
	       query
	       frame
	       (lambda (v f) (constract-question-mark v))))
	    (qeval query (singleton-stream '()))))
	  (query-driver-loop)))))


