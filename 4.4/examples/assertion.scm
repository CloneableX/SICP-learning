(load "utils/tagged-list.scm")
(load "stream-util.scm")
(load "operation-table.scm")
(load "expression.scm")
(load "frame.scm")

(define the-assertions the-empty-stream)
(define (reset-assertions!) (set! the-assertions the-empty-stream))
(define (get-all-assertions) the-assertions)

(define (add-assertion! assertion)
  (store-index-of-assertion assertion)
  (let ((old-assertions the-assertions))
    (set! the-assertions (cons-stream assertion old-assertions))
    'ok))
(define (store-index-of-assertion assertion)
  (let ((current-stream (get-indexed-assertions assertion)))
    (put (index-key-of assertion) 
	 'assertion-stream 
	 (cons-stream assertion current-stream))))

(define (find-assertions query frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum query frame))
   (fetch-assertions query)))

(define (check-an-assertion assertion pattern frame)
  (let ((match-result (pattern-match assertion pattern frame)))
    (if (eq? match-result 'failed)
	the-empty-stream
	match-result)))
(define (pattern-match assertion pattern frame)
  (cond ((eq? frame 'failed) 'failed)
	((var? pattern) (extend-if-consistent pattern assertion frame))
	((and (pair? assertion) (pair? pattern))
	 (check-an-assertion
	  (cdr assertion) 
	  (cdr pattern)
	  (check-an-assertion (car assertion) (car pattern) frame)))
	((equal? assertion pattern) frame)
	(else 'failed)))

(define (extend-if-consistent variable value frame)
  (let ((binding (binding-in-frame variable frame)))
    (if binding
	(check-an-assertion (binding-value binding) value frame)
	(extend-frame variable value frame))))

(define (fetch-assertions pattern)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))
(define (get-indexed-assertions exp)
  (get-stream (index-key-of exp) 'assertion-stream))


