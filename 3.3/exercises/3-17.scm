(define pairs
  (let ((exists-pairs (list '())))
    (define (include-in-pairs? x)
      (let ((target (filter (lambda (pair) (eq? x pair)) exists-pairs)))
	(if (null? target)
	    false
	    (car target))))
    (define (add-to-pairs! x)
      (set! exists-pairs
	    (cons x exists-pairs))
      exists-pairs)
    (define (dispatch m)
      (cond ((eq? m 'include-in-pairs?) include-in-pairs?)
	    ((eq? m 'add-to-pairs!) add-to-pairs!)
	    ((eq? m 'reset!) (set! exists-pairs (list '())))
	    ((eq? m 'exists-pairs) exists-pairs)
	    (else (error "Unknown type: DISPATCH" m))))
    dispatch))

(define (include-in-pairs? x pairs)
  ((pairs 'include-in-pairs?) x))
(define (add-to-pairs! x pairs)
  ((pairs 'add-to-pairs!) x))
(define (reset-pairs!)
  (pairs 'reset!))
(define (exists-pairs)
  (pairs 'exists-pairs))

(define (count-pairs x)
  (reset-pairs!)
  (define (loop pair)
    (cond ((not (pair? pair)) 0)
	  ((include-in-pairs? pair pairs) 0)
	  (else
	   (add-to-pairs! pair pairs)
	   (+ (loop (car pair))
	      (loop (cdr pair))
	      1))))
  (loop x))

(count-pairs (cons (cons 1 '()) (cons 2 '())))

(define two (list 1 2))
(count-pairs (cons two (cdr two)))

(define one (list 1))
(define three (cons one one))
(count-pairs (cons three three))

(load "utils/last-pair.scm")
(define cycle (list 1 2 3))
(set-cdr! (last-pair cycle) cycle)
(count-pairs cycle)


