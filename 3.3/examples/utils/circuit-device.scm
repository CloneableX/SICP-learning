(define (valid-signal? s)
  (or (= s 0) (= s 1)))
(define (invalid-signal? s)
  (not (valid-signal? s)))


(define (inverter input output)
  (define (logical-not s)
    (cond ((= s 0) 1)
	  ((= s 1) 0)
	  (else (error "Invalid signal" s))))
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (and-gate a1 a2 output)
  (define (logical-and s1 s2)
    (cond ((or (invalid-signal? s1) (invalid-signal? s2))
	   (error "Invalid signal" (list s1 s2)))
	  ((and (= s1 1) (= s2 1)) 1)
	  (else 0)))
  (define (and-action-procedure)
    (let ((new-value
	   (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; exercise 3.28
(define (or-gate a1 a2 output)
  (define (logical-or s1 s2)
    (cond ((or (invalid-signal? s1) (invalid-signal? s2))
	   (error "Invalid signal" (list s1 s2)))
	  ((and (= s1 0) (= s2 0)) 0)
	  (else 1)))
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
