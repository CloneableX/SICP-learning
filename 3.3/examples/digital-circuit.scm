(load "utils/queue.scm")
(load "utils/wire.scm")
(load "utils/circuit-device.scm")
(load "utils/agenda.scm")

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))

(define (probe name wire)
  (add-action! wire
	       (lambda ()
		 (newline)
		 (display name) (display " ")
		 (display (current-time the-agenda))
		 (display "  New-value = ")
		 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (half-adder a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

;; exercise 3.29
(define (or-gate a1 a2 output)
  (let ((b (make-wire))
	(c (make-wire))
	(d (make-wire)))
    (inverter a1 b)
    (inverter a2 c)
    (add-gate b c d)
    (inverter d output)
    'ok))

;; exercise 3.30
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder a-wires b-wires c s-wires)
  (if (null? a-wires)
      'ok
      (let ((c-out (make-wire)))
	(full-adder (car a-wires)
		    (car b-wires)
		    c
		    (car s-wires)
		    c-out)
	(ripple-carry-adder (cdr a-wires)
			    (cdr b-wires)
			    c-out
			    (cdr s-wires)))))
  

