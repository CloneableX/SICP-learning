(load "utils/constraint.scm")
(load "utils/connector.scm")
(load "utils/constraint-system.scm")

(define (averager a b c)
  (let ((u (make-connector))
	(w (make-connector)))
    (multiplier c w u)
    (adder a b u)
    (constant 2 w)
    'ok))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe 'A a)
(probe 'B b)
(probe 'C c)

(averager a b c)

(set-value! a 4 'user)
(set-value! b 2 'user)