(load "utils/constraint.scm")
(load "utils/connector.scm")
(load "utils/constraint-system.scm")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (cv value)
  (let ((x (make-connector)))
    (constant value x)
    x))

(define (c/ product x)
  (let ((y (make-connector)))
    (multiplier x y product)
    y))


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))

(define c (make-connector))
(define f (celsius-fahrenheit-converter c))

(probe "Celsius temp" c)
(probe "Fahrenheit temp" f)

(set-value! c 25 'user)

(forget-value! c 'user)

(set-value! f 212 'user)