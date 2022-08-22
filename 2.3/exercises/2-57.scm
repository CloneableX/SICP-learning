(load "2-56.scm")

(define (augend s)
  (let ((tail (cddr s)))
    (if (= (length tail) 1)
	(car tail)
	(let ((new-sum (cons '+ tail)))
	  (make-sum (addend new-sum) (augend new-sum))))))

(define (multiplicand p)
  (let ((tail (cddr p)))
    (if (= (length tail) 1)
	(car tail)
	(let ((new-product (cons '* tail)))
	  (make-product (multiplier new-product) (multiplicand new-product))))))

(deriv '(+ x (+ x 3) (+ y 3)) 'x)
(deriv '(+ (* 3 x) (* 4 x) (* 5 x)) 'x)
(deriv '(* x y (+ x 3)) 'x)




