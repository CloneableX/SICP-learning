(load "utils/enumerate-interval.scm")
(load "utils/accumulate.scm")
(load "utils/prime.scm")

(define (flat-map proc sequence)
  (accumulate append '()
	      (map proc sequence)))

(define (unique-pairs n)
  (flat-map (lambda (i)
	      (map (lambda (j) (list i j))
		   (enumerate-interval 1 (- i 1))))
	    (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ((a (car pair))
	(b (cadr pair)))
    (list a b (+ a b))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


(prime-sum-pairs 6)