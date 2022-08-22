(define (compose g f)
  (lambda (x) (g (f x))))

(define (repeated f times)
  (define (repeated-iter func count)
    (if (= count 0)
	func
	(repeated-iter (compose f func) (- count 1))))
  (repeated-iter f (- times 1)))


((repeated square 2) 5)