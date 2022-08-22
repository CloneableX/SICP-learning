(load "utils/enumerate-interval.scm")
(load "utils/accumulate.scm")

(define (flat-map proc sequence)
  (accumulate append '()
	      (map proc sequence)))

(define (unique-pairs n)
  (flat-map (lambda (i)
	      (map (lambda (j) (list i j))
		   (enumerate-interval 1 (- i 1))))
	    (enumerate-interval 1 n)))

(define (unique-triples n)
  (flat-map (lambda (i)
	      (map (lambda (j) (cons i j))
		   (unique-pairs (- i 1))))
	    (enumerate-interval 1 n)))
	    
(define (triple-sum-eq? triple result)
  (= result
     (accumulate + 0 triple)))

(define (eq-sum-triples n sum)
  (filter (lambda (triple) (triple-sum-eq? triple sum))
	  (unique-triples n)))

(eq-sum-triples 6 10)