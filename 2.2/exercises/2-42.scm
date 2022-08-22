(load "utils/enumerate-interval.scm")
(load "utils/accumulate.scm")
(load "utils/flat-map.scm")

(define empty-board '())

(define (adjoin-position new-row k positions)
  (cons new-row positions))

(define (safe? k positions)
  (define (iter current-pos other-pos i)
    (if (null? other-pos)
	true
	(if (or (= current-pos (car other-pos))
		(= current-pos (+ i (car other-pos)))
		(= current-pos (- (car other-pos) i)))
	    false
	    (iter current-pos (cdr other-pos) (+ i 1)))))
  (iter (car positions)
	(cdr positions)
	1))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter
	 (lambda (positions) (safe? k positions))
	 (flat-map
	  (lambda (rest-of-queens)
	    (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))


(for-each (lambda (pos)
	    (begin
	      (newline)
	      (display pos)))
	  (queens 8))




