(define (p-triangle row col)
  (cond ((or (< col 1) (> col row)) 0)
	((= row 1) 1)
	(else (+ (p-triangle (- row 1)
			     col)
		 (p-triangle (- row 1)
			     (- col 1))))))

(p-triangle 3 3)
(p-triangle 4 3)
(p-triangle 5 3)
			  