(define (psc level index)
  (if (or (= index 1)
	  (= index level))
      1
      (+ (psc (- level 1)
	      index)
	 (psc (- level 1)
	      (- index 1)))))

(psc 5 3)