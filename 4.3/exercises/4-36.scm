(define (pythagorean-triples low high)
  (define (loop triple)
    (if (pair? triple)
	(cons triple
	      (loop (try-again)))
	'()))
  (loop (a-pythagorean-triple-between low high)))