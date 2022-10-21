(rule ((grandson) ?x ?y)
      (grandson ?x ?y))
(rule (end-with-grandson (grandson)))
(rule (end-with-grandson (?x . ?y)) (end-with-grandson ?y))

(rule ((great . ?rel) ?x ?y)
      (and (son ?f ?y)
	   (?rel ?x ?f)
	   (end-with-grandson ?rel)))