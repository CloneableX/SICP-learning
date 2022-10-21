(rule (reverse () ()))
(rule (reverse (?x . ?y) ?z)
      (and (append-to-from ?v (?x) ?z)
	   (reverse ?y ?v)))