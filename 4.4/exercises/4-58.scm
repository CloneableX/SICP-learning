(rule (big-shot ?person)
      (and (job ?person (?division . ?type-1))
	   (supervisor ?person ?super)
	   (not (job ?super (?division . ?type-2)))))