(rule (grandsons ?person)
      (and (son ?person ?father)
	   (son ?father ?grandson)))

(grandsons Cain)
(grandsons Methushael)


(rule (sons ?person)
      (or (son ?person ?son-1)
	  (and (wife ?person ?husband)
	       (son ?husband ?son-2))))

(sons Lamech)