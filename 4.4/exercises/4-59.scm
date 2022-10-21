(meeting ?division (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
      (or
       (and (job ?person (?division . ?type))
	    (meeting ?division ?day-and-time))
       (meeting whole-company ?day-and-time)))

(meeting-time (Hakcer Alyssa P) (Wednesday ?time))