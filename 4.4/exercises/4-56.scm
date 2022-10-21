(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?where))

(and (salary ?person ?salary-1)
     (salary (Bitdiddle Ben) ?salary-2)
     (lisp-value < ?salary-1 ?salary-2))

(and (supervisor ?person ?super)
     (not (job ?super (computer . ?type))))