(rule (last-pair (?x) ?x))
(rule (last-pair (?first . ?rest) ?x)
      (last-pair ?rest ?x))