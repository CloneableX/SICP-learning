(load "utils/tagged-list.scm")
(load "expression.scm")
(load "assertion.scm")
(load "rule.scm")

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp) (content exp))
