(define (application? exp) (tagged-list? 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))

