(load "prime.scm")

(equal? (fast-prime? 561 3) (prime? 561))
(equal? (fast-prime? 1105 3) (prime? 1105))
(equal? (fast-prime? 1729 3) (prime? 1729))
(equal? (fast-prime? 2465 3) (prime? 2465))
(equal? (fast-prime? 2821 3) (prime? 2821))