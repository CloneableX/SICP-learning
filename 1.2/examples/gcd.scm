(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 12 4)
(gcd 120 74)