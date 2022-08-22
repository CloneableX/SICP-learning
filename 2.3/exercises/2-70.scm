(load "utils/huffman-tree.scm")
(load "2-68.scm")
(load "2-69.scm")

(define sample-tree (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9))))

(define message '(Get a job Sha na na na na na na na na Get a job Sha na na na na na na na na Wah yip yip yip yip yip yip yip yip yip Sha boom))

(define sample-message (encode message sample-tree))
(length sample-message)
(* 3 (length message))
