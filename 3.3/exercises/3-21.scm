(load "utils/queue.scm")

(define (print-queue queue)
  (front-ptr queue))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)

(delete-queue! q1)
(print-queue q1)
