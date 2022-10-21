(load "utils/table.scm")

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define (reset-operation-table!) 
  (set! operation-table (make-table))
  (set! get (operation-table 'lookup-proc))
  (set! put (operation-table 'insert-proc!))
  'ok)

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))
