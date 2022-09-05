(define (make-queue)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
	  (error "FRONT called with empty queue" front-ptr)
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-queue?)
	       (set! front-ptr new-pair)
	       (set! rear-ptr new-pair)
	       front-ptr)
	      (else
	       (set-cdr! rear-ptr new-pair)
	       (set! rear-ptr new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
	     (error "DELETE-QUEUE! called with empty queue" front-ptr))
	    (else
	     (set! front-ptr (cdr front-ptr))
	     front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    (else
	     (error "Unknown operation: MAKE-QUEUE" m))))
    dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))
(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))
(define (delete-queue! queue)
  (queue 'delete-queue!))

(define x (make-queue))
(empty-queue? x)

(insert-queue! x 'a)
(insert-queue! x 'b)

(front-queue x)

(delete-queue! x)
(front-queue x)

(empty-queue? x)
(delete-queue! x)
(empty-queue? x)
				  
