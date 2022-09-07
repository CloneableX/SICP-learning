(define (make-deque)
  (let ((front-ptr '())
	(rear-ptr '()))
    (define (format-deque)
      (define (loop ptr)
	(if (null? ptr)
	    '()
	    (cons (car ptr) (loop (cddr ptr)))))
      (loop front-ptr))
    (define (empty-deque?)
      (or (null? front-ptr) (null? rear-ptr)))
    (define (get-item ptr)
      (if (empty-deque?)
	  (error "GET-ITEM called with empty deque" front-ptr)
	  (car ptr)))
    (define (front-deque) (get-item front-ptr))
    (define (rear-deque) (get-item rear-ptr))

    (define (front-delete-deque!)
      (cond ((empty-deque?)
	     (error "FRONT-DELETE-DEQUE! called with empty deque" front-ptr))
	    (else
	     (set! front-ptr (cddr front-ptr))
	     (cond ((null? front-ptr) (format-deque))
		   (else
		    (set-car! (cdr front-ptr) '())
		    (format-deque))))))
    (define (rear-delete-deque!)
      (cond ((empty-deque?)
	     (error "REAR-DELETE-DEQUE! called with empty deque" front-ptr))
	    (else
	     (set! rear-ptr (cadr rear-ptr))
	     (cond ((null? rear-ptr) (format-deque))
		   (else
		    (set-cdr! (cdr rear-ptr) '())
		    (format-deque))))))

    ;; insert item to queue
    (define (append-to-deque! front-part rear-part new-pair)
      (cond ((empty-deque?)
	     (set! front-ptr new-pair)
	     (set! rear-ptr new-pair))
	    (else
	     (set-car! (cdr rear-part) front-part)
	     (set-cdr! (cdr front-part) rear-part))))
    (define (front-insert-deque! item)
      (let ((new-pair (cons item (cons '() '()))))
	(append-to-deque! new-pair front-ptr new-pair)
	(set! front-ptr new-pair)
	(format-deque)))
    (define (rear-insert-deque! item)
      (let ((new-pair (cons item (cons '() '()))))
	(append-to-deque! rear-ptr new-pair new-pair)
	(set! rear-ptr new-pair)
	(format-deque)))
    (define (dispatch m)
      (cond ((eq? m 'empty-deque?) (empty-deque?))
	    ((eq? m 'format-deque) (format-deque))
	    ((eq? m 'front-queue) (front-deque))
	    ((eq? m 'rear-queue) (rear-deque))
	    ((eq? m 'front-insert-deque!) front-insert-deque!)
	    ((eq? m 'rear-insert-deque!) rear-insert-deque!)
	    ((eq? m 'front-delete-deque!) (front-delete-deque!))
	    ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
	    (else
	     (error "Unknown operation: MAKE-QUEUE" m))))
    dispatch))

(define (empty-deque? deque)
  (deque 'empty-deque?))
(define (front-insert-deque! deque item)
  ((deque 'front-insert-deque!) item))
(define (rear-insert-deque! deque item)
  ((deque 'rear-insert-deque!) item))
(define (front-delete-deque! deque)
  (deque 'front-delete-deque!))
(define (rear-delete-deque! deque)
  (deque 'rear-delete-deque!))

(define q1 (make-deque))

(empty-deque? q1)
(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)

(rear-insert-deque! q1 'd)
(rear-insert-deque! q1 'e)

(front-delete-deque! q1)
(front-delete-deque! q1)

(rear-delete-deque! q1)
(rear-delete-deque! q1)