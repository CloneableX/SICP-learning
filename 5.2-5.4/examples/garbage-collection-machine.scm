(load "simulator.scm")

(define default-vector-length 10)

(define (make-default-vector) (make-vector 10))
(define (ljust-vector vector length)
  (let ((rest-length (- length (vector-length vector))))
    (vector-append vector
		   (make-vector rest-length))))

(define (subvector-from-machine machine register-name start end)
  (subvector
   (get-register-contents machine register-name)
   start end))

(define (make-typed-pointer type val)
  (string->symbol
   (string-append type
		  (number->string val))))
(define (pointer-index pointer)
  (let ((pointer-str (symbol->string pointer)))
    (string->number
     (substring pointer-str 1 (string-length pointer-str)))))
  
(define (pointer-to-pair? pointer)
  (cond ((not (symbol? pointer)) false)
	(else
	 (let ((type (substring (symbol->string pointer) 0 1)))
	   (equal? type "p")))))
(define (broken-heart? val)
  (eq? val 'broken-heart))

(define (typed-pointer-vector-ref vector pointer)
  (cond ((number? pointer)
	 (vector-ref vector pointer))
	((pointer-to-pair? pointer)
	 (vector-ref vector (pointer-index pointer)))
	(else pointer)))

(define (typed-pointer-vector-set! vector typed-pointer val)
  (let ((index (pointer-index typed-pointer)))
    (vector-set! vector index val)))
(define (typed-pointer-vector-set-pointer! vector typed-pointer index)
  (let ((pointer (make-typed-pointer "p" index)))
    (typed-pointer-vector-set! vector typed-pointer pointer)))

(define (make-garbage-collection-machine
	 root the-cars the-cdrs new-cars new-cdrs)
  (let ((machine (make-machine '(root 
				 free scan old new oldcr relocate-continue temp
				 the-cars the-cdrs new-cars new-cdrs)
			       (list
				(list 'pointer-to-pair? pointer-to-pair?)
				(list 'broken-heart? broken-heart?)
				(list 'vector-ref typed-pointer-vector-ref)
				(list 'vector-set! vector-set!)
				(list 'typed-pointer-vector-set! typed-pointer-vector-set!)
				(list 'typed-pointer-vector-set-pointer! typed-pointer-vector-set-pointer!)
				(list '= =)
				(list '+ +))
			       '(begin-garbage-collection
				   (assign free (const 0))
				   (assign scan (const 0))
				   (assign old (reg root))
				   (assign relocate-continue (label reassign-root))
				   (goto (label relocate-old-result-in-new))
				 reassign-root
				   (assign root (reg new))
				   (goto (label gc-loop))
				 gc-flip
				   (assign temp (reg the-cars))
				   (assign the-cars (reg new-cars))
				   (assign new-cars (reg temp))
				   (assign temp (reg the-cdrs))
				   (assign the-cdrs (reg new-cdrs))
				   (assign new-cdrs (reg temp))
				   (goto (label collection-done))
				 gc-loop
				   (test (op =) (reg scan) (reg free))
				   (branch (label gc-flip))
				   (assign old (op vector-ref) (reg new-cars) (reg scan))
				   (assign relocate-continue (label update-car))
				   (goto (label relocate-old-result-in-new))
				 relocate-old-result-in-new
				   (test (op pointer-to-pair?) (reg old))
				   (branch (label copy-pair))
				   (assign new (reg old))
				   (goto (reg relocate-continue))
				 update-car
				   (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
				   (assign old (op vector-ref) (reg new-cdrs) (reg scan))
				   (assign relocate-continue (label update-cdr))
				   (goto (label relocate-old-result-in-new))
				 update-cdr
				   (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
				   (assign scan (op +) (reg scan) (const 1))
				   (goto (label gc-loop))
				 copy-pair
				   (assign oldcr (op vector-ref) (reg the-cars) (reg old))
				   (test (op broken-heart?) (reg oldcr))
				   (branch (label already-moved))
				   (perform (op vector-set!) (reg new-cars) (reg free) (reg oldcr))
				   (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
				   (perform (op vector-set!) (reg new-cdrs) (reg free) (reg oldcr))
				   (perform (op typed-pointer-vector-set!) (reg the-cars) (reg old) (const broken-heart))
				   (perform (op typed-pointer-vector-set-pointer!) (reg the-cdrs) (reg old) (reg free))
				   (assign new (reg free))
				   (assign free (op +) (reg free) (const 1))
				   (goto (reg relocate-continue))
				 already-moved
				   (assign new (op vector-ref) (reg the-cdrs) (reg old))
				   (goto (reg relocate-continue))
				 collection-done))))
    (set-register-contents! machine 'root root)
    (set-register-contents! machine 'the-cars the-cars)
    (set-register-contents! machine 'the-cdrs the-cdrs)
    (set-register-contents! machine 'new-cars new-cars)
    (set-register-contents! machine 'new-cdrs new-cdrs)
    machine))