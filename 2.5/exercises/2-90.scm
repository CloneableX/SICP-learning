(define (install-sparse-poly-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))

  ;; interfaces to rest of the system
  (define (tag term-list) (attach-tag 'sparse-poly term-list))
  (put 'adjon-term '(polynomial sparse-poly)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'rest-terms '(sparse-poly)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'first-term '(sparse-poly) first-term)
  (put 'empty-termlist? '(sparse-poly) empty-termlist?)
  'done)


(define (install-dense-poly-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (let ((t1 (first-term term-list)))
      (if (>= (order term) (order t1))
	  (cons (coeff term) term-list)
	  (cons (coeff t1)
		(adjon-term term (rest-terms term-list))))))
  (define (empty-termlist? term-list) (null? term-list))
  (define (first-term term-list)
    (make-term (- (length term-list) 1)
	       (car term-list)))
  (define (rest-terms term-list) (cdr term-list))

  ;; interfaces to rest of the system
  (define (tag term-list) (attach-tag 'dense-poly term-list))
  (put 'adjoin-term '(polynomial dense-poly)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'rest-terms '(dense-poly)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'first-term '(dense-poly) first-term)
  (put 'empty-termlist? '(dense-poly) empty-termlist?)
  'done)

(define (install-polynomial-package)
  (define (adjoin-term term term-list)
    (apply-generic 'adjon-term term term-list))
  (define (rest-terms term-list)
    (apply-generic 'rest-term term-list))
  (define (empty-termlist? term-list)
    (apply-generic 'empty-termlist? term-list))
  (define (first-term term-list)
    (apply-generic 'first-term term-list))
  (define (the-empty-termlist) '())

  (put 'make 'poly-term make-term)
  'done)

(define (make-term order coeff)
  ((get 'make 'poly-term) order coeff))

