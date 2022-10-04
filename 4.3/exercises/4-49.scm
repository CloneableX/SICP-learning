(define (an-element-of items)
  (require (not (null? items)))
  (amb (car itesm) (an-element-of (cdr items))))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (set! *unparsed* (cdr *unparsed*))
  (an-element-of (cdr word-list)))