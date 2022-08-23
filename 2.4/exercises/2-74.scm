(define (division datum)
  (if (pair? datum)
      (car datum)
      (error "Bad division: DIVISION" datum)))

;; file -> (cons 'xx-division file)
(define (get-record file given-key)
  ((get 'get-record (division file)) file given-key))


;; record -> (cons 'xx-division record)
(define (get-salary record)
  ((get 'get-salary (division record)) record))


(define (find-employee-record name files)
  (let ((divisions (map division files)))
    (filter
     (lambda (x) (not (null? x)))
     (map
      (lambda (division) ((get 'find-employee-record division) name))
      divisions))))