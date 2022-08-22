(load "utils/set-tree.scm")
(load "utils/lookup.scm")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
	((equal? given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))


(define a
  (make-tree (make-record 5 "Tim")
	     (make-tree (make-record 1 "Hank")
			'()
			(make-tree (make-record 3 "Tom") '() '()))
	     (make-tree (make-record 9 "Jimmy")
			(make-tree (make-record 7 "Susan") '() '())
			(make-tree (make-record 11 "Geoge") '() '()))))


(lookup 3 a)
(lookup 10 a)
