(load "garbage-collection-machine.scm")

;;;
;;; copy-machine
;;;

;;; it should copy number data to free memory
(let ((root 'n4)
      (the-cars (ljust-vector (vector 'n1) default-vector-length))
      (the-cdrs (ljust-vector (vector 'p4) default-vector-length))
      (new-cars (make-default-vector))
      (new-cdrs (make-default-vector)))
  (let ((machine
	 (make-garbage-collection-machine root
					  the-cars the-cdrs
					  new-cars new-cdrs)))
    (start machine)
    (let ((actual (vector-append
		   (get-register-contents machine 'the-cars)
		   (get-register-contents machine 'the-cdrs))))
      (equal? actual
	      (make-vector 20)))))

;;; it should copy pair data to free memory
(let ((root 'p0)
      (the-cars (ljust-vector (vector 'n1) default-vector-length))
      (the-cdrs (ljust-vector (vector 'e0) default-vector-length))
      (new-cars (make-default-vector))
      (new-cdrs (make-default-vector)))
  (let ((machine
	 (make-garbage-collection-machine root
					  the-cars the-cdrs
					  new-cars new-cdrs)))
    (start machine)
    (let ((actual (vector-append
		   (subvector-from-machine machine 'the-cars 0 1)
		   (subvector-from-machine machine 'the-cdrs 0 1))))
      (equal? actual
	      (vector 'n1 'e0)))))

;;; it should update pair copied to broken heart
(let ((root 'p0)
      (the-cars (ljust-vector (vector 'n1) default-vector-length))
      (the-cdrs (ljust-vector (vector 'e0) default-vector-length))
      (new-cars (make-default-vector))
      (new-cdrs (make-default-vector)))
  (let ((machine
	 (make-garbage-collection-machine root
					  the-cars the-cdrs
					  new-cars new-cdrs)))
    (start machine)
    (let ((actual (vector-append
		   (subvector-from-machine machine 'new-cars 0 1)
		   (subvector-from-machine machine 'new-cdrs 0 1))))
      (equal? actual
	      (vector 'broken-heart 'p0)))))

;;; it should copy multiple relational data in root to free memory
(let ((root 'p0)
      (the-cars (ljust-vector (vector 'n1 'n2) default-vector-length))
      (the-cdrs (ljust-vector (vector 'p1 'e0) default-vector-length))
      (new-cars (make-default-vector))
      (new-cdrs (make-default-vector)))
  (let ((machine
	 (make-garbage-collection-machine root
					  the-cars the-cdrs
					  new-cars new-cdrs)))
    (start machine)
    (let ((actual (vector-append
		   (subvector-from-machine machine 'the-cars 0 2)
		   (subvector-from-machine machine 'the-cdrs 0 2))))
      (equal? actual			
	      (vector 'n1 'n2 'p1 'e0)))))

(let ((root 'p0)
      (the-cars (ljust-vector (vector 'n1 'n2) default-vector-length))
      (the-cdrs (ljust-vector (vector 'p1 'e0) default-vector-length))
      (new-cars (make-default-vector))
      (new-cdrs (make-default-vector)))
  (let ((machine
	 (make-garbage-collection-machine root
					  the-cars the-cdrs
					  new-cars new-cdrs)))
    (start machine)
    (equal? (subvector-from-machine machine 'the-cars 0 3)
	    (vector 'n1 'n2 false))))