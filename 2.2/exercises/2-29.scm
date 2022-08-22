(define (make-mobile left right)
  (list left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (make-mobile left right) (cons left right))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

(define (make-branch length structure) (cons length structure))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define (not-mobile? structure)
  (not (pair? structure)))
(define (total-weight mobile)
  (if (not-mobile? mobile)
      mobile
      (+ (total-weight (branch-structure (left-branch mobile)))
	 (total-weight (branch-structure (right-branch mobile))))))
(define (total-length mobile)
  (if (not-mobile? mobile)
      0
      (+ (branch-length (left-branch mobile))
	 (branch-length (right-branch mobile))
	 (total-length (branch-structure (left-branch mobile)))
	 (total-length (branch-structure (right-branch mobile))))))

(define (branch-priority branch)
  (let ((m (branch-structure branch)))
    (* (+ (branch-length branch)
	  (total-length m))
       (total-weight m))))
(define (mobile-balanced? mobile)
  (= (branch-priority (left-branch mobile))
     (branch-priority (right-branch mobile))))

(define a-branch (make-branch 4 3))
(define b-branch (make-branch 3 4))
(define a-mobile (make-mobile a-branch b-branch))

(define c-branch (make-branch 1 a-mobile))
(define d-branch (make-branch 5 1))
(define b-mobile (make-mobile c-branch d-branch))

(left-branch a-mobile)
(right-branch a-mobile)
(branch-length a-branch)
(branch-structure b-branch)

(total-weight a-mobile)
(total-weight b-mobile)

(total-length a-mobile)
(total-length b-mobile)

(mobile-balanced? a-mobile)
(mobile-balanced? b-mobile)