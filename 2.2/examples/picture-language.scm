;; painter constructor and selector

(define wave '())
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))


;; painter operation or compound painter

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom (transform-painter painter1
					   (make-vect 0.0 0.0)
					   (make-vect 1.0 0.0)
					   split-point))
	  (paint-top (transform-painter painter2
					split-point
					(make-vect 1.0 0.5)
					(make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right (transform-painter painter2
			      split-point
			      (make-vect 1.0 1.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 0.5 1.0)
		     (make-vect 1.0 0.5)))
(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))


;; higher-level abstraction from painter to picture

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (split bigger-located smaller-located)
  (lambda (painter n)
    (define (layer)
      (if (= layer 0)
	  painter
	  (let ((smaller (iter (- layer 1))))
	    (bigger located (smaller-located smaller smaller)))))
    (iter n)))

(define (flipped-pairs painter)
  (let ((painter2 (beside wave (flip-vert painter))))
    (below painter2 painter2)))
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter	(below smaller smaller)))))
(define right-split (split beside below))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;; vector constructor & selector

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

;; frame constructor & selector

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (list-ref frame 1))
(define (dege2-frame frame) (list-ref frame 2))

(define (edge1-point-frame frame)
  (add-vect (edge1-frame frame)
	    (origin-frame frame)))
(define (edge2-point-frame frame)
  (add-vect (edge2-frame frame)
	    (origin-frame frame)))
(define (diagonal-point-frame frame)
  (sub-vect (add-vect (edge1-point-frame frame) (origin-frame frame))
	    (edge2-point-frame frame)))

(define (diagonals-frame frame)
  (let ((diagonal1 (sub-vect (edge1-point-frame frame) (edge2-point-frame frame)))
	(diagonal2 (sub-vect (diagonal-point-frame frame) (origin-frame frame))))
    (list diagonal1 diagonal2)))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (cddr frame))

;; vector operations

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))


;; map vector into frame

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
	      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
			(scale-vect (ycor-vect v) (edge2-frame frame))))))


;; segment constructor & selector

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))


;; draw segments

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each (lambda (segment)
		(draw-line
		 ((frame-coord-map frame)
		  (start-segment segment))
		 ((frame-coord-map frame)
		  (end-segment segment))))
	      segment-list)))

(define (frame-outline-painter frame)
  (let ((diagonal-point (diagonal-point-frame frame))
	(origin (origin-frame frame)))
    (let ((edge1-point (edge1-point-frame frame))
	  (edge2-point (edge2-point-frame frame)))
      (let ((edge1 (make-segment origin edge1-point))
	    (edge2 (make-segment origin edge2-point))
	    (edge3 (make-segment edge1-point diagonal-point))
	    (edge4 (make-segment edge2-point diagonal-point)))
	((segments->painter (list edge1 edge2 edge3 edge4)) frame)))))

(define (frame-diagonal-painter frame)
  ((segments->painter (diagonals-frame frame) frame)))

(define (frame-diamond-painter frame)
  (let ((origin (origin-frame frame))
	(edge1-point (edge1-point-frame frame))
	(edge2-point (edge2-point-frame frame))
	(diagonal-point (diagonal-point-frame frame)))
    (let ((mid1 (midpoint origin edge1-point))
	  (mid2 (midpoint origin edge2-point))
	  (mid3 (midpoint edge1 diagonal-point))
	  (mid4 (midpoint edge2 diagonal-point)))
      (let ((segments (list (make-segment mid1 mid2)
			    (make-segment mid2 mid3)
			    (make-segment mid3 mid4)
			    (make-segment mid4 mid1))))
	((segments->painter segments) frame)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter (make-frame
		  new-origin
		  (sub-vect (m corner1) new-origin)
		  (sub-vect (m corner2) new-origin)))))))



