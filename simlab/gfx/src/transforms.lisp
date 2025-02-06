;;;   -*- Syntax: Common-Lisp; Package: common-lisp-user; Base: 10; Mode: LISP -*-

;;; ***************************************************
;;; **  2-D AND 3-D GRAPHICS TRANSFORMATIONS PACKAGE **
;;; ***************************************************

;;; written by Scott Benson, July 1989
;;; Modified to run on ALGEBRA-UTILS rather than WEYL, Feb 1990

(in-package "COMMON-LISP-USER")

;;;
;;; FUNCTIONS FOR GENERATION OF TRANSFORMATION MATRICES 
;;;

;;; standard n-dimensional transformation matrices.  Translation and
;;; scaling are implemented in arbitrary dimensions, while rotation is
;;; implemented in two and three dimensional cases only.

;; N dimensional matrix to translate a point by the list of offsets.  N is set
;; to the length of the offset list
(defun translation-matrix (&rest offsets)
  ;; make an identity matrix and insert the proper translation offsets
  (let* ((dim (length offsets))
	 (m (make-identity-matrix (+ dim 1))))
    (loop for arg in offsets
	  for n from 0 to (- dim 1)
	  do (set-matrix-elem m dim n arg)
	  finally (return m))))

;; N dimensional scaling matrix to scale by the list of scale factors.
;; N is set to the length of the factor list
(defun scale-matrix (&rest factors)
  ;; make an identity and insert the proper scaling offsets
  (let* ((dim (length factors))
	 (m (make-identity-matrix (+ dim 1))))
    (loop for arg in factors
	  for n from 0 to (- dim 1)
	  do (set-matrix-elem m n n arg)
          finally (return m))))

;; matrix for performing arbitrary two and three dimensional rotations.
;; Theta specifies clockwise angle of rotation, dim specifies
;; dimension.  rest should be the point x, y of rotation in two 
;; dimensions, or the (projective) vector of rotation in three
;; dimensions.  Dimensions other than 2 or 3 are currently not
;; supported.
(defun rotation-matrix (theta dim &rest args)
  (case dim 
    ;; if it's two dimensional, just multiply three matrices
    (2 (let ((x (first args))
	     (y (second args)))
	 (mult-matrix (translation-matrix (- x) (- y))
		      (mult-matrix (2d-rotation-matrix theta)
				   (translation-matrix x y)))))
    ;; if it's three dimensional, finding the matrix is harder
    (3 (let* (;; the axis of rotation
	      ;; normalize vect
	      (vx (first args))
	      (vy (second args))
	      (vz (third args))
	      (r (sqrt (+ (* vx vx) (* vy vy) (* vz vz))))
	      (vx (/ vx r))
	      (vy (/ vy r))
	      (vz (/ vz r))
	      (cost (cos theta))
	      (sint (sin theta))
	      ;; the normalized form of vect
	      (norm-vect (make-3-vector vx vy vz))
	      ;; i, j, and k unit vectors
	      (xaxis (make-3-vector 1.0 0.0 0.0))
	      (yaxis (make-3-vector 0.0 1.0 0.0))
	      (zaxis (make-3-vector 0.0 0.0 1.0))
	      ;; i, j, and k rotated around vect
	      (xrotate (rotated-vector cost sint xaxis norm-vect))
	      (yrotate (rotated-vector cost sint yaxis norm-vect))
	      (zrotate (rotated-vector cost sint zaxis norm-vect)))
	 ;; make a matrix out of the results.  xrotate, yrotate and
	 ;; zrotate are just the three rows of the matrix
	 (make-general-matrix (make-array '(4 4) :initial-contents
				  (list (list (vector-elem xrotate 0)
					      (vector-elem xrotate 1)
					      (vector-elem xrotate 2)
					      0)
					(list (vector-elem yrotate 0)
					      (vector-elem yrotate 1)
					      (vector-elem yrotate 2)
					      0)
					(list (vector-elem zrotate 0)
					      (vector-elem zrotate 1)
					      (vector-elem zrotate 2)
					      0)
					'(0 0 0 1))))))
    (otherwise (error "~A-dimensional rotation is not supported" dim))))


;;; utility functions used by rotation-matrix

;; returns the matrix for two-dimensional rotation about the origin
;; by an angle theta
(defun 2d-rotation-matrix (theta)
  (let ((sint (sin theta))
	(cost (cos theta)))
    (make-general-matrix (make-array '(3 3) :initial-contents
				     (list (list cost (- sint) 0)
					   (list sint cost 0)
					   '(0 0 1))))))

;; returns the cross product of vectors a and b.  a and b must
;; both be 3-vectors, of course.
(defun cross-product (a b)
  (let ((x1 (vector-elem a 0)) ;the first vector
	(y1 (vector-elem a 1))
	(z1 (vector-elem a 2))
	(x2 (vector-elem b 0)) ;the second vector
	(y2 (vector-elem b 1))
	(z2 (vector-elem b 2)))
    (make-3-vector (- (* y1 z2) (* z1 y2))
		   (- (* z1 x2) (* x1 z2))
		   (- (* x1 y2) (* y1 x2)))))

;; returns the dot product of vectors a and b.  a and b must be 3-vectors.
(defun dot-product (a b)
  (+ (* (vector-elem a 0) (vector-elem b 0))
     (* (vector-elem a 1) (vector-elem b 1))
     (* (vector-elem a 2) (vector-elem b 2))))

;; returns the vector vect rotated around the vector axis by
;; an angle given by cost and sint.
;; axis must be a unit vector.
;; both vectors must be 3-vectors.
(defun rotated-vector (cost sint vect axis)
  (let* (;; the component of vect parallel to axis
	 (parallel-comp (mult-vector-scalar axis
					    (dot-product vect axis)))
  	 ;; the component of vect normal to axis
	 (normal-comp (subtract-vector vect parallel-comp))
	 ;; the vector normal to both parallel-comp and normal-comp
	 (normal-nc (cross-product normal-comp axis))
	 ;; normal-comp rotated about axis by theta
	 (rotated-norm (add-vector (mult-vector-scalar normal-comp cost)
				   (mult-vector-scalar normal-nc sint))))
    (add-vector parallel-comp rotated-norm)))


;;; ********************* VIEWING TRANSFORM ********************************

;; the transform used in creating views.  Target and Position
;; represent camera target and position, and should both be vectors.
;; lens-height and lens-width are both numbers.  Orientation is a
;; 4-vector stating "which way is up."
;; This transform will transform object space points into image space such
;; that target=>(z=1) and lens-width=>(x=1), lens-height=>(y=1)

(defun viewing-transform (position target lens-width lens-height
				   orientation)
  (let* (;; matrix to move camera to origin

	 (t1 (translation-matrix (- (vector-elem position 0))
				 (- (vector-elem position 1))
				 (- (vector-elem position 2))))
	 ;; the inverted viewing vector
	 (viewing-vect (subtract-vector position target))
	 (vx (vector-elem viewing-vect 0))
	 (vy (vector-elem viewing-vect 1))
	 (vz (vector-elem viewing-vect 2))
	 ;; length of the viewing vector
	 (rxy (sqrt (+ (* vx vx) (* vy vy))))
	 (r (sqrt (+ (* rxy rxy) (* vz vz))))
	 ;; matrix to move view vector into xz plane
	 (t2 (if (= 0 rxy)
		 (make-identity-matrix 4)
		 (z-rotation-matrix (/ vy rxy)
				    (/ vx rxy))))
	 ;; matrix to move view vector to z axis
	 (t3 (y-rotation-matrix (sqrt (- 1 (/ (* vz vz) (* r r))))
				(/ vz r)))
	 ;; matrix to invert z axis
	 (t4 (scale-matrix 1.0 1.0 -1.0))
	 ;; the orientation vector after the rotation transforms
	 (rotated-orient (mult-vector-matrix orientation
					     (mult-matrix t2 t3)))
	 (ox (vector-elem rotated-orient 0))
	 (oy (vector-elem rotated-orient 1))
	 (lxy (sqrt (+ (* ox ox) (* oy oy))))
	 ;; vector to correct for orientation
	 (t5 (cond
	      ((= 0 lxy)
	       (error "orientation vector points in view direction"))
	      (t(z-rotation-matrix (/ ox lxy) (- (/ oy lxy))))))
	 ;; matrix to adjust for lens size
	 (t6 (scale-matrix (/ 1.0 lens-width)
			   (/ 1.0 lens-height)
			   (/ 1.0 r))))
    '(ppq rxy  vx vy vz  ox oy lxy r lens-height lens-width)
    '(mapc #'(lambda(name m)
	      (format t "~s ~%" name)
	      (print-matrix m))
	   '(t1 t2 t3 t4 t5 t6)
	   (list t1 t2 t3 t4 t5 t6))
    ;; the real viewing transform
    (mult-matrix t1 
       (mult-matrix t2
	  (mult-matrix t3 
	     (mult-matrix t4 
		(mult-matrix t5 t6)))))))


;;; utilities used by viewing-transform routine

;; matrix to rotate around the y axis by an angle t
(defun y-rotation-matrix (sint cost)
  (make-general-matrix (make-array '(4 4) :initial-contents
				   (list (list cost 0 sint 0)
					 '(0 1 0 0)
					 (list (- sint) 0 cost 0)
					 '(0 0 0 1)))))

;; matrix to rotate around the z axis by an angle t
(defun z-rotation-matrix (sint cost)
  (make-general-matrix (make-array '(4 4) :initial-contents
				   (list (list cost (- sint) 0 0)
					 (list sint cost 0 0)
					 '(0 0 1 0)
					 '(0 0 0 1)))))
