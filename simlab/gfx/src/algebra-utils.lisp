;;;   -*- Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Mode: LISP -*-

(in-package "COMMON-LISP-USER")

;;; ******************************************************************
;;; ALGEBRAIC UTILITIES USED BY GFX, NON-WEYL VERSION
;;; ******************************************************************

;;; Rewrite of the algebraic utilities for GFX, not using the WEYL
;;; package.  This file contains macros for all of the matrix and vector
;;; functions used by the clu package.  Should be equivalent to the
;;; functions in WEYL.

;;; This file contains macros for all of the matrix and vector functions
;;; used by the gfx package.  By default, these macros map to functions from
;;; the WEYL package.  This file can be replaced by another file containing
;;; equivalent macros, which avoids having to use the WEYL package.
;;; The macros necessary are:
;;;    MAKE-IDENTITY-MATRIX
;;;    MAKE-GENERAL-MATRIX
;;;    MATRIX-ELEM
;;;    SET-MATRIX-ELEM
;;;    MAKE-VECTOR <-function
;;;    MAKE-3-VECTOR
;;;    MAKE-4-VECTOR
;;;    VECTOR-ELEM
;;;    SET-VECTOR-ELEM
;;;    MULT-MATRIX
;;;    ADD-VECTOR
;;;    SUBTRACT-VECTOR
;;;    MULT-VECTOR-SCALAR
;;;    MULT-VECTOR-MATRIX
;;; The global variables are not needed unless WEYL is being used.

;;; creates an N-dimensional identity matrix.  A matrix is simply an
;;; array of the appropriate size.
(defmacro make-identity-matrix (dimension)
  `(make-one-matrix ,dimension))

;;; function used by make-identity-matrix
(defun make-one-matrix (dimension)
  (let ((m (make-array (list dimension dimension) :initial-element 0)))
    (dotimes (x dimension)
      (setf (aref m x x) 1))
    m)) ;return the matrix value


  
;;; returns element (i,j) from the given matrix
(defmacro matrix-elem (matrix i j)
  `(aref ,matrix ,i ,j))

;;; sets element (i,j) of the matrix to x
(defmacro set-matrix-elem (matrix i j x)
  `(setf (aref ,matrix ,i ,j) ,x))

;;; makes a vector of the appropriate dimension from a list of elements
(defun make-vector (element-list)
  (make-array (length element-list) :initial-contents element-list))

;;; creates a 3-vector from the given elements.
(defmacro make-3-vector (v1 v2 v3)
  `(make-array 3 :initial-contents (list ,v1 ,v2 ,v3)))

;;; creates a 4-vector from the given elements.
(defmacro make-4-vector (v1 v2 v3 v4)
  `(make-array 4 :initial-contents (list ,v1 ,v2 ,v3 ,v4)))

;;; returns element i from the given vector
(defmacro vector-elem (vector i)
  `(aref ,vector ,i))

;;; sets element i of the given vector to x
(defmacro set-vector-elem (vector i x)
  `(setf (aref ,vector ,i) ,x))

;;; creates a general matrix from the given array of elements.
;;; trivial in this implementation
(defmacro make-general-matrix (elements)
  elements)

;;; multiply the two given matrices together
(defmacro mult-matrix (m1 m2)
  `(matrix-multiply ,m1 ,m2))

;;; add the given vectors
(defmacro add-vector (v1 v2)
  `(vector+ ,v1 ,v2))
;;; function used by add-vector
(defun vector+ (v1 v2)
  (let* ((array-size (array-dimension v1 0))
	 (new-vect (make-array array-size)))
    (if (= (array-dimension v2 0) array-size)
	(dotimes (i array-size new-vect)
	  (setf (aref new-vect i)
		(+ (aref v1 i) (aref v2 i))))
	(error "trying to add different sized vectors"))))

;;; subtract v2 from v1
(defmacro subtract-vector (v1 v2)
  `(vector- ,v1 ,v2))
;;; function used by subtract-vector
(defun vector- (v1 v2)
  (let* ((array-size (array-dimension v1 0))
	 (new-vect (make-array array-size)))
    (if (= (array-dimension v2 0) array-size)
	(dotimes (i array-size new-vect)
	  (setf (aref new-vect i)
		(- (aref v1 i) (aref v2 i))))
	(error "trying to subtract different sized vectors"))))

;;; multiply a vector by a scalar
(defmacro mult-vector-scalar (vector scalar)
  `(vs-mult ,vector ,scalar))
;;; function used by mult-vector-scalar
(defun vs-mult (vector scalar)
  (let* ((array-size (array-dimension vector 0))
	 (new-vect (make-array array-size)))
    (dotimes (i array-size new-vect)
      (setf (aref new-vect i)
	    (* (aref vector i) scalar)))))

;;; multiply a vector by a matrix
;;; simply calls matrix-multiply!
(defun mult-vector-matrix (vector matrix)
  (mv-multiply vector matrix))

;;; function to multiply a vector by a matrix
;;; Does not do any error checking.
(defun mv-multiply (v m)
  (let* ((vector-size (array-dimension v 0))
	 (new-vector (make-array vector-size :initial-element 0)))
    (dotimes (col (array-dimension m 1) new-vector)
      (dotimes (temp vector-size)
	(setf (aref new-vector col)
	      (+ (aref new-vector col)
		 (* (aref v temp)
		    (aref m temp col))))))))

;;; function to multiply two matrices together.
(defun matrix-multiply (m1 m2)
  (let* ((matrix-size-1a (array-dimension m1 0))
	 (matrix-size-1b (array-dimension m1 1))
	 (matrix-size-2a (array-dimension m2 0))
	 (matrix-size-2b (array-dimension m2 1))
 	 (new-matrix (make-array (list matrix-size-1a matrix-size-2b)
				 :initial-element 0)))
    (if (= matrix-size-1b matrix-size-2a)
	(dotimes (col matrix-size-2b new-matrix)
	  (dotimes (row matrix-size-1a)
	    (dotimes (temp matrix-size-1b)
	      (setf (aref new-matrix row col)
		    (+ (aref new-matrix row col)
		       (* (aref m1 row temp)
			  (aref m2 temp col)))))))
	(error "trying to multiply matrices of incompatible sizes"))))

;;; diagnostic function - prints out matrix m
(defun print-matrix (m)
  (let ((rows (array-dimension m 0))
	(cols (array-dimension m 1)))
    (dotimes (row rows)
      (dotimes (col cols)
	(format t "~a " (aref m row col)))
      (format t "~%"))))




(defmethod print-object :around ((v vector) stream)
  (cond
    ((and (plusp (length v))
	  (< (length v) 500)
	  (numberp (aref v 0)))
     (format-list stream  
		  (loop for i below (length v) 
			collect (aref v i))
		  :start "<" :end ">"))
    (t(call-next-method))))

#|
(defmethod print-object ((a array) stream)
  (print-array a stream))
|#

;;; Function:UNIT-VECTOR                 Created: Sun Apr 25 13:56:27 1993

(defun unit-vector (list) 
  (loop for item in list
	with divisor = (loop for item in list sum (* item item) into sum
			     finally (if (zerop sum)
					 (error "unit-vector of zero vector")
					 (return (sqrt sum))))
	collect (/ item divisor)))





;;; Function:RADIANS-TO-DEGREES          Created: Mon Apr  4 10:22:28 1994

(defun radians-to-degrees (radians) 
  (* (/ radians pi) 180))

