;;;   -*- Syntax: Common-Lisp; Package: Common-Lisp-User; Base: 10; Mode: LISP -*-

(in-package "COMMON-LISP-USER")

;;; ****************************************
;;; ** viewing DEFINITIONS AND PROCEDURES **
;;; ****************************************


(defvar *viewport* nil)


;;; ***************************** vueS ********************************
;;; Class: vue
;; a vue consists of a reference to a scene, as well as information
;; needed in rendering that scene, including camera and lighting.
(defclass vue ()
  ((scene :initform nil :initarg :scene :accessor vue-scene)
   (camera :initform nil :initarg :camera :accessor vue-camera)
   (lighting :initform nil :initarg :lighting :accessor vue-lighting))
  (:documentation "Contains information (camera, lighting) for viewing ~
                   a scene")) 

;; the veiw specified by the given scene, camera, and lighting information
(defun make-vue (scene camera &key (lighting nil))
  (let ((self (make-instance 'vue :scene scene :camera camera
			     :lighting lighting )))
    self))


;;; **************************** CAMERAS *******************************
;;; Class: CAMERA
;; A camera contains information on how a scene is to be transformed for
;; viewing.  This includes camera location, camera target, lens size,
;; and orientation
(defclass camera ()
  ((new-camera :initform t :accessor new-camera-of)
   (position :initarg :position :accessor camera-position)
   (target :initarg :target :accessor camera-target)
   (lens-width :initarg :lens-width :accessor camera-lens-width)
   (lens-height :initarg :lens-height :accessor camera-lens-height)
   (orientation :initform (make-3-vector 0.0 1.0 0.0)
		:initarg :orientation :accessor camera-orientation)
   ;; the viewing transform for the camera - not for user access
   (viewing-transform :initarg :viewing-transform
		      :accessor camera-viewing-transform))
  (:documentation "A function which transforms a scene from three ~
                   dimensions to two"))

;; sets the scene of vue to scene
(defun set-scene (scene &key (vue *vue*))
  (setf (vue-scene vue) scene))

;; sets the camera of vue to camera
(defun set-camera (camera &key (vue *vue*))
  (setf (new-camera-of camera) t)
  (setf (vue-camera vue) camera))


(defun 4-vector (list)
  (make-4-vector (first list) (second list) (third list) 1.0))

(defun update-camera (&key (camera *camera*) position orientation target 
			   lens-width lens-height)
  (if (and position (null orientation)
	   (every #'zerop (cross-prod
			   position
			   (list (aref (camera-orientation camera) 0)
				 (aref (camera-orientation camera) 1)
				 (aref (camera-orientation camera) 2)
				 ))))
      (setf orientation '(1.0 0.0 0.0)))
  (if position (setf (camera-position camera)(4-vector position)))
  (if orientation (setf (camera-orientation camera)(4-vector orientation)))
  (if target (setf (camera-target camera) (4-vector target)))
  (if lens-width (setf (camera-lens-width camera) lens-width))
  (if lens-height (setf (camera-lens-height camera) lens-height))
  (setf (new-camera-of camera) t)
  
  (with-slots (viewing-transform position target lens-width 
				 lens-height orientation) 
	      camera
	      (setf (camera-viewing-transform camera)
		    (viewing-transform position target lens-width
				       lens-height orientation)))
  camera)

;; a camera with the specified position, target, lens width, lens
;; height, and orientation.  Target and Position are assumed to be 
;; position vectors, lens width and height are assumed to be reals,
;; and orientation is assumed to be a unit vector, defaulting to an
;; upward direction if not given.  It must have 4 elements.
(defun make-camera-2 (&key (position '(10.0 8.0 5.0)) (target '(0.0 0.0 0.0))
			   (lens-width 10.0) (lens-height 10.0)
			   (orientation 
			    '(0.0 -1.0 0.0 1.0)))
  (make-camera (first position)
	       (second position)
	       (third position)
	       (first target)
	       (second target)
	       (third target)
	       lens-width
	       lens-height
	       (apply #'(lambda(x y z w) (make-4-vector x y z w))
		      orientation)))

(defun make-camera (px py pz tx ty tz lens-width lens-height 
		       &optional
		       (orientation 
			(make-4-vector 0.0 -1.0 0.0 1.0)))
  ;; find the viewing transform
  (let* (;; position and target vectors
	 (position (make-4-vector px py pz 1.0))
	 (target (make-4-vector tx ty tz 1.0))
	 ;; viewing transformation vector
	 (vue-transform (viewing-transform position target lens-width
					    lens-height orientation)))
    ;; make the camera
    (make-instance 'camera :position position :target target
		   :lens-width lens-width :lens-height lens-height
		   :orientation orientation
		   :viewing-transform vue-transform)))



;; changes the position of the given camera to (x y z)
(defun set-camera-position (x y z &key (camera *camera*))
  (setf (new-camera-of camera) t)
  (setf (camera-position camera)
	(make-4-vector x y z 1.0))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
      camera
    (setf (camera-viewing-transform camera)
	  (viewing-transform position target lens-width
			     lens-height orientation))))

;; changes the target of the given camera to (x,y,z)
(defun set-camera-target (x y z &key (camera *camera*))
  (setf (new-camera-of camera) t)
  (setf (camera-target camera)
	(make-4-vector x y z 1.0))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
      camera
    (setf (camera-viewing-transform camera)
	  (viewing-transform position target lens-width
			     lens-height orientation))))

;; changes the lens of the given camera to new-width and new-height
(defun set-camera-lens (new-width new-height &key (camera *camera*))
  (setf (new-camera-of camera) t)
  (setf (camera-lens-width camera) new-width)
  (setf (camera-lens-height camera) new-height)
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
              camera
     (setf (camera-viewing-transform camera)
	   (viewing-transform position target lens-width
			      lens-height orientation))))

;; changes the orientation of the given camera to (x, y, z), which
;; should be a unit vector
(defun set-camera-orientation (x y z &key (camera *camera*))
  (setf (new-camera-of camera) t)
  (setf (camera-orientation camera)
	(make-4-vector x y z 1.0))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
      camera
    (setf (camera-viewing-transform camera)
	  (viewing-transform position target lens-width
			     lens-height orientation))))


;;; Function:CHANGE-vue                 Created: Fri Sep 25 15:00:22 1992

(defun change-vue (x y z) 
  (set-camera-position x y z))


;; changes the lens size, by multiplying it by zoom-amount.
;; If zoom-amount<1, the camera appears to move inwards
;; If zoom-amount>1, the camera appears to move outwards
;; A value of zoom-amount<=0 causes an error.  Note that camera position
;; and target remain unchanged.
(defun zoom-camera (zoom-amount &key (camera *camera*))
  ;; find new camera lens
  (setf (new-camera-of camera) t)
  (if (<= zoom-amount 0)
      (error "camera lens zoom-amount must be a positive number")
      (progn (setf (camera-lens-width camera)
		   (* (camera-lens-width camera) zoom-amount))
	     (setf (camera-lens-height camera)
		   (* (camera-lens-height camera) zoom-amount))))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
              camera
     (setf (camera-viewing-transform camera)
	   (viewing-transform position target lens-width
			      lens-height orientation))))

;; pans the camera - that is, rotates its vuepoint around its
;; position.  Rotation occurs perpendicular to the orientation vector
;; The camera's target, not its position, is affected.
;; vuepoint is rotated clockwise, which means the objects appear to
;; rotate counterclockwise around the camera position (left on the screen)
(defun pan-camera (theta &key (camera *camera*))
  ;; calculate the new target position
  ;; a matrix which will rotate the target vector
  (setf (new-camera-of camera) t)
  (let* ((pos (camera-position camera))
	 (orient (camera-orientation camera))
	 (targ-rot (mult-matrix 
		    (translation-matrix (vector-elem pos 0)
					(vector-elem pos 1)
					(vector-elem pos 2))
		    (mult-matrix
		     (rotation-matrix (- theta) 3 orient)
		     (translation-matrix (- (vector-elem pos 0))
					 (- (vector-elem pos 1))
					 (- (vector-elem pos 2)))))))
    (setf (camera-target camera)
	  (mult-vector-matrix (camera-target camera) targ-rot)))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
              camera
	      (setf (camera-viewing-transform camera)
		    (viewing-transform position target lens-width
				       lens-height orientation))))

;; orbits the camera around the point it is looking at.  Rotation
;; occurs perpendicular to the orientation vector.  The camera's
;; position, not its target, is affected.
(defun orbit-camera (theta &key (camera *camera*))
  ;; calculate the new position
  ;; a matrix which will rotate the position vector
  (setf (new-camera-of camera) t)
  (let* ((targ (camera-target camera))
	 (orient (camera-orientation camera))
	 (pos-rot (mult-matrix 
		   (translation-matrix (- (vector-elem targ 0))
				       (- (vector-elem targ 1))
				       (- (vector-elem targ 2)))
		   (mult-matrix
		    (rotation-matrix (- theta) 3 orient)
		    (translation-matrix (vector-elem targ 0)
					(vector-elem targ 1)
					(vector-elem targ 2))))))
    (setf (camera-position camera)
	  (mult-vector-matrix (camera-position camera) pos-rot)))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
              camera
	      (setf (camera-viewing-transform camera)
		    (viewing-transform position target lens-width
				       lens-height orientation))))

;; changes the orientation of the camera.  orientation is rotated by
;; theta counterclockwise about line of sight, so the scene appears
;; to rotate clockwise.
(defun roll-camera (theta &key (camera *camera*))
  ;; calculate the new orientation vector
  (setf (new-camera-of camera) t)
  (let* ((pos (camera-position camera))
	 (targ (camera-target camera))
	 (line-of-sight (make-4-vector (- (vector-elem targ 0)
					  (vector-elem pos 0))
				       (- (vector-elem targ 1)
					  (vector-elem pos 1))
				       (- (vector-elem targ 2)
					  (vector-elem pos 2))
				       1.0)))
    (setf (camera-orientation camera)
	  (mult-vector-matrix (camera-orientation camera)
			      (rotation-matrix (- theta) 3 line-of-sight))))
  ;; recalculate viewing transformations
  (with-slots (position target lens-width lens-height orientation)
              camera
     (setf (camera-viewing-transform camera)
	   (viewing-transform position target lens-width
			      lens-height orientation))))

;;; *************************** LIGHTS ********************************
;;; Class: LIGHT
;; a single light source, at essentially infinite distance, with a 
;; specified direction and color
(defclass light ()
  ((direction :initarg :direction :accessor light-direction)
   (color :initarg :color :accessor light-color))
  (:documentation "A single light source at infinity"))

;; a light source at infinity in the specified direction with the 
;; specified color.  Direction should be a vector.
(defun make-light (direction color)
  (make-instance 'light :direction direction :color color))

;; inserts the given light source into the given vue
(defun insert-light (light &key (vue *vue*))
  (setf (vue-lighting vue)
	(cons light (vue-lighting vue))))

;; removes the given light source from the given vue
(defun remove-light (light &key (vue *vue*))
  (setf (vue-lighting vue)
	(remove light (vue-lighting vue))))

;; sets the color of light to color
(defun set-light-color (color light)
  (setf (light-color light) color))

;; sets the direction of light to direction
(defun set-light-direction (direction light)
  (setf (light-direction light) direction))


;;; *************************** viewportS *****************************
;;; Class: viewport
;; a section of an actual window on thes creen, in which a vue is to
;; be projected
(defclass viewport ()
  ((display :initarg :display :accessor viewport-display)
   (rect :initarg :rect :accessor viewport-rect)
   (vue :initarg :vue :accessor viewport-vue)
   ;; a hash table used for drawing vertices
   (hash-table :initform NIL :accessor viewport-ht))
  (:documentation "A section of a window, on which a vue is displayed"))

;; a viewport which displays vue, in window.  Optional parameter
;; rect, if not given, is set to the window bounds
;; rect is a list of (left-x top-y width height)
(defun make-viewport (vue &key (display *display*) (rect nil))
  (unless display (error "no display"))
  (let* ((window-rect (or rect
			  (list 0 0 (display-width display)
				(display-height display))))
	 (vp (make-instance 'viewport :display display :rect window-rect
			    :vue vue)))
    (setf (viewport-ht vp) (make-hash-table))
    vp))

;; changes the rect for viewport to the rect specified by x, y, width,
;; and height
(defun set-viewport-rect (x y width height &key (viewport *viewport*))
  (setf (viewport-rect viewport) (list x y width height)))



;;; Method:DRAW                          Created: Thu Sep 24 14:12:24 1992

#+xlib
(defmethod draw :before ((obj t) &key (viewport *viewport*))
	   (with-slots (new-camera) (vue-camera (viewport-vue viewport))
	     (when new-camera
	       (clrhash (viewport-ht viewport))
	       (setf new-camera nil))))

(defmethod draw ((obj list) &key (color nil) (update t) (clear nil)
		 (viewport *viewport*)(offset nil)(position nil))
  (if clear (clear-display))
  (loop for item in obj do
	(draw item :color color :update nil :clear nil
	      :viewport viewport :offset offset :position position))
  (if update (x-force-output)))

(defun conform (bbox &key (camera *camera*) (scale 0.9) (update t))
  (let* ((min (first bbox))
	 (max (second bbox))
	 (lens (*  (/ scale) 1/2
		   (loop for min in min
			 for max in max
			 maximize (- max min))))
	 (target (list (/ (+ (first min) (first max)) 2)
		       (if (second min)
			   (/ (+ (second min) (second  max)) 2)
			 0.0)
		       (if (third min)
			   (/ (+ (third min) (third  max)) 2)
			 0.0))))
    (if update
	(update-camera
	 :target target
	 :lens-width lens
	 :lens-height lens
	 :camera camera))
    (list lens target)))

(defun conform2 (bbox &key (camera *camera*) (scale 0.9))
  (let* ((min (first bbox))
	 (max (second bbox))
	 (lens (*  (/ scale) 1/2
		   (loop for min in min
			 for max in max
			 minimize (- max min)))))
    (update-camera
     :target
     (list (/ (+ (first min) (first max)) 2)
	   (if (second min)
	       (/ (+ (second min) (second  max)) 2)
	     0.0)
	   (if (third min)
	       (/ (+ (third min) (third  max)) 2)
	     0.0))
     :lens-width lens
     :lens-height lens
     :camera camera)))



;;; Function:LOOK-FROM                   Created: Sat Mar 27 14:46:12 1993

(defun look-from (where at
			&key (orientation '(1.0 0.0 0.0))
			(color *foreground*)
			(scale 0.9) (offset nil) (position nil))
  (update-camera :position where :orientation orientation)
  (conform (bbox (or position at)) :scale scale)
  (draw at :color color :clear t :offset offset :position position)
  )


(defun look (at-what &key
		     (position nil)
		     (orientation nil)
		     (color *foreground*)
		     (scale 0.7)
		     (offset nil)
		     (pos nil)
		     (draw t))
  (let ((bbox (bbox (or pos at-what)))
	)
    (ppq bbox)
    (if (and (null(rest(rest bbox)))
	     (null position))
	(setf position (list
			(/ (+ (first (first bbox)) (first (second bbox))) 2)
			(/ (+ (or (second (first bbox)) 0.0)
			      (or (second (second bbox)) 0.0)
			      ) 2)
			100.0)))
    (let ((x (conform bbox :scale scale :update nil)))
      (update-camera :position position :orientation orientation
		     :lens-width (first x)
		     :lens-height (first x)
		     :target (second x)))
    (if draw (draw at-what :color color :clear t :offset offset :position pos))))





   
