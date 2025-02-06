;;;   -*- Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Mode: LISP -*-

;;; ************************
;;; ** RENDERING ROUTINES **
;;; ************************

(in-package "COMMON-LISP-USER")
(defmacro xv (val)
  `(coerce (round ,val) 'fixnum))

;;; *************** GLOBAL DEFAULT viewing VARIABLES *********************
(defvar *display*)   ;default display (an x-window)
(defvar *viewport*)  ;default viewport
(defvar *vue*)      ;default viewing variables
(defvar *scene*)     ;default scene to be looked at
(defvar *camera*)    ;a default camera
(defvar *lighting*)  ;default lighting, set to nil

;;; *********************** GENERAL UTILITES *****************************

;; a global variable used to tell which viewports must be displayed.  
;; really just a list of viewports
(defvar *global-viewports*)

;; routine which sets up everything necessary for performing graphical
;; output on the specified host
;; also sets all default viewing variables to their default values, unless
;; otherwise specified by the user
(defun initialize-graphics
  (&key (host NIL)
	(camera-pos-x 0.0) (camera-pos-y 0.0) (camera-pos-z 10.0)
	(camera-targ-x 0.0) (camera-targ-y 0.0) (camera-targ-z 0.0)
	(camera-lens-width 12.0) (camera-lens-height 12.0)
	(background *background*)
	(foreground *foreground*)
	(name  "DISPLAY")
	(width 500)
	(height 500)
	(window nil)
	(gcontext nil)
	(display-num 0))
  #+xlib(progn
          (when (not host)
	    (setf host (machine-instance)))
	  (initialize-x host :display-num display-num))
					;make clx ready for use
  #+apple(setf *display-width* width)
  #+apple(setf *display-height* height)
  
  (setf *global-viewports* nil) ;no viewports to begin with
  (setf *display* (create-display :width width :height height
				  :background background 
				  :foreground foreground 
				  :display-name name
				  :window window
				  :gcontext gcontext))
  (setf *lighting* nil) ;no lighting
  (setf *camera* (make-camera camera-pos-x camera-pos-y camera-pos-z
			      camera-targ-x camera-targ-y camera-targ-z
			      camera-lens-width camera-lens-height))
  (setf *scene* nil
					;(make-scene)
        ) ;a scene in space NIL
  (setf *vue* (make-vue *scene* *camera* :lighting *lighting*))
  (setf *viewport* (make-viewport *vue* :display *display*))
  (setf *point-style* :dot)
  (setf *point-diameter* 5)
  (setf *point-radius* 2)
  (setf *line-width* 0))

#+apple(defun display-height (display)
         *display-height*)
#+apple(defun display-width (display)
         *display-width*)

;; resets *point-style* to the given style, which must be one of :dot, 
;; :plus, or :cross.
(defun set-point-style (new-style)
  (if (member new-style '(:point :dot :plus :cross))
      (setf *point-style* new-style)
      (error "~A is not a standard point style" new-style)))

;; sets the point diameter of displayed points to new-diameter
(defun set-point-diameter (new-diameter)
  (setf *point-radius* (floor (/ new-diameter 2.0)))
  (setf *point-diameter* new-diameter))

;; sets the width of displayed lines to new-width
(defun set-line-width (new-width)
  (setf *line-width* new-width))

;; puts viewport into the list of displayable-viewports
(defun insert-viewport (viewport &optional (remove-previous nil))
  (setf *global-viewports*
	(if remove-previous (list viewport)
	    (cons viewport *global-viewports*))))

;; removes viewport from the list of displayable-viewports
(defun remove-viewport (viewport)
  (setf *global-viewports*
	(remove viewport *global-viewports*)))
	      

;;; Function:DRAW-VERTEX                 Created: Fri Jul 31 16:41:30 1992

(defun draw-vertex (vert &key (viewport *viewport*) (transform nil)
			 (radius *draw-point-radius*)
			 (color *black*) (update t) (size 1)
			 (fill t))
  (if color
      (set-drawing-color color (viewport-display viewport)))
  (let((screen-point (screen-point (coords-of vert) viewport transform)))
    (draw-point (first screen-point) (second screen-point)
		(viewport-display viewport) color fill
		(truncate (* radius size))
		))
  (if update (x-force-output)))

(defun draw-elipse (vert width height &key
			 (viewport *viewport*) (transform nil)
			 (color *black*) (update t) (size 1)
			 (fill t))
  (let((screen-point (screen-point (coords-of vert) viewport transform))
       (lens (minus (screen-point (list width height) viewport transform)
		    (screen-point (list 0.0 0.0) viewport transform)
		    )))
    (pp screen-point lens)
    (draw-elipse-ll
     (first screen-point) (second screen-point)
     (abs (first lens)) (abs (second lens))
     :display (viewport-display viewport)
     :color color
     :fill fill
     )
    (if update (x-force-output))))



(defun tnorm (list)
  (sqrt(loop for item in list sum (* item item))))

(defun draw-edge (edge &key
		       (viewport *viewport*) (transform nil)
		       (color nil) (oriented nil) (curved nil)
		       (width *line-width*)
		       (ratio 0.1)
		       (scale 1.0))
  (if color
      (set-drawing-color color (viewport-display viewport)))
  (let*((endpoints  (if (listp edge) edge (weyli::vertices-of edge)))
	(p1 (coords-of (first endpoints)))
	(p2 (coords-of (second endpoints)))
	(bc nil)
	(*line-width* width))
    (declare (special *line-width*))
    (unless (eql scale 1.0)
	    (setf bc (barycenter (list p1 p2)))
	    (if curved (setf bc (mapcar #'+ bc
				   (loop for obj in
					 (normal-to (mapcar #'- p1 p2))
					 collect (times ratio obj)
					 ))))
	    (setf p1 (plus(times scale p1) (times (- 1.0 scale) bc)))
	    (setf p2 (plus (times scale p2) (times (- 1.0 scale) bc))))
    (setf p1 (screen-point p1 viewport transform))
    (setf p2 (screen-point p2 viewport transform))
    (cond
     (curved
      (draw-curved-line
       (first p1) (second p1)
       (first p2) (second p2)
       (viewport-display viewport)
       :color color :oriented oriented :curve-scale ratio))
     (oriented
      (draw-arrow-line
       (first p1) (second p1)
       (first p2) (second p2)
       (viewport-display viewport)
       :color color))
     (t(draw-line
	(first p1) (second p1)
	(first p2) (second p2)
	(viewport-display viewport)
	color)))
    (x-force-output)))

(defmethod cross-prod ((v1 list) (v2 list))
  (list
   (-
    (* (second v1) (third v2))
    (* (second v2) (third v1)))
   (-
    (* (first v2) (third v1))
    (* (first v1) (third v2)))
   (-
    (* (first v1) (second v2))
    (* (first v2) (second v1)))))

(defun normal-to (v)
  (cond ((= 2 (length v))
	 (list (- (second v)) (first v)))
	((= 3 (length v))
	 (if (zerop (first v))
	     (cross-prod v '(1 0 0))
	   (cross-prod v '(0 1 0))))
	(t (error "no normal"))))

(defun draw-arrow-arc (center radius &key
			      (viewport *viewport*)
			      (transform nil)
			      (start 0.0)
			      (end (* 2 pi))
			      (ratio 0.2)
			      (color *black*) 
			      (width *line-width*)
			      )
  (let((*line-width* width)
       (p2 (list (+ (first center) radius)
		 (second center))))
    (declare (special *line-width*))
    (setf center (screen-point center viewport transform))
    (setf radius (screen-point p2  viewport transform))
    (draw-arrow-circle
     (first center)
     (second center)
     (abs (- (first radius) (first center)))
     :display 	 (viewport-display viewport)
     :color color
     :start start
     :end end
     :ratio ratio)))



(defun draw-vector (vector where &key (viewport *viewport*) (transform nil)
			   (color nil) (ratio 1/8))
  (let*((endpoints (list where (mapcar #'+ where vector)))
	(p1(screen-point (first endpoints) viewport transform))
	(p2(screen-point (second endpoints) viewport transform)))
    (draw-arrow-line
     (first p1) (second p1)
     (first p2) (second p2)
     (viewport-display viewport) :ratio ratio
     :color color)
    (x-force-output)))

(defun draw-face (face-verts &key (viewport *viewport*) (transform nil)
			     (color nil) (shape :complex))
  (unless color 
    (setf color (display-foreground (viewport-display viewport))))
  (let ((verts (loop for vert in face-verts
		     collect(screen-point vert viewport transform))))
    (draw-poly (append verts (list (first verts)))
	       :display (viewport-display viewport)
	       :color color :shape shape :fill t)))

;; takes a vert in object-space, a modeling transform, and a 
;; viewport, and returns a list (x y) of the window coordinates in
;; viewport of the vert transformed by transform if transform is
;; non-NIL

(defun screen-point (coords viewport transform)
  (let* (
	 ;; a 4-vector representing the vertex
	 ;; dimension of the vertex must be taken into account.
	 (vert-vect (case (length coords)
		      (1 (make-4-vector (coord coords 0)
					0.0 0.0 1.0))
		      (2 (make-4-vector (coord coords 0)
					(coord coords 1)
					0.0 1.0))
		      (t (make-4-vector (coord coords 0)
					(coord coords 1)
					(coord coords 2)
					1.0))))
	 ;; the vertex transformed into image space
	 (vt1 (if transform
		  (mult-vector-matrix vert-vect transform)
		  vert-vect))
	 (image-vert (mult-vector-matrix vt1 (camera-viewing-transform
					      (vue-camera
					       (viewport-vue viewport)))))
	 ;; screen coordinates after the perspective transformation,
	 ;; with values ranging from -1 to 1
	 
	 (screen-x (/ (vector-elem image-vert 0)
		      (vector-elem image-vert 2)))
	 (screen-y (/ (vector-elem image-vert 1)
		      (vector-elem image-vert 2)))
	 ;; the viewing rectangle of the viewport - a list of
	 ;; (viewx viewy width height)
	 (v-rect (viewport-rect viewport))
	 ;; actual screen coordinates
	 ;; x = vuex + width * (screenx+1)/2
	 (sx (+ (first v-rect)
		(* 0.5 (third v-rect) (1+ screen-x))))
	 ;; y = vuey + height * (1-screeny)/2
	 (sy (+ (second v-rect)
		(* 0.5 (third v-rect) (- 1 screen-y)))))
    (setf sx (xv sx))
    (setf sy (xv sy))
    (list sx sy)))

;;; *********************** drawing methods ******************************

;; causes an object in a viewport to flash on and off flashes times, at
;; intervals of interval seconds
(defun flash (obj &key (viewport *viewport*) (color NIL)
		  (flashes 3) (intervals 0.1))
  (let* ((display (viewport-display viewport))
        (backcol (display-background display)))
    (unless color 
      (setf color (display-foreground display)))
      (dotimes (a flashes)
        (draw obj :color backcol)
        (sleep intervals)
        (draw  obj :color color)
        (sleep intervals))))
		       
;; Method: DRAW
;; causes an object to be drawn in a viewport without actually putting it
;; in the scene
;; DRAW for a standard object
'(defmethod draw ((obj object) &key (color NIL) (viewport *viewport*)
		  (offset nil))
  (unless color
    (setf color (display-foreground (viewport-display viewport))))
  (one-color-draw-object obj color viewport)
  (x-force-output))

;; DRAW for a list
'(defmethod draw ((obj-list list) &key (color NIL) (viewport *viewport*)
		  (offset nil))
  (loop for obj in obj-list
        do (draw obj :color color :viewport viewport
		 :offset offset)))


(defun draw-float (float pos &key (color 1) (update t) (clear nil)
			 (viewport *viewport*) (font *default-font*))
  (let* ((string (format nil "~f" float))
	 (string (if (< 8 (length string))
		     (format nil "~5f" float)
		     string)))
    (draw-string string pos :color color :update update
		 :clear clear :viewport viewport
		 :font font)))

(defun draw-obj (obj pos &key (color 1) (update t) (clear nil)
		     (viewport *viewport*) (font *default-font*))
  (draw-string (format nil "~a" obj) pos :color color :update update
	       :viewport viewport :font font))

;;; Function:DRAW-COORDINATE-FRAME       Created: Sat Mar 27 14:19:57 1993
(defmacro  DRAW-COORDINATE-FRAME (&rest rest)
  `(DRAW-cf ,@rest))

(defun draw-cf (&key (where '(0.0 0.0 0.0))
		     (viewport *viewport*)
		     (color *black*)
		     (clear nil)
		     (ratio 1/8)
		     (len 1.0)
		     (basis '((1.0 0.0 0.0)(0.0 1.0 0.0)(0.0 0.0 1.0))))
  (if clear (clear-display))
  (loop for vector in basis
	for vector2 = (loop for v in vector collect (* len v))
	for char in '("x" "y" "z") do
	(draw-vector vector2
		     where :viewport viewport
		     :color color :ratio ratio)
	(draw-string  char (mapcar #'+ vector2 where)
		      :viewport viewport
		      :color color)))




(defun draw-arrow-line (x1 y1 x2 y2 display &key (ratio 1/8)
			   (color nil)
			   (operation :draw)
			   (max-head-size 15)
			   (min-head-size 10))
  (let ((x1 (xv x1))
	(y1 (xv y1))
	(x2 (xv x2))
	(y2 (xv y2))
	(dx (* ratio (- x2 x1)))
	(dy (* ratio (- y2 y1)))
	(len nil)
	(nx nil)
	(ny nil))
    (draw-line x1 y1 x2  y2 display color)
    (cond ((or  max-head-size min-head-size)
	   (if (zerop (setf len (sqrt (+ (* dx dx) (* dy  dy)))))
	       (return-from draw-arrow-line nil))
	   (if (> len max-head-size)
	       (setf dx (* max-head-size dx  (/ len))
		     dy (* max-head-size dy  (/ len)))
	     (if (< len min-head-size)
		 (setf dx (* min-head-size dx  (/ len))
		       dy (* min-head-size dy  (/ len)))))))
    (setf nx (* 1/4 (- dy))  ny (* 1/4 dx))
    
    (draw-poly
     (list (list x2 y2)
	   (list (xv(- x2  dx nx))
		 (xv(- y2  dy ny)))
	   (list (xv(- x2  dx (- nx)))
		 (xv(- y2  dy (- ny))))
	   (list x2 y2))
     :color color
     :display display)))

