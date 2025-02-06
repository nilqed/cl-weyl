;;;  Rick's version modified
;;;   -*- Syntax: Common-Lisp; Package: User; Base: 10; Mode: LISP -*-

(in-package "COMMON-LISP-USER")
(require :quickdraw)
(shadow '(draw))

 

(defvar *display* nil)
(defvar *display-width* nil)
(defvar *display-height* nil)
(defvar *default-font*)

(defvar *directed-edges* t)
 
(defvar *draw-2d-solid-p* nil "Whether or not to draw 2d objects as
         solids or skeletons")  
(defvar *draw-point-radius* 2 "The radius of a point on the screen")


;; sets up the global variables and colors.  If the system is
;; monochrome, *white* is set to white, and all other colors are
;; set to black
(defun set-mac-colors () 
  ;; set up white and black
  (setf *white* *white-color*)
  (setf *black* *black-color*)
  ;; set other colors appropriately
  (if *color-available* 
    ;; set colors to predefined colors on Mac
    (progn (setf *gray* *gray-color*)
           (setf *red* *red-color*)
           (setf *green* *green-color*)
           (setf *blue* *blue-color*)
           (setf *yellow* *yellow-color*)
           (setf *orange* *orange-color*)
           (setf *magenta* *pink-color* )
           (setf *cyan* *purple-color*)
           (setf *brown* *brown-color*)
           (setf *cornflower-blue*  *light-blue-color* )
	   (setf *forest-green* *dark-green-color*)
	   (setf *firebrick* *tan-color*))
    ;; else set all colors to black
    (progn (setf *gray* *black-color*)
           (setf *red* *black-color*)
           (setf *green* *black-color*)
           (setf *blue* *black-color*)
           (setf *yellow* *black-color*)
           (setf *orange* *black-color*)
           (setf *magenta* *black-color*)
           (setf *cyan* *black-color*)
           (setf *brown* *black-color*)
           (setf *cornflower-blue* *black-color*)
           (setf *forest-green* *black-color*)
           (setf *firebrick* *black-color*))
    ))

(set-mac-colors)
 
(defun create-display (&key
		       (width 300)
		       (height 300)
                       (background *yellow-color*)
                       (foreground *black-color*)
		       (display-name 'display-window))
  (declare (special *display*))
  (setf *display* (make-instance 'window 
                 :window-title (string display-name)
                 :color-p t
                 :view-size (make-point height width)
                 )) 
   
  (set-part-color *display* :frame *red-color*)
  (if background 
    (set-back-color *display* (eval background))
    (set-back-color *display* *yellow-color*))
  (if foreground
    (set-fore-color *display* (eval foreground))
    (set-fore-color *display* *black-color*))
  (setf *display-width* width)
  (setf *display-height* height)
  *display*)
 
 
;; makes the display display visible on the screen, if it is not already
(defun show-display (&optional (display *display*))
  (window-show display)
  (set-window-layer display 0)
  )

;; makes the display display invisible on the screen, if it is not already
(defun hide-display (&optional (display *display*))
  (window-hide display))

;; clears the given display completely
(defun clear-display (&optional (display *display*))
  (erase-region  display (clip-region  display)))

;; eliminates the given display completely
(defun destroy-display (&optional (display *display*))
  (window-close display))

 

(defun draw-screen-point (x y &optional (scrn  *display*)
		     (color *black-color*) (radius *draw-point-radius*))
  (set-drawing-color color  scrn)
  (frame-oval scrn 
              (-  x radius)
              (-  y radius)
              (+ x radius)
              (+  y radius)))

(defun set-drawing-color (&optional (color *black-color*)
                                    (scrn  *display*))
  (if (numberp color) 
    (set-fore-color scrn color)
    '(warn "Set color called with garbage color ~s" color)))

(defun draw-line (a-x a-y b-x b-y &optional (display *display*)
		      (color *black-color*))
  (set-drawing-color color display)
  (move-to  display a-x a-y)
  (line-to display b-x b-y))

(defun draw-lines (points &optional (display *display*)
		      (color *black-color*))
 
  (when (listp (first points))
    (warn "draw-lines called with wrong fromat")
    (setf points (loop for p in points append p)))
  (set-drawing-color color display)
  (move-to  display (first points) (second points))
  (setf points(cddr points))
  (loop while points do
        (line-to display (first points) (second  points))
        (setf points(cddr points))))

(defun draw-polygon (polygon &optional (display *display*)
                             (color *black-color*) (shape :complex))
 
  (when (listp (first polygon))
    (warn "draw-polygon called with wrong format")
    (setf polygon (loop for p in polygon append p)))
  (draw-lines `(,@polygon ,(first polygon) ,(second polygon))
              display color))

(defun draw-poly (points &key (display *display*)
			 (color *black*)
			 (shape :complex))
  (draw-polygon points display  color shape))
  
(defun display-foreground (display)
  (get-fore-color display))

(defun display-background (display)
  (get-back-color  display))

;;; Generic:DRAW                         Created: Wed Apr 15 09:59:34 1992

(defgeneric draw (what-to-draw &key color update clear)
  (:documentation "Display."))


;;; Method:DRAW                          Created: Thu Apr  9 12:50:21 1992

(defmethod draw ((list list) &key (color 1) (update t) (clear nil))
  (loop for item in list do
    (draw item :color color :update update :clear clear)))

(defun x-force-output (&rest rest)
  (ignore  rest)
  nil)
 


(defun string-size (string)
  (make-point  (+ 10 (string-width string))
              20))

(eval-when (eval compile load)
  (defmacro with-clip-rect-intersect (rect &rest body)
    (let ((old (gensym))
          (new (gensym)))
      `(let ((,old (#_NewRgn))
             (,new (#_NewRgn)))
         (#_getclip ,old)
         (#_rectrgn ,new ,rect)
         (#_SectRgn ,old ,new ,new)
         (#_SetClip ,new)
         (unwind-protect
           (progn ,@body)
           (#_SetClip ,old)
           (#_DisposeRgn ,old)
           (#_DisposeRgn ,new)))))

  ) ;end eval-when



(defun draw-string (string h v  &key (color nil)
                           (update t)
                           (display *display*)
                           (font *default-font*))                         
  (move-to display h v)
  (if color
    (set-drawing-color color *display*))
  (if font
    (set-view-font display font))
  (map nil 
        #'(lambda(x)  (stream-tyo display (character x)))
        string))
        
         
  
 

