;;;   -*- Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Mode: LISP -*-

;;; ************************
;;; ** WINDOWING ROUTINES **
;;; ************************
(in-package "COMMON-LISP-USER")

;;; constant information about the system being used
(defvar *host*)   ;the host machine
(defvar *x-display* "not yet defined" );the display on the host
(defvar *screen*) ;the screen being used
(defvar *root*)   ;the root display of the host
(defvar *default-font-name* "6x10")
(defvar *default-font* )
(defvar *default-pixelsperunit* 7)

;; system visual information
(defvar *x-colormap*)     ;the screen default color map
(defvar *x-visual*)       ;visual information for the default window
(defvar *x-visual-class*) ;the visual-class of the default window
;; standard colors
(defvar *black* 1)
(defvar *white* 0)
(defvar *gray*)
(defvar *red*)
(defvar *green*)
(defvar *blue*)
(defvar *grey*)
(defvar *yellow*)
(defvar *orange*)
(defvar *magenta*)
(defvar *cyan*)
(defvar *brown*)
(defvar *cornflower-blue*)
(defvar *forest-green*)
(defvar *firebrick*)
(defvar *foreground* 1)
(defvar *background* 0)



;; sets up the global variables and colors.  If the system is
;; monochrome, *white* is set to white, and all other colors are
;; set to black

;;; ***************************** DISPLAYS ******************************

;; class: DISPLAY
;; Includes a window and a graphics context
(defclass display ()
  ((window :initarg :window :accessor display-window)
   (gcontext :initarg :gcontext :accessor display-gcontext)
   (foreground :initarg :foreground :accessor display-foreground)
   (background :initarg :background :accessor display-background))
  (:documentation "a window along with its graphics context"))

(defun create-display (&key
		       (width 750)
		       (height 750)
		       (pixelsperunit *default-pixelsperunit*)
		       (display-name 'display-window)
		       (background nil)
		       (foreground nil)
		       (window nil)
		       (gcontext nil))
  (or background (setf background *background*))
  (or foreground (setf foreground *foreground*))
  (or window
      (setf window
	  (xlib:create-window
	   :parent *root*
	   :x 0
	   :y 0
	   :width width
	   :height height 
	   :background background
	   :border foreground
	   :border-width 2
	   :colormap *colormap*
	   :bit-gravity :center
	   :event-mask '(:exposure :button-press)
	   :backing-store :always)))
  (or gcontext
   (setf gcontext (xlib:create-gcontext
		    :drawable window
		    :background background 
		    :foreground foreground 
		    :font *default-font*)))
  (setf *window-center* (truncate width 2))
  (setf *window-y-center* (truncate height 2))
  ;;(setf *color-ramp* (define-ramp '(0.0 0.0 1.0) '(1.0 0.0 0.0) 50))
  
  
  ;; Set window manager hints
  ;;
  (xlib:set-standard-properties
   window
   :name display-name
   :icon-name display-name
   :resource-name "Simulation Display"
   :resource-class 'display-window
   :command '(create-display)
   :x 0 :y 0
   :width width :height height
   :min-width 100 :min-height 100
   :input :off
   :initial-state :normal)
  (xlib:map-window window)
  (make-instance 'display :window window :gcontext gcontext
		 :foreground foreground  :background background))

(defun initialize-display (host &key 
				(width 750)
				(height 750)
				(pixelsperunit *default-pixelsperunit*)
				(display-name 'display-window)
				(display-num 0)
				(protocol nil))
  (initialize-x host :display-num display-num :protocol protocol)
  (setf *display*
	(create-display :width width :height height
			:pixelsperunit pixelsperunit
			:display-name display-name)))

;; changes the size of the window in display to new-width and new-height
(defun resize-display (new-width new-height &key (display *display*))
  (setf (xlib:drawable-height (display-window display)) new-height)
  (setf (xlib:drawable-width (display-window display)) new-width)
  (x-force-output))

;; moves the upper left corner of the window in display to (new-x, new-y)
(defun move-display (new-x new-y &key (display *display*))
  (setf (xlib:drawable-x (display-window display)) new-x)
  (setf (xlib:drawable-y (display-window display)) new-y)
  (x-force-output))

;; changes the display background and foreground colors
(defun set-display-colors (&key (background NIL) (foreground NIL)
				(display *display*))
  (when background
    (setf (display-background display) background))
  (when foreground
    (setf (display-foreground display) foreground)))

;;; *********************** OTHER ROUTINES ************************




(defun initialize-x (host &key (display-num 0) (protocol nil))
  (setf *count* 0)
  (setf *host* host)
  (setf *x-display*
	(xlib:open-display host
			   :display display-num
			   :protocol protocol))
  ;;(setf *default-font* (xlib:open-font *x-display* *default-font-name*))
  (setf *screen* (xlib:display-default-screen *x-display*))
  
  (setf *root* (xlib:screen-root *screen*))

  (setf *colormap* (xlib:screen-default-colormap *screen*))

  
  (setf *visual* (xlib:visual-info (xlib:window-display *root*)
				   (xlib:window-visual *root*)))
  (setf *visual-class* (xlib:visual-info-class *visual*))

  (setf *white* (xlib:alloc-color *colormap* "white"))

  (setf *black* (xlib:alloc-color *colormap* "black"))

  ;; set other colors appropriately
  (if (or (equal *visual-class* :gray-scale)
	  (equal *visual-class* :static-gray))
      ;; if black-and-white, set all other colors to black
      (progn (setf *gray* *black*)
	     (setf *red* *black*)
	     (setf *green* *black*)
	     (setf *blue* *black*)
	     (setf *yellow* *black*)
	     (setf *orange* *black*)
	     (setf *magenta* *black*)
	     (setf *cyan* *black*)
	     (setf *brown* *black*)
	     (setf *cornflower-blue* *black*)
	     (setf *forest-green* *black*)
	     (setf *firebrick* *black*)
	     )
      ;; else set other colors correctly
    (progn (setf *gray* (xlib:alloc-color *colormap* "gray"))
	     (setf *red* (xlib:alloc-color *colormap* "red"))
	     (setf *green* (xlib:alloc-color *colormap* "green"))
	     (setf *blue* (xlib:alloc-color *colormap* "blue"))
	     (setf *yellow* (xlib:alloc-color *colormap* "yellow"))
	     (setf *orange* (xlib:alloc-color *colormap* "orange"))
	     (setf *magenta* (xlib:alloc-color *colormap* "magenta"))
	     (setf *cyan* (xlib:alloc-color *colormap* "cyan"))
	     (setf *brown* (xlib:alloc-color *colormap* "brown"))
	     (setf *cornflower-blue* (xlib:alloc-color *colormap* 
						       "cornflower blue"))
	     (setf *forest-green* (xlib:alloc-color *colormap* "forest green"))
	     (setf *firebrick* (xlib:alloc-color *colormap* "firebrick"))
	     ))
  
  (setf *default-font* (xlib:open-font *x-display* *default-font-name*))
  )
	

(defun define-color (red green blue  &key (colormap *colormap*))
  (xlib:alloc-color
   colormap
   (xlib:make-color
    :red red
    :green green
    :blue blue)))


(defun define-ramp (start end num-shades &key (colormap *colormap*))
  "Create a color ramp."
  (let ((new-colors (xlib:alloc-color-cells  colormap num-shades))
	(red (first start))
	(green (second start))
	(blue (third start))
	(red-incr (/ (- (first end) (first start)) num-shades))
	(green-incr (/ (- (second end) (second start)) num-shades))
	(blue-incr (/ (- (third end) (third start)) num-shades)))
    (loop for color in new-colors do
	  (incf red red-incr)
	  (incf green green-incr)
	  (incf blue blue-incr)
	  (xlib:store-color
	   colormap color
	   (xlib:make-color
	    :red (max 0.0 (min red  1.0))
	    :green (max 0.0 (min green 1.0))
	    :blue (max 0.0 (min blue 1.0)))))
    new-colors))

;; forces output in *x-display*
(defun x-force-output ()
  (xlib:display-force-output *x-display*))

;; makes the display display visible on the screen, if it is not already
(defun show-display (&optional (display *display*))
  (xlib:map-window (display-window display))
  (x-force-output))

;; makes the display display invisible on the screen, if it is not already
(defun hide-display (&optional (display *display*))
  (xlib:unmap-window (display-window display))
  (x-force-output))

;; clears the given display completely
(defun clear-display (&optional (display *display*))
  ;; erase the window
  (draw-rect
   0 0 (xlib:drawable-width (display-window display))
  (xlib:drawable-height (display-window display))
  display :color (display-background display))
  ;; update the display
  (x-force-output))


;; eliminates the given display completely
(defun destroy-display (&optional (display *display*))
  (xlib:destroy-window (display-window display))
  (x-force-output))

;; returns the current width of the given display
(defun display-width (&optional (display *display*))
  (xlib:drawable-width (display-window display)))

;; returns the current height of the given display
(defun display-height (&optional (display *display*))
  (xlib:drawable-height (display-window display)))
