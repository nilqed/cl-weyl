;;;   -*- Syntax: Common-Lisp; Package: Common-Lisp-User; Base: 10; Mode: LISP -*-

(in-package "COMMON-LISP-USER")


(defvar *default-display-machine* "arvak"
  "Machine on which to open an x-window.")
(defvar *default-window-size*  600)
(defvar *black* 1)
(defvar *white* 0)
(defun init-gfx(&key (host *default-display-machine*)
		     (name "SimLab")
		     (height  *default-window-size*)
		     (width  *default-window-size*)
		     (window nil)
		     (gcontext nil)
		     (display-num 0)
		     (foreground  *black*)
		     (background *white*))
  (setf *default-postscript-scale*
	(* 1.0 (/ *default-window-size*  (min height width))))
  (setf *background* background)
  (setf *foreground* foreground)

  (initialize-graphics
   :host host :name name :height height  :width width
   :foreground foreground :background background
   :window window
   :gcontext gcontext :display-num display-num))


   


