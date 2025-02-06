;;   -*- Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10; Mode: LISP -*-

;;; ********************************************
;;; ** FUNDAMENTAL GRAPHICAL DISPLAY ROUTINES **
;;; ********************************************

(in-package 'common-lisp-user)

(defvar *postscript* nil)
(defvar *ps-font-scale* 0.8)
(defvar *ps-adjust-scale* 1.0)
(defvar *last-postscript-color* nil)
(defvar *last-postscript-line-width* 0.1)
(defvar *postscript-color-density* 1.0)
(defvar *default-postscript-scale* 1.0)
(defvar *min-x*)
(defvar *min-y*)
(defvar *max-x*)
(defvar *max-y*)
(defvar *postscript-scale*)

;;  The command drawedge is complete; it actually draws the edge.
;;  The command newcircle should be followed by either "fill" or "stroke".
;;
(defvar *eps-prefix*
  "%!
%%Title: ~s
%%Creator: GFX
%%CreationDate: ~a
%%For: ~a
%%BoundingBox: ~d ~d ~d ~d
%%Pages: 0
%%EndComments
%!
")

(setf postscript-prefix
      (list "%!"
	    "/inch {72 mul} def"
	  ""
#|
	  "%4.25 inch 5.5 inch translate % Origin (from lower left)."
	  "0 rotate                     % Rotate (degrees ccw)."
	  "1 1 scale                    % Scale the picture."
	  "0.1 setlinewidth             % Affected by scaling."
|#
	  ""
	  "/drawedge                    % x1 y1 x0 y0 drawedge"
	  "  {newpath moveto lineto stroke"
	  "  } def"
	  ""
	  "/newcircle                   % xc yc r newcircle"
	  "  {newpath 0 360 arc"
	  "  } def"
	  ""
	  "/rhcircle                   % xc yc r rhcircle"
	  "  {newpath 270 90 arc"
	  "  } def"
	  ""
	  "/lhcircle                   % xc yc r lhcircle"
	  "  {newpath 90 270 arc"
	  "  } def"
	  ""
	  "/rectangle                   % lx ly hx hy rectangle"
	  "  {4 copy moveto"            ; hx hy
	  "   4 1 roll exch lineto"     ; lx hy
	  "   exch 4 2 roll lineto"     ; lx ly
	  "   lineto closepath"         ; hx ly

	  "  } def"
	  "/l {lineto} bind def"
	  "/s {stroke} bind def"
	  "/n {newpath} bind def"
	  "/clp {closepath} bind def"
	  "/gs {gsave} bind def"
	  "/gr {grestore} bind def"
	  "/m {moveto} bind def"
	  ""
	  "0 setlinecap 0 setlinejoin"
	  "/setrgb                      % r g b setrgb"
	  "  {setrgbcolor} def"         ; Here so it's easy to modify.
	  ""
	  "0.0 0.0 0.0 setrgb"
	  "/Times-Roman findfont 11.0 scalefont setfont"
	  ""
	  ))

(defmacro with-line-width (width &rest body)
  `(let ((*line-width* ,width))
     (declare (special *line-width*))
     ,@body))

(defmacro without-postscript (#-ecl() &rest body)
  `(let ((*postscript* nil))
    (declare (special *postscript*))
    ,@body))


(defun min-screen-point (bb)
  (let ((p1 (screen-point (first bb) *viewport* nil))
	(p2 (screen-point (second bb) *viewport* nil))
	(p3 (screen-point (list(first(first bb))
			       (second(second bb)))
			  *viewport* nil))
	(p4 (screen-point (list(second(first bb))
			       (first(second bb)))
			  *viewport* nil))
	(*ps-x* 0)
	(*ps-y* 0))
    (declare(special *px-x* *ps-y*))
    (loop for point in (list p1 p2 p3 p4)
	  minimize (px (first point) :cbbox nil) into min-x
	  minimize (py (second point) :cbbox nil) into min-y
	  finally (return (list min-x min-y)))))

(defmacro with-postscript
  ((file-name
    &key (scale 1.0) (draw-bbox nil)
    (title "GFX Graphics"))
   (&rest before)
   &body body)
  (setf scale `(*  *ps-adjust-scale* ,scale))
  `(progn
     (setf *ps-x* 0)
     (setf *ps-y* 0)
     (let* ((bb (or (bbox (list ,@before)) '((0.0 0.0)(1.0 1.0))))
	    (offset  (min-screen-point bb)))
       (setf *ps-y* (-  (second offset) 0))
       (setf *ps-x* (- (first offset) 20))
       (pp ,file-name)
       (ppq  *ps-x*  *ps-y*)
       (with-string-output-stream
	(*postscript*)
	(setf *min-x* 1.0e6 *min-y* 1e10 *max-x* 0 *max-y* 0
	      *postscript-scale* (* *default-postscript-scale* ,scale))
	(ppq 	 *postscript-scale* *default-postscript-scale* ,scale)
	,@body
	(when ,draw-bbox
	  (set-ps-color *black*)
	  (format *postscript* "~a ~a ~a ~a rectangle stroke ~%"
		  (* ,scale *min-x*)
		  (* ,scale *min-y*)
		  (* ,scale *max-x*)
		  (* ,scale  *max-y*)))
	;;  Place the last instruction.
	(write-line "showpage" *postscript*)
	(with-open-file
	 (*ps2* ,file-name :direction :output)
	 (format *ps2* *eps-prefix*
		 ,title
		 (date-string)
		 "Rick"
		 (max 0 (* ,scale *min-x*))
		 (max 0 (* ,scale *min-y*))
		 (* ,scale *max-x*)
		 (* ,scale  *max-y*))
	 (loop for line in postscript-prefix
	       do (write-line line *ps2*)
	       )
	 (format  *ps2* "~a  ~a scale~%" ,scale ,scale)
	 (format  *ps2* "~a setlinewidth~%" (* 1.0 *line-width*))
	 (setf *last-postscript-line-width* *line-width*)
	 (setf *last-postscript-color* *black*)
	 (apply #'format *ps2* "~a ~a ~a setrgb~%"
		(rgb-color *black*))
	 (format *ps2* "~a" (get-output-stream-string *postscript*)))
	nil))))

(defmacro with-geomview
  ((file-name
    &key (scale 1.0) (draw-bbox nil)
    (title "GFX Graphics"))
   (&rest before)
   &body body)
  `(progn
     (setf *ps-x* 0)
     (setf *ps-y* 0)
     (let* ((bb (or (bbox (list ,@before)) '((0.0 0.0)(1.0 1.0))))
	    (offset  (min-screen-point bb)))
       (setf *ps-x* (- (first offset) 10))
       (setf *ps-y* (-  (second offset) 10))
       '(ppq  *ps-x*  *ps-y*)
       (with-string-output-stream
	(*postscript*)
	(setf
	 *min-x* 1.0e6
	 *min-y* 1e10
	 *max-x* 0
	 *max-y* 0
	 *postscript-scale* (* *default-postscript-scale* ,scale))
	,@body
	,@(when draw-bbox
	    '((set-ps-color *black*)
	      (format *postscript* "~a ~a ~a ~a rectangle stroke ~%"
		      *min-x* (py *min-y* :cbbox nil)
		      *max-x* (py *max-y* :cbbox nil))))
	
	;;  Place the last instruction.
	(write-line "showpage" *postscript*)
	(with-open-file
	 (*ps2* ,file-name :direction :output)
	 (format *ps2* *eps-prefix*
		 ,title
		 (date-string)
		 "Rick"
		 (* ,scale *min-x*)
		 (* ,scale *min-y*)
		 (* ,scale *max-x*)
		 (* ,scale  *max-y*))
	 (loop for line in postscript-prefix
	       do (write-line line *ps2*)
	       )
	 (format  *ps2* "~a  ~a scale~%" ,scale ,scale)
	 (format  *ps2* "~a setlinewidth~%" (* 1.0 *line-width*))
	 (setf *last-postscript-line-width* *line-width*)
	 (setf *last-postscript-color* *black*)
	 (apply #'format *ps2* "~a ~a ~a setrgb~%"
		(rgb-color *black*))
	 (format *ps2* "~a" (get-output-stream-string *postscript*)))
	nil))))

(defmacro set-ps-color(color)
  `(unless (eql ,color *last-postscript-color*)
    (setf *last-postscript-color* ,color)
    (apply #'format *postscript* "~a ~a ~a setrgb~%"
     (rgb-color ,color))))
(defmacro set-ps-linewidth (linewidth)
  `(unless (eql ,linewidth *last-postscript-line-width*)
    (setf *last-postscript-line-width* ,linewidth)
    (format  *postscript* "~a setlinewidth~%" (* 1.0 ,linewidth))))

(defmacro when-postscript ((&key  (color *black*))
			   &body body)
  `(when *postscript*
	 (or ,color (setf ,color *black*))
	 (set-ps-color ,color)
	 (set-ps-linewidth *line-width*)
	 ,@body))

(defun setminx (n)
  (if (minusp n) (pp "minus" n))
  (setf *min-x* (min *min-x* n)
	*max-x* (max *max-x* n))
  n)
(defun setminy (n)
  (if (minusp n) (pp "minus" n))
  (setf *min-y* (min *min-y* n)
	*max-y* (max *max-y* n))
  n)

(defun px(num  &key (cbbox t))
  (let ((n (- (truncate num) *ps-x*)))
    (if cbbox
	(setminx n))
    n))

(defun py(num &key (cbbox t))
  (let ((n (- (* 2 *window-center*)
	      (- (truncate num) *ps-y*))))
    (if cbbox (setf *min-y* (min *min-y* n)
		    *max-y* (max *max-y* n)))
    n))

(defun rgb-color (color)
  #+Lucid
  (let ((x-color (first (xlib:query-colors *colormap* (list color)))))
    (if (eql color *white*)
	(list 1.0 1.0 1.0)
	(list (*  *postscript-color-density* (xlib:color-red x-color))
	      (* *postscript-color-density*  (xlib:color-green x-color))
	      (* *postscript-color-density* (xlib:color-blue x-color))
	      )))

  #+CCL
  (mapcar #'(lambda (c) (/ c 65535.0))
	  (multiple-value-list (ccl:color-values (ccl-color color)))  
          )
  )

;; global display control variables
(defvar *point-style*)
(defvar *point-diameter*)
(defvar *point-radius*)
(defvar *line-width*)

(defmacro xv (val)
  `(coerce (round ,val) 'xlib:int16))
	      
;; internal routine used for various drawing modes
;; sets the drawing mode of display to :draw, :erase, or :flip
(defun set-drawing-mode (display operation)
  (cond ((eql operation :draw)
	 (setf (xlib:gcontext-function (display-gcontext display))
	       boole-1)) ;copy source
	((eql operation :erase)
	 (setf (xlib:gcontext-function (display-gcontext display))
	       boole-c1)) ;copy inverse of source
	((eql operation :flip)
	 (setf (xlib:gcontext-function (display-gcontext display))
	       boole-xor)) ;invert bits using XOR
	(t (error "drawing mode ~A is not supported"))))
;; draws a point at window coordinates (x,y) in display with mode
;; operation.  The point is drawn in the style given by *point-style*

(defvar *draw-point-radius* 4 "The radius of a point on the screen")
(defun draw-point (x y &optional (display *display*)
		     (color *black*)(fill-p t)
		     (radius *draw-point-radius*))
  '(pp 'draw-point x y)
  (xlib:with-gcontext
      ((display-gcontext display)
       :foreground color)
    (xlib:draw-arc
     (display-window display)
     (display-gcontext display)
     (- (xv x) radius)
     (-  (xv y) radius)
     (* 2 radius)
     (* 2 radius)
     ;; the next 2 numbers must be float, or you get a
     ;; bus error 
     0.0
     (* 2.0 pi)
     fill-p))
  (when-postscript
   (:color color)
   (format *postscript* "~a ~a ~a newcircle ~a~%" 
	   (px x)
	   (py y)
	   radius (if fill-p "fill" ""))))

(defun draw-circle (x y radius &key (display *display*)
		      (color *black*)
		      (fill-p nil)
		      (start 0.0)
		      (end (* 2 pi))
		      )
  '(pp 'draw-circle x y)
  (xlib:with-gcontext
   ((display-gcontext display)
    :foreground color)
   (xlib:draw-arc
    (display-window display)
    (display-gcontext display)
    (- (xv x) radius)
    (-  (xv y) radius)
    (* 2 radius)
    (* 2 radius)
    ;; the next 2 numbers must be float, or you get a
    ;; bus error 
    (float start)
    (float end)
    fill-p))
  (when-postscript
   (:color color)
   (format *postscript* "~a ~a ~a newpath ~a ~a arc ~a~%" 
	   (px x)
	   (py y)
	   radius
	   (radians-to-degrees start)
	   (radians-to-degrees end)
	   (if fill-p "fill" "stroke"))))

(defun draw-elipse-ll (x y width height &key (display *display*)
			 (color *black*)
			 (fill nil)
			 (start 0.0)
			 (end (* 2 pi))
			 )
  (setf (xlib:gcontext-line-width (display-gcontext display)) *line-width*)
  (setf (xlib:gcontext-foreground (display-gcontext display))
	(or color *black*))
  (xlib:with-gcontext
   ((display-gcontext display)
    :foreground color)
   (xlib:draw-arc
    (display-window display)
    (display-gcontext display)
    (- (xv x) width)
    (-  (xv y) height)
    (* 2 width)
    (* 2 height)
    ;; the next 2 numbers must be float, or you get a
    ;; bus error 
    (float start)
    (float end)
    fill))
  (when-postscript
   (:color color)
   (format *postscript*
	   "gs ~a ~a scale ~a setlinewidth ~a  ~a 1 newpath ~a ~a arc ~a gr~%" 
	   width
	   height
	   (float (/ *line-width* (max width height)))
	   (setminx (float (/ (px x :cbbox nil) width)))
	   (setminy (float (/ (py y  :cbbox nil) height)))
	   (radians-to-degrees start)
	   (radians-to-degrees end)
	   (if fill "fill" "stroke"))
   
   ))



(defun draw-arrow-circle (x y radius &key (display *display*)
			    (color *black*)
			    (start 0.0)
			    (end (* 2 pi))
			    (ratio 0.2))
  
  (draw-circle x y radius :display display :color color
	       :fill-p nil :start start :end end)
  (let* ((end-pos-x (* radius (cos end)))
	 (end-pos-y  (-(* radius (sin end))))
	 (sx (+ x end-pos-x
		(* (- ratio) end-pos-y)))
	 (sy (+ y end-pos-y
		(* ratio end-pos-x)))
	 (ex (+ x end-pos-x
		(* ratio end-pos-y)))
	 (ey (+ y end-pos-y
		(* (- ratio) end-pos-x))))
    (draw-arrow-line sx sy ex ey display :color color :ratio 1)))

(defun draw-curved-line
  (x1 y1 x2 y2 display &key
      (max-head-size 15)
      (min-head-size 10)
      (color *black*)
      (oriented nil)
      (curve-scale .1)
      (ratio 1/8)
      (label nil))
  (let*
      ((nx (- y1 y2))
       (ny (- x2 x1))
       (center-x1 (+ (* 2/3 x1) (* 1/3 x2)))
       (center-y1 (+ (* 2/3 y1) (* 1/3 y2)))
       (center-x2 (+ (* 1/3 x1) (* 2/3 x2)))
       (center-y2 (+ (* 1/3 y1) (* 2/3 y2)))
       (px1 (+ center-x1 (* curve-scale nx)))
       (py1 (+ center-y1 (* curve-scale ny)))
       (px2 (+ center-x2 (* curve-scale nx)))
       (py2 (+ center-y2 (* curve-scale ny))))
    (xlib:with-gcontext
     ((display-gcontext display)
	 :foreground color)
     (without-postscript
      ()
      (draw-line x1 y1 px1 py1 display color)
      (draw-line px1 py1 px2 py2 display color)
      (if oriented
	  (draw-arrow-line px2 py2 x2 y2 display
			   :color color
			   :max-head-size max-head-size
			   :min-head-size min-head-size
			   :ratio ratio)
	(draw-line px2 py2 x2 y2 display color)))
     (when-postscript
      (:color color)
      (format *postscript*
	      "n ~a ~a m ~% ~a ~a ~a ~a ~a ~a   curveto~% gs s gr~%"
	      (PX X1) (PY Y1)
	      (PX PX1) (PY PY1)
	      (PX PX2) (PY PY2)
	      (PX X2) (PY Y2)
	      (PX X1) (PY Y1)
	      ))
     (if oriented
	 (let ((dx (- x2 px2))
	       (dy (- y2 py2)))
	   (cond ((or  max-head-size min-head-size)
		  (if (zerop (setf len (sqrt (+ (* dx dx) (* dy  dy)))))
		      (return-from draw-curved-line nil))
		  (if (> len max-head-size)
		      (setf dx (* max-head-size dx  (/ len))
			     dy (* max-head-size dy  (/ len)))
		       (if (< len min-head-size)
			   (setf dx (* min-head-size dx  (/ len))
				 dy (* min-head-size dy  (/ len)))))))
	    (setf nx (* 1/4 (- dy))  ny (* 1/4 dx))
	    
	    (setf poly
		  `((,x2 ,y2)
		    (,(xv(- x2  dx nx)) ,(xv(- y2  dy ny)))
		    (,(xv(- x2  dx (- nx))) ,(xv(- y2  dy (- ny))))
		    (,x2 ,y2)))
	    
	    (draw-poly poly :display display
		       :color color :shape :convex :fill t)))
      (set-drawing-mode display :draw)
      )))



;; draws a line between (x1,y1) and (x2,y2) in display with mode
;; operation
(defun draw-line (x1 y1 x2 y2 display &optional 
		     (color *black*))
  ;; set the line width properly
  (setf (xlib:gcontext-line-width (display-gcontext display)) *line-width*)
  (setf (xlib:gcontext-foreground (display-gcontext display))
	(or color *black*))
  (xlib:draw-line
   (display-window display)
   (display-gcontext display)
   (xv x1)
   (xv y1)
   (xv x2)
   (xv y2))
  (set-drawing-mode display :draw)
  (when-postscript
   (:color color)
   (format *postscript* "~a ~a ~a ~a drawedge~%"
	   (px x1) (py y1) (px x2) (py y2)))
  )



(defun draw-arrow-line (x1 y1 x2 y2 display &key (ratio 1/8)
			   (operation :draw)
			   (max-head-size 15)
			   (min-head-size 10)
			   (color *black*)
			   )
  
  (set-drawing-mode display operation)
  ;; set the line width properly
  (setf (xlib:gcontext-line-width (display-gcontext display)) *line-width*)
  (setf (xlib:gcontext-foreground (display-gcontext display))
	(or color *black*))
  
  ;; do the drawing
  (let ((x1 (xv x1))
	(y1 (xv y1))
	(x2 (xv x2))
	(y2 (xv y2))
	(dx (* ratio (- x2 x1)))
	(dy (* ratio (- y2 y1)))
	(len nil)
	(nx nil)
	(ny nil)
	(poly nil))
    
    (xlib:draw-line
     (display-window display) (display-gcontext display) x1 y1 x2  y2)
    
    (when-postscript (:color color)
		     (format *postscript* "~a ~a ~a ~a drawedge~%"
			     (px x1) (py y1) (px x2) (py y2)))
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
    
    (setf poly
	  `((,x2 ,y2)
	    (,(xv(- x2  dx nx)) ,(xv(- y2  dy ny)))
	    (,(xv(- x2  dx (- nx))) ,(xv(- y2  dy (- ny))))
	    (,x2 ,y2)))
    
    (draw-poly poly :display display :color color :shape :convex :fill t)
    (set-drawing-mode display :draw)))

;; draws a (filled) rectangle with upper left corner (x1,y1), width w, 
;; and height h in display with mode operation
(defun draw-rect (x1 y1 w h display &key (color *background*)
		     (fill t) (operation :draw))
  (xlib:with-gcontext
   ((display-gcontext display)
    :foreground color)
   (xlib:draw-rectangle (display-window display)
			(display-gcontext display)
			(xv x1)
			(xv y1)
			(xv w)
			(xv h)
			fill)) ;filled
  (when-postscript
   (:color color)
   (format *postscript* "~a ~a ~a ~a rectangle ~a ~%"
 	   (px x1) (py y1)
	   (px  (+ x1 w))
	   (py (+ y1 h))
	   (if fill "fill" "")))
  
  (set-drawing-mode display :draw)) ;return to normal mode


;; draws an unfilled rectangle with upper left corner (x1,y1), width w, 
;; and height h in display with mode operation
(defun draw-frame (x1 y1 w h display &optional (operation :draw))
  (set-drawing-mode display operation)
  (xlib:draw-rectangle (display-window display)
		       (display-gcontext display)
		       (xv x1)
		       (xv y1)
		       (xv w)
		       (xv h)
		       nil) ;not filled
  (set-drawing-mode display :draw)) ;return to normal mode


;; sets the current drawing color in display to color
(defun set-drawing-color (color display)
  (setf (xlib:gcontext-foreground (display-gcontext display))
	color))


;;;;;;;;;;;;;;;;;;;;; New Routines 12/3/90

;; draws a line between the points in the specified point list in display with mode
;; operation
(defun draw-lines (points display &optional (operation :draw))
  (set-drawing-mode display operation)
  ;; set the line width properly
  (setf (xlib:gcontext-line-width (display-gcontext display)) *line-width*)
  ;; do the drawing
  '(pp 'draw-lines points)
  (xlib:draw-lines (display-window display)
		   (display-gcontext display)
		   points)
  (set-drawing-mode display :draw)) ;return to normal mode

(defun draw-polygon (polygon &optional (display *display*)
                             (color *black*) (shape :complex))
  
  (when (listp (first polygon))
	(warn "draw-polygon called with wrong format")
	(setf polygon (loop for p in polygon append p)))
  (draw-poly polygon :display display :color color))

(defun draw-poly (points &key (display *display*)
			 (color *black*)
			 (shape :complex)
			 (fill t))
  (xlib:with-gcontext
      ((display-gcontext display) :foreground color)
    (xlib::fill-polygon
     (display-window display)
     (display-gcontext display)
     (loop for point in points
	   nconc
	   (list (first point) (second point)))
     nil
     shape))

  (when-postscript
   (:color color)
   (loop initially (format *postscript* "newpath ~a ~a moveto~%"
			   (px(first (first points)))
			   (py (second (first points)))
			   )
	 for point in (rest points)
     do (format *postscript* "~a ~a lineto~%"
		(px (first point)) (py (second point)))
     finally (format *postscript* "closepath ")
     (if fill (format *postscript* "fill "))
     )))


(defun draw-string (string pos &key (color 1) (update t) (clear nil)
			   (viewport *viewport*) (font *default-font*)
			   (boxed nil)
			   (justify :center))
  (let* ((slen (* 3 (length string)))
	 (orig-pos pos)
	 (pos(screen-point pos viewport nil))
	 (display (viewport-display viewport))
	 (just (case justify
		     (:right ( - slen))
		     (:center 0)
		     (:left (* 1 slen))
		     (t (error "Justify must be one of :left :center :right")))))
    
    (xlib:with-gcontext
     ((display-gcontext display)
      :foreground *background*
      )
     (without-postscript
      ()
      (draw-rect (+ (first pos)(- slen) just) (-(second pos) 5)
		 (+ 1 (* 2.0 slen) just) 10 display)
      (when boxed
	    (let ((p1x (+ (- *line-width*) just (first pos)(- slen) -2))
		  (p1y(+ (- *line-width*) (second pos) (- 5 ) -2))
		  (p2x (+ (+ *line-width*) just (first pos)(+ slen) 2))
		  (p2y(+ (+ *line-width*) (second pos) (+ 5) 2))
		  (*line-width* 1))
	      (declare (special *line-width*))
	      (draw-line p1x p1y p1x p2y *display* color)
	      (draw-line p2x p2y p1x p2y *display* color)
	      (draw-line p1x p1y p2x p1y *display* color)
	      (draw-line p2x p2y p2x p1y *display* color)))))
    
    (xlib:with-gcontext
     ((display-gcontext display)
      :foreground color :font font)
     (xlib:draw-glyphs (display-window display)
		       (display-gcontext display)
		       (+ just (first pos) (- slen))
		       (+  (or (second pos) 0) 3) string))
    
    (when-postscript
     (:color *background*)
     (setf string (string-right-trim '(#\Newline) string))
     (setf slen (* *ps-font-scale* (str-post-len string)))
     (let* ((center-y  (py (+ (second pos) 0)))
	    (radius (* 0.5 (- (py 0 :cbbox nil) (py 12 :cbbox nil))))
	    (p1x (px (+ (first pos) (- slen) (* 0.25 radius))))
	    (p2x (px (+ (first pos)
			(+ slen (-(* 0.25 radius))))))
	    )
       '(ppq p1x center-y p2x radius string)
       (format
	*postscript*
	"~a ~a ~a lhcircle fill ~%"
	p1x
	center-y
	radius)
       (format
	 *postscript*
	 "~a ~a ~a rhcircle fill ~%"
	 p2x
	 center-y
	 radius)
	(format
	 *postscript*
	"~a ~a ~a ~a rectangle fill ~%"
	p1x (- center-y radius) p2x (+ center-y radius))
       (set-ps-color color)
       
       (when boxed
	 (set-ps-linewidth 1)
	 (format
	  *postscript*
	  "~a ~a ~a lhcircle s ~%"
	  p1x
	  center-y
	  radius)
	 (format
	  *postscript*
	  "~a ~a ~a rhcircle s ~%"
	  p2x
	  center-y
	  radius)
	 (when (> p2x p1x)
	   (format *postscript* "~a ~a ~a ~a drawedge~%"
		   p1x  (setminy (- center-y radius))
		   p2x (setminy (- center-y radius)))
	   (format *postscript* "~a ~a ~a ~a drawedge~%"
		   p1x (+ center-y radius)
		   p2x  (+ center-y radius)))
	 (set-ps-linewidth  *line-width*))
       (format *postscript* "~a ~a m (~a) show~%"
	       (px (- (first pos) slen))
	       (py (+ 4 (second pos)))
	       string)))
    (if update (x-force-output))))

(defun str-post-len (str)
  (loop for i below (length str)
	for char = (aref str i) sum
	(cond
	 ((upper-case-p char) 6.1)
	  ((lower-case-p char) 4.2)
	  ((eql char #\Space) 2)
	  ((eql char #\.) 1)
	  (t 4.6))))
