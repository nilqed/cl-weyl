;;; ==========================================================================
;;;		    Basic Utilities for SimLab Systems
;;; ==========================================================================
;;; (c) Copyright 1995 Cornell University

;;; basic-support.lisp,v 1.4 1995/04/07 17:16:58 rz Exp

(in-package "MAKE")


;; If this feature is set then there is a CL-USER package, otherwise
;; the USER package exits.  Also, the CLTL2 Loop syntax is used.

#+(or CCL-2 GENERA Excl)
(push :ANSI-CL *features*)

;; Apple Common Lisp does contain CLOS, it just doesn't indicate it on
;; the features list.
#+CCL-2
(push :clos *features*) 

;; Make sure we don't reload files more than necessary.
(setq make:*minimal-load* t)

;; Creates a directory pathname from pathname. If NAMES is included, these
;; names are appended to the directory.
(defun make-path-directory (pathname &rest names)
  (make-pathname :host (pathname-host pathname)
                 :device (pathname-device pathname)
                 :directory (append (pathname-directory pathname)
                                    names)))

(defun module-directory (&rest names)
  (let ((path (pathname make::*central-registry*)))
    (make-pathname :device (pathname-device path)
		   :directory (append
				(butlast (pathname-directory path))
				(mapcar #'(lambda (q)
					    (if (stringp q) q
					      (string-downcase (string q))))
					names)))))

(defun standard-binary-directory ()
  #+(and Lucid Sparc)   "lucid-sparc"
  #+(and Allegro Sparc) "allegro-sparc"
  #+MCL                 "mcl-binary"
  )  

;; Guarantee that there we can grow to at least MEGS megabytes of
;; memory.
(defvar *ensure-sufficient-memory-finished* nil)

(defun ensure-sufficient-memory (megs)
  #-Lucid
  (declare (ignore megs))
  (unless *ensure-sufficient-memory-finished*
    #+Lucid
    (let ((segs (* megs 16)))
      (declare (special lucid::*external-growth-limit*))
      (when (> segs lucid::*external-growth-limit*)
	(format t "~%;;; Increasing memory growth limit to ~D segements, ~DMB~%"
		segs megs)
	(change-memory-management :growth-limit segs)))
    (setq *ensure-sufficient-memory-finished* t)))

(defun parse-version-number (string)
  (unless (stringp string)
    (error "Expected ~S to be a string" string))
  (let* ((len (length string))
	 (major-start (or (position #\: string) -1))
	 (minor-start (position #\. string))
	 (subminor-start (position #\. string :start (+ minor-start 1)))
	 (string-end (if (char= #\$ (aref string (1- len))) (1- len)
			 len)))
    (cond ((null minor-start)
	   (values nil nil nil))
	  ((null subminor-start)
	   (values (read-from-string string nil nil
				     :start (1+ major-start) :end minor-start) 
		   (read-from-string string nil nil
				     :start (1+ minor-start) :end string-end)
		   nil))
	  (t 
	    (values (read-from-string string nil nil
				     :start (1+ major-start) :end minor-start) 
		    (read-from-string string nil nil
				      :start (1+ minor-start)
				      :end subminor-start)
		    (read-from-string string nil nil
				      :start (1+ subminor-start)
				      :end string-end))))))

(defmacro bind-symbol-names (name &body body)
  `(let ((major-sym
	   (intern (format nil "*~A-MAJOR-VERSION*" (symbol-name ,name))
		   'make))
	 (minor-sym
	   (intern (format nil "*~A-MINOR-VERSION*" (symbol-name ,name))
		   'make))
	 (subminor-sym
	   (intern (format nil "*~A-SUBMINOR-VERSION*" (symbol-name ,name))
		   'make)))
	,@body))

(defun adjust-system-version-numbers (name string)
  (bind-symbol-names name
    (multiple-value-bind (major minor subminor) (parse-version-number string)
      (cond ((null major))
	    ((null (symbol-value major-sym))
	     (setf (symbol-value major-sym) major))
	    ((not (= major (symbol-value major-sym)))
	     (error "This file came from major version ~D, expected ~D"
		  major (symbol-value major-sym))))
      (cond ((null minor))
	    ((null (symbol-value minor-sym))
	     (setf (symbol-value minor-sym) minor))
	    (t (setf (symbol-value minor-sym)
		     (+ (symbol-value minor-sym) minor))))
      (cond ((null subminor))
	    ((null (symbol-value subminor-sym))
	     (setf (symbol-value subminor-sym) subminor))
	    (t (setf (symbol-value subminor-sym)
		     (+ (symbol-value subminor-sym) subminor)))))))

(defmacro adjust-version-numbers (name string)
  (bind-symbol-names name
    `(progn
       (eval-when (load compile eval)
	 (proclaim '(special ,major-sym ,minor-sym ,subminor-sym))
	 (unless (boundp ',major-sym)
	   (setf (symbol-value ',major-sym) nil))
	 (unless (boundp ',minor-sym)
	   (setf (symbol-value ',minor-sym) nil))
	 (unless (boundp ',subminor-sym)
	   (setf (symbol-value ',subminor-sym) nil)))
       (eval-when (load eval)
	 (adjust-system-version-numbers ',name ,string)))))

(defun print-system-banner (name &optional (stream *standard-output*))
  (bind-symbol-names name
    (cond ((null (symbol-value subminor-sym))
	   (format stream ";;; ~A Version ~D.~D loaded. ~%"
		   (string-capitalize name)
		   (symbol-value major-sym) (symbol-value minor-sym)))
	  (t (format stream ";;; ~A Version ~D.~D.~D loaded. ~%"
		     (string-capitalize name)
		     (symbol-value major-sym) (symbol-value minor-sym)
		     (symbol-value subminor-sym))))))
       
