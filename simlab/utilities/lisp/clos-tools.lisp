(in-package :user)

;;; Functions for finding all methods that dispatch off some subclass of 
;;; the domain class.
;;; 
;;; useful functions:

;;; clos:generic-function-name
;;; clos:generic-function-lambda-list
;;; clos:generic-function-methods
;;; subtypep   ; can be used to compare classes.
;;; (find-class 'standard-generic-function)  ; class of generic-functions
;;; fboundp ; symbol has function binding
;;; clos:method-specializers 
;;;   returns list of method specializers
;;;   a method specializer is one of
;;;   a) a class. e.g. #<Built-In-Class T>
;;;                    #<Standard-Class WEYLI::GENERAL-EXPRESSIONS>
;;;   b) a form:  (eql <object>)
;;;

(defmacro list-method-specializers (gen-fun-name)
   (let ((gen-fun (symbol-function gen-fun-name)))
     `(mapcar #'clos:method-specializers
              (clos:generic-function-methods ,gen-fun))))


(defun has-domain-specializer (spec-list)
  (let ((dom-class (find-class 'weyli::domain)))
    (some #'(lambda (x) (and (typep x 'class) (subtypep x dom-class)))
            spec-list)))

;;; takes generic function object.
;;; returns list of domain specializer lists.

(defun domain-specializers-of-gf (gf)
  (let ((dom-specializers))
    (dolist (method (clos:generic-function-methods gf))
      (let ((spec-list (clos:method-specializers method)))
        (if (has-domain-specializer spec-list)
            (push spec-list dom-specializers))))
    dom-specializers))


;;; evaluate body with gf argument bound successively to each generic function
;;; in package-name

(defmacro do-generic-functions ((gf package-name &optional (sorted? nil))
				&body body)
  `(let ((package (find-package ,package-name))
         (generic-function-class (find-class 'standard-generic-function)))
	,(if sorted?
	     `(let ((.gfs. nil))
		(do-symbols (symbol package)
		  (if (and (fboundp symbol)
			   (typep (symbol-function symbol)
				  generic-function-class))
		      (push (symbol-function symbol) .gfs.)))
		(setq .gfs. (sort .gfs.
				  #'(lambda (a b)
				      (string-lessp
					(string
					  (clos:generic-function-name a))
					(string
					  (clos:generic-function-name b))))))
		(loop for ,gf in .gfs.
		      do ,@body))
	     `(do-symbols (symbol package)
		(if (and (fboundp symbol)
			 (typep (symbol-function symbol)
				generic-function-class))
		    (let ((,gf (symbol-function symbol)))
			 (progn ,@body)))))))


(defun print-gf-spec-lists (gf spec-lists)
   (format t "~A:~%" (string-downcase
                   (symbol-name (clos:generic-function-name gf))))
   (dolist (spec-list spec-lists)
     (dolist (spec spec-list)
       (if (typep spec 'class)
           (format t "  ~A" (string-downcase
                           (symbol-name (clos:class-name spec))))
         (format t "  ~A" spec)))
     (format t "~%")))

                   
(defun print-domain-keyed-gfs (package-name)
  (do-generic-functions (gf package-name t)
    (let ((dom-spec-lists (domain-specializers-of-gf gf)))
       (if dom-spec-lists
           (print-gf-spec-lists gf dom-spec-lists)))))

#+ignore
(defun print-domain-keyed-gfs (package-name)
  (do-generic-functions (gf package-name t)
    (let ((dom-spec-lists (domain-specializers-of-gf gf)))
       (if dom-spec-lists
	   (print-gf-spec-lists gf
	     (loop for meth in (clos:generic-function-methods gf)
		   collect (clos::method-specializers meth)))))))

