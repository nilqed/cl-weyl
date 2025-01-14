(ql:quickload :weyl)


(load "xref.lisp")



;;; for a in ../weyl/*.lisp; do echo \"$(basename $a .lisp)\";done
;;; - maintenance, new-topology, walk

(let ((dir "../weyl/"))
  (uiop:with-current-directory (dir)
      (xref:xref-files 
            "algebraic-extension"
            "avl"
            "differential-domains"
            "direct-sums"
            "domain-support"
            "fourier"
            "functions"
            "funct-spaces"
            "general"
            "lisp-numbers"
            "lisp-support"
            "matrix"
            "mesh"
            "morphisms"
            "multipole"
            "new-domains"           
            "packages"
            "quotient-fields"
            "rational-functions"
            "sets"
            "taylor"
            "topology"
            "tpower"
            "classes/algebraic-domains"
            "classes/general-classes"
            "classes/space-classes"
            "numbers/bigfloat"
            "numbers/gfp"
            "numbers/numbers"
            "polynomials/epolynomial"
            "polynomials/grobner"
            "polynomials/mpolynomial"
            "polynomials/poly-tools"
            "polynomials/sparsegcd"
            "polynomials/upolynomial"
            "vector-spaces/projective-space"
            "vector-spaces/quaternions"
            "vector-spaces/vector")))


(xref:display-database)

(if (not (probe-file "weyl-xref-db.txt"))
  (xref:write-callers-database-to-file "weyl-xref-db.txt"))


(with-open-file 
    (*standard-output* "weyl-trees.dat"
        :direction :output
        :if-exists :supersede)
            (xref:print-caller-trees))

(with-open-file 
    (*standard-output* "weyl-display-db.dat"
        :direction :output
        :if-exists :supersede)
            (xref:display-database))

(with-open-file 
    (*standard-output* "weyl-symbols.dat"
        :direction :output
        :if-exists :supersede)
             (do-external-symbols (s (find-package "WEYL")) (print s)))

(with-open-file 
    (*standard-output* "weyl-all-symbols.dat"
        :direction :output
        :if-exists :supersede)
             (do-all-symbols (s (find-package "WEYL")) (print s)))

(with-open-file 
    (*standard-output* "weyli-symbols.dat"
        :direction :output
        :if-exists :supersede)
             (do-external-symbols (s (find-package "WEYLI")) (print s)))

(with-open-file 
    (*standard-output* "weyli-all-symbols.dat"
        :direction :output
        :if-exists :supersede)
             (do-all-symbols (s (find-package "WEYLI")) (print s)))



;;; (let (symbols)
;;;  (do-external-symbols (s (find-package "WEYL"))
;;;    (push s symbols))
;;;  symbols)


#|
(ql::quickload :cl-package-locks)
(cl-package-locks::unlock-package :common-lisp)


(load "psgraph")
(defparameter *postscript-output-directory* "")
(defun psgraph-xref (&key (mode *default-graphing-mode*)
			  (output-directory *postscript-output-directory*)
			  (types-to-ignore *types-to-ignore*)
			  (compact t)
			  (shrink t)
			  root-nodes
			  insert)
  ;; If root-nodes is a non-nil list, uses that list as the starting
  ;; position. Otherwise tries to find all roots in the database.
  (multiple-value-bind (rooted cycles)
      (if root-nodes
	  (values (gather-tree root-nodes nil mode types-to-ignore compact))
	  (make-caller-tree mode types-to-ignore compact))
    (psgraph-output (append rooted cycles) output-directory shrink insert)))


(defun psgraph-output (list-of-trees directory shrink &optional insert)
  (let ((psgraph:*fontsize* 9)
	(psgraph:*second-fontsize* 7)
;	(psgraph:*boxkind* "fill")
	(psgraph:*boxgray* "0") ; .8
	(psgraph:*edgewidth* "1")
	(psgraph:*edgegray* "0"))
    (labels ((stringify (thing)
		(cond ((stringp thing) (string-downcase thing))
		      ((symbolp thing) (string-downcase (symbol-name thing)))
		      ((and (listp thing) (eq (car thing) :xref-list))
		       (stringify (cdr thing)))
		      ((listp thing) (stringify (car thing)))
		      (t (string thing)))))
      (dolist (item list-of-trees)
	(let* ((fname (stringify item))
	       (filename (concatenate 'string directory
				      (string-trim '(#\: #\|) fname)
				      ".ps")))
	  (format t "~&Creating PostScript file ~S." filename)
	  (with-open-file (*standard-output* filename
					     :direction :output
					     :if-does-not-exist :create
					     :if-exists :supersede)
	    ;; Note that the #'eq prints the DAG as a tree. If
	    ;; you replace it with #'equal, it will print it as
	    ;; a DAG, which I think is slightly ugly.
	    (psgraph:psgraph item
			     #'caller-tree-children #'caller-info shrink
			     insert #'eq)))))))

(defun caller-tree-children (tree)
  (when (and tree (listp tree) (not (eq (car tree) :xref-list)))
    (cadr tree)))

(defun caller-tree-node (tree)
  (when tree
    (cond ((and (listp tree) (eq (car tree) :xref-list))
	   (cdr tree))
	  ((listp tree)
	   (car tree))
	  (t
	   tree))))

(defun caller-info (tree)
  (let ((node (caller-tree-node tree)))
    (list node)))

;;; Code to print out graphical trees of CLOS class hierarchies.
;;; kfp/removed clos: (no package anymore)
;;; kfp/changed start-class 'anything to 'set
(defun print-class-hierarchy (&optional (start-class 'weyli:set) 
					(file "classes.ps"))
  (let ((start (find-class start-class)))
    (when start
      (with-open-file (*standard-output* file :direction :output)
	(psgraph:psgraph start 
			 #'cl::class-direct-subclasses
			 #'(lambda (x) 
			     (list (format nil "~A" (cl::class-name x))))
			 t nil #'eq)))))


;;; (xref:psgraph-xref)
(print-class-hierarchy)
(cl-package-locks::lock-package :common-lisp)

|#

(in-package :common-lisp)
(load "psgraph")
(defparameter *postscript-output-directory* "")
(defun print-class-hierarchy (&optional (start-class 'weyli:set) 
					(file "classes.ps"))
  (let ((start (find-class start-class)))
    (when start
      (with-open-file (*standard-output* file :direction :output)
	(psgraph:psgraph start 
			 #'sb-mop:class-direct-subclasses
			 #'(lambda (x) 
			     (list (format nil "~A" (class-name x))))
			 t nil #'eq)))))

(cl::print-class-hierarchy) ;;; -> classes.ps , ok