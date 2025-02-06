;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weyl Database Tools
;;; originated 24-JAN-95
;;; Venkatesh Gopalakrishnan (vxg@cs.cornell.edu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wtools.lisp,v 1.3 1995/04/12 16:15:56 vxg Exp
;;;
;;; These are some small functions that make it easier to load the
;;; weyl package into XREF and perform some simple XREF tasks.  It
;;; also includes a preliminary consistency checker for given commutative
;;; methods.
;;;


(in-package "USER")


;;; First things first, load xref
(load "xref")

(defun set-global-values ()
  (declare (special weyl-files)
	   (special old-weyl-files)
	   (special method-ids)
	   (special method-names))
  (setf weyl-files '("weyl/algebraic-domains.lisp"
		     "weyl/matrix.lisp" 
		     "weyl/algebraic-extension.lisp"
		     "weyl/morphisms.lisp" 
		     "weyl/avl.lisp"
		     "weyl/mpolynomial.lisp" 
		     "weyl/bigfloat.lisp"
		     "weyl/multipole.lisp" 
		     "weyl/differential-domains.lisp"
		     "weyl/numbers.lisp" 
		     "weyl/direct-sums.lisp"
		     "weyl/packages.lisp" 
		     "weyl/domain-support.lisp"
		     "weyl/poly-tools.lisp" 
		     "weyl/epolynomial.lisp"
		     "weyl/projective-space.lisp" 
		     "weyl/fourier.lisp"
		     "weyl/quaternions.lisp" 
		     "weyl/funct-spaces.lisp"
		     "weyl/quotient-fields.lisp" 
		     "weyl/general-classes.lisp"
		     "weyl/rational-functions.lisp"
		     "weyl/general.lisp"
		     "weyl/sets.lisp"
		     "weyl/gfp.lisp"
		     "weyl/sparsegcd.lisp"
		     "weyl/grobner.lisp" 
		     "weyl/lisp-numbers.lisp" 
		     "weyl/topology.lisp"
		     "weyl/lisp-support.lisp" 
		     "weyl/upolynomial.lisp"
		     "weyl/maintenance.lisp" 
		     "weyl/vector.lisp"
		     "weyl/new-domains.lisp"))
  
  (setf old-weyl-files '("weyl/algebraic-domains.lisp"
			 "old-weyl/matrix.lisp" 
			 "old-weyl/algebraic-extension.lisp"
			 "old-weyl/morphisms.lisp" 
			 "old-weyl/avl.lisp"
			 "old-weyl/mpolynomial.lisp" 
			 "old-weyl/bigfloat.lisp"
			 "old-weyl/multipole.lisp" 
			 "old-weyl/differential-domains.lisp"
			 "old-weyl/numbers.lisp" 
			 "old-weyl/direct-sums.lisp"
			 "old-weyl/packages.lisp" 
			 "old-weyl/domain-support.lisp"
			 "old-weyl/poly-tools.lisp" 
			 "old-weyl/epolynomial.lisp"
			 "old-weyl/projective-space.lisp" 
			 "old-weyl/fourier.lisp"
			 "old-weyl/quaternions.lisp" 
			 "old-weyl/funct-spaces.lisp"
			 "old-weyl/quotient-fields.lisp" 
			 "old-weyl/general-classes.lisp"
			 "old-weyl/rational-functions.lisp"
			 "old-weyl/general.lisp"
			 "old-weyl/sets.lisp"
			 "old-weyl/gfp.lisp"
			 "old-weyl/sparsegcd.lisp"
			 "old-weyl/grobner.lisp" 
			 "old-weyl/lisp-numbers.lisp" 
			 "old-weyl/topology.lisp"
			 "old-weyl/lisp-support.lisp" 
			 "old-weyl/upolynomial.lisp"
			 "old-weyl/maintenance.lisp" 
			 "old-weyl/vector.lisp"
			 ))
  (setf method-ids (xref:list-method-ids))
  (setf method-names (xref:list-method-names)))
(set-global-values)

;;; ****
;;; Have xref parase through the entire weyl package
;;; ****
;;(xref:xref-files weyl-files)

;;; ****
;;; Create some front-ends for xref to make usage smoother
;;; ****
(defun list-method-names ()
  (xref:list-method-names))
(defun list-old-method-names ()
  (xref:list-old-method-names))
(defun list-variable-names ()
  (xref:list-variable-names))
(defun list-old-variable-names ()
  (xref:list-old-variable-names))
(defun list-function-names ()
  (xref:list-function-names))
(defun list-old-function-names ()
  (xref:list-old-function-names))			  
(defun list-method-ids ()
  (xref:list-method-ids))
;; TO-DO fix this so it works.
;;(defun list-old-method-ids ()
;;  (xref:list-old-method-ids))
(defun Methods (symbol)
  (xref:list-methods symbol))
(defun OldMethods (symbol)
  (xref:list-old-methods symbol))
(defun CallerTrees ()
  (xref:print-caller-trees))
(defun CalleeTrees ()
  (xref:print-callee-trees))
(defun Save (filename)
  (xref:write-callers-database-to-file filename))
(defun SaveToOld (filename)
  (xref:write-callers-database-to-old-file filename))
(defun CompMethodNames ()
  (xref:compare-method-names))
(defun CompVarNames ()
  (xref:compare-variable-names))
(defun CompFunctNames ()
  (xref:compare-function-names))
(defun CompMethods ()
  (xref:compare-all-method-lists))
(defun CompCallers ()
  (xref:compare-all-callers-lists))
(defun CompReaders ()
  (xref:compare-all-reader-lists))
(defun CompSetters ()
  (xref:compare-all-setter-lists))
(defun CompFileCallers ()
  (xref:compare-all-file-callers-lists))
(defun XFile (filename)
  (xref:xref-file filename))
(defun XFiles (file-list)
  (xref:xref-files file-list))
(defun X2Files (old-file new-file)
  (xref:xref-two-files old-file new-file))
(defun X2SrcSets (old-set new-set)
  (xref:xref-two-source-sets old-set new-set))
(defun MoveToOld ()
  (xref:move-to-old))



;;; **********************************
;;;  Internal utilities **************
;;; **********************************



(defun switch-last-two-elements (list)
  "Switches the order of the second and the third elements in a list"
  "if and only if the list lenght equals 3.  USED by consistency"
  "checking utility"
  (let ((tmp-list nil)
	(tmp nil))
       (when (eq 3 (length list))
	 (setf tmp (first list))
	 (setf tmp-list (rest list))
	 (setf tmp-list (reverse tmp-list))
	 (push tmp tmp-list))
       tmp-list))


(defun remove-redundant-elements (list)
  (let ((tmp-list nil))
       (loop for element in list
	     do (unless (member element tmp-list)
		  (push element tmp-list)))
       tmp-list))

		  
(defun list-method-names-with (number-of-args &optional (verbose? nil))
  (let ((field (list-method-ids-with number-of-args verbose?))
	(tmp-list nil))
       (loop for element in field
	     do (push (car element) tmp-list)
		(when verbose?
		  (format "~& Adding ~S" element))
		(setf tmp-list (remove-redundant-elements tmp-list)))
       tmp-list))
					      


(defun list-method-ids-with (number-of-args &optional (verbose? nil))
  (let ((tmp-list nil))
       (loop for element in (xref:list-method-ids)
	     for check-len = (- (length element) 1)
	     do (when verbose?
		  (format t "~& Checking ~A against ~A" number-of-args check-len))
		(when (= number-of-args check-len)
		  (push element tmp-list)))
       tmp-list))




(defun inconsistent-args (method &optional (verbose? nil))
  (let ((field (xref:list-methods method))
	(illegal nil))
       (loop for m-id in (xref:list-metHods Method)
	     for key = (switch-last-two-elements m-id)
	     do (unless (or (null key) (exists-in key field))
		  (when verbose?
		    (format t "Missing method ~S~%" key))
		  (push key illegal)))
       illegal))



(defun exists-method (method-id)
  "EXISTS-METHOD checks if METHOD-ID exists in the global database of methods"
  (let ((retval nil))
       (when (eq 3 (length method-id))
	 (loop for element in (xref:list-methods (first method-id))
	       do (when (eql element method-id)
		    (setf retval T))))
       retval))

(defun exists-in (method-id method-list)
  "replaces MEMBER primitve when searching for lists within lists"
  (when (eq 3 (length method-id))
    (dolist (element method-list nil)
      (when (equal method-id element)
	(return T)))))


