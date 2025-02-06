;;; -*- Mode:Lisp; Package:User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Lisp Support
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; lisp-support.lisp,v 1.8 1994/10/21 18:16:43 rz Exp

(in-package #-ANSI-CL "USER" #+ANSI-CL "CL-USER")

(make::adjust-version-numbers Weyl "1.8")

;; The following is done instead of importing defgeneric and
;; defmethod, to avoid muddying the user package.   
#+PCL
(progn
  (defmacro clos-defgeneric (&rest args) `(pcl:defgeneric . ,args))
  (defmacro clos-defmethod (&rest args) `(pcl:defmethod . ,args)))

#+(and CLOS (not MCL))
(progn
  (defmacro clos-defgeneric (&rest args) `(clos::defgeneric . ,args))
  (defmacro clos-defmethod (&rest args) `(clos::defmethod . ,args)))

#+MCL
(progn
  (defmacro clos-defgeneric (&rest args) `(cl:defgeneric . ,args))
  (defmacro clos-defmethod (&rest args) `(cl:defmethod . ,args)))

;; Extend defmethod slightly

#+PCL
(defmacro weyli::defmethod (&rest args &environment env)
  (declare (pcl:arglist name
			{method-qualifier}*
			specialized-lambda-list
			&body body))
  (labels ((duplicate-arglist (arglist)
	     (cond ((null arglist) (list nil))
		   ((or (atom (first arglist))
			(null (rest (first arglist)))
			(atom (second (first arglist)))
			(not (eql 'or (first (second (first arglist))))))
		    (mapcar (lambda (q) (cons (first arglist) q))
			    (duplicate-arglist (rest arglist))))
		   (t (loop for type in (rest (second (first arglist)))
			    with rest = (duplicate-arglist (rest arglist))
			    nconc (mapcar #'(lambda (q)
					      (cons (list (first (first arglist)) type)
						  q))
					  rest))))))
    (multiple-value-bind (name qualifiers lambda-list body)
	(pcl:parse-defmethod args)
      (let ((proto-method (pcl:method-prototype-for-gf name)))
	`(progn
	   ,@(loop for ll in (duplicate-arglist lambda-list)
		   collect
		     (pcl:expand-defmethod proto-method name qualifiers ll body env)))))))

#+CLOS
(defmacro weyli::defmethod (&rest args)
  #+Genera
  (declare (arglist name {method-qualifier}* specialized-lambda-list
		    &body body)
	   (zwei:indentation . wei:indent-for-clos-defmethod))
  (labels ((duplicate-arglist (arglist)
	     (cond ((null arglist) (list nil))
		   ((or (atom (first arglist))
			(null (rest (first arglist)))
			(atom (second (first arglist)))
			(not (eql 'or (first (second (first arglist))))))
		    (mapcar #'(lambda (q) (cons (first arglist) q))
			    (duplicate-arglist (rest arglist))))
		   (t (loop for type in (rest (second (first arglist)))
			    with rest = (duplicate-arglist (rest arglist))
			    nconc (mapcar #'(lambda (q)
					      (cons (list (first (first arglist)) type)
						    q))
					  rest))))))
    #-LispWorks
    (multiple-value-bind (name qualifiers lambda-list body)
          #+(or excl Lucid) (clos::parse-defmethod args)
	  #+(or MCL Genera) (clos-parse-defmethod args)
      `(progn
	,@(loop for ll in (duplicate-arglist lambda-list)
		collect
                #-MCL
		 `(clos::defmethod ,name ,@qualifiers ,ll ,@body)
                 #+MCL
                  `(,(if (or qualifiers ll) 'cl:defmethod 'defun) ,name ,@qualifiers
                    ,ll ,@body))))
    #+LispWorks
    (let ((name (first args)))
      (multiple-value-bind (qualifiers lambda-list body)
            (clos::parse-defmethod nil name (rest args))
        `(progn
	  ,@(loop for ll in (duplicate-arglist lambda-list)
		  collect
  		   `(clos:defmethod ,name ,@qualifiers ,ll ,@body)))))))

(defmacro clos-class-direct-superclasses (c)
  `(#-MCL clos::class-direct-superclasses 
    #+MCL class-direct-superclasses       ,c))

;; The following predicate determines if class is a subclass of super-class.
(defun weyli::subclass-of? (class super-class)
  (when (symbolp class)
    (setq class (find-class class)))
  (when (symbolp super-class)
    (setq super-class (find-class super-class)))
  (labels ((search-list (list)
	     (or (member class list)
		 (loop for c in list
		       when (search-list (clos-class-direct-superclasses c))
			 return t))))
    (or (eql class super-class)
	(search-list (clos-class-direct-superclasses super-class)))))

#+(or MCL (and Genera CLOS))
(defun clos-parse-defmethod (form)
  (let ((name (pop form))
	qualifiers)
    (loop while (and (atom (first form))
		     (not (null (first form))))
	  do (push (pop form) qualifiers))
    (values name (reverse qualifiers) (first form) (rest form))))

(defmacro weyli::%funcall (function &rest args)
  `(cl:funcall ,function ,@args))

(clos-defmethod weyli::funcall (function &rest args)
  (weyli::apply function args))

(defmacro weyli::%apply (function &rest args)
  `(cl:apply ,function ,@args))

(defun weyli::accum-apply-args (args)
  (cond ((null (rest args))
         (first args))
        (t (cons (first args) (weyli::accum-apply-args (rest args))))))

(clos-defmethod weyli::apply (function &rest args)
  (cond ((null args)
         (error "The function APPLY was called with too few arguments"))
        (t (cl:apply function (weyli::accum-apply-args args)))))

(defmacro weyli::%getf (place indicator &optional (default nil))
  (if default
      `(cl:getf ,place ,indicator ,default)
      `(cl:getf ,place ,indicator)))

(clos-defgeneric weyli::getf (place indicator &optional default)
		 )

(clos-defmethod weyli::getf (place indicator &optional (default nil))
  (cl:getf place indicator default))

(clos-defmethod weyli::putf (place indicator value)
  (setf (cl:getf place indicator) value))

(defsetf weyli::getf weyli::putf)

(clos-defgeneric weyli::delete (item set &key &allow-other-keys)
  )

(clos-defmethod weyli::delete (item (sequence sequence) &rest args)
  (apply #'cl:delete item sequence args))

(clos-defgeneric weyli::member (item list &key &allow-other-keys)
  )

(clos-defmethod weyli::member (item (list list) &rest args)
  (apply #'cl:member item list args))

(clos-defgeneric weyli::replace (item list &key &allow-other-keys)
  )

(clos-defmethod weyli::replace ((item sequence) (list sequence) &rest args)
  (apply #'cl:replace item list args))

(clos-defgeneric weyli::substitute
    (newitem olditem sequence &key &allow-other-keys)
  )

(clos-defmethod weyli::substitute (newitem olditem (seq sequence) &rest args)
  (apply #'cl:substitute newitem olditem seq args))

(clos-defgeneric weyli::map (result-type function sequence &rest sequences)
  )

(clos-defmethod weyli::map (result-type function sequence &rest sequences)
  (apply #'cl:map result-type function sequence sequences))

(clos-defgeneric weyli::reduce (function sequence &rest options)
  )

(clos-defmethod weyli::reduce (function (sequence sequence) &rest options)
  (apply #'cl:reduce function sequence options))

(clos-defmethod weyli::union ((arg1 list) (arg2 list) &rest rest)
  (apply #'cl:union arg1 arg2 rest))

(clos-defmethod weyli::intersection ((arg1 list) (arg2 list) &rest rest)
  (apply #'cl:intersection arg1 arg2 rest))


#+Genera
(eval-when (compile load eval)
  ;; Link the value cells of algebra:* and zl:*, etc.
  (unless (eq (locf (symbol-value 'weyli::*))
	      (locf (symbol-value 'zl:*)))
    (setq weyli::* zl:*)
    (si:link-symbol-value-cells 'weyli::* 'zl:*))
  (unless (eq (locf (symbol-value 'weyli::+))
	      (locf (symbol-value 'zl:+)))
    (setq weyli::+ zl:+)
    (si:link-symbol-value-cells 'weyli::+ 'zl:+))
  )

#+Lucid
(setf (symbol-function 'lucid-old-top-level-eval) #'lucid::top-level-eval)

#+Lucid
(defun  lucid::top-level-eval (&rest arguments)
  (declare (special weyli::* weyli::+ cl:* cl:+))
  (multiple-value-prog1 (apply #'lucid-old-top-level-eval arguments)
    (setq weyli::* cl:*)
    (setq weyli::+ cl:+)))

(defmacro weyli::defsubst (function lambda-list &body body)
  `(#+Genera scl:defsubst
    #+Lucid  lcl:defsubst
    #-(or Genera Lucid) defun
    ,function ,lambda-list ,@body))

;;Infinities...

(defvar weyli::*positive-infinity*
	#+Genera si:infinite-positive-double-float
	#+Lucid system:float-positive-infinity
        #-(or Lucid Genera) (expt 2.0 1000))

(defvar weyli::*negative-infinity*
	#+Genera si:infinite-negative-double-float
	#+Lucid system:float-negative-infinity
        #-(or Genera Lucid) (- (expt 2.0 1000)))

(defmacro weyli::copy-array-contents (from-array to-array)
  #+Genera
  `(scl:copy-array-contents ,from-array ,to-array)
  #-Genera
  `(copy-array-contents* ,from-array ,to-array))

#+(not (or Genera Lucid MCL Lispworks Allegro))
(error "Need to define COPY-ARRAY-CONTENTS*")

#+Lucid
(defmacro general-set-aref (value array indices)
  `(apply #'lucid-runtime-support:set-aref ,value ,array ,indices))

#+MCL
(defmacro general-set-aref (value array indices)
  `(apply #'ccl::aset ,value ,array ,indices))

#+Lispworks
(defmacro general-set-aref (value array indices)
  `(apply #'system::set-aref ,array ,indices ,value))

#+Allegro
(defmacro general-set-aref (value array indices)
  `(apply #'excl::.inv-s-aref ,value ,array ,indices))

(defun copy-array-contents* (from-array to-array)
  (let ((from-dims (array-dimensions from-array))
	(to-dims (array-dimensions to-array)))
    (unless (eql (length from-dims) (length to-dims))
      (error "Incompatable array dimensions: ~A -> ~A"
	     from-array to-array))
    (labels ((worker (from-dims to-dims indices)
	       (cond ((null from-dims)
		      (general-set-aref (apply #'aref from-array indices)
					to-array indices))
		     (t (loop for i below (min (first from-dims)
					       (first to-dims))
			      do (worker (rest from-dims) (rest to-dims)
					 (cons i indices)))))))
      (worker (reverse from-dims) (reverse to-dims) nil))))

(defun weyli::circular-list (&rest arguments)
  #+Genera (apply #'scl:circular-list arguments)
  #-Genera (nconc arguments arguments))

(weyli::defsubst structure-of (x)
  (cl:type-of x))

;; The following macros deal with certain functions that should take an
;; arbitrary number of arguments.

(defun associate-predicate (predicate values)
  (let ((forms 
	 (loop for (x y) on values
	       when y
		 collect `(,predicate ,x ,y))))
    (if (null (rest forms)) (first forms)
	(cons 'and forms))))

(defmacro weyli::< (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to <"))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary< values))))

(defmacro weyli::= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to ="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary= values))))

(defmacro weyli::> (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to >"))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary> values))))

(defmacro weyli::<= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to <="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary<= values))))

(defmacro weyli::>= (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to >="))
	((null (rest values)) t)
	(t (associate-predicate 'weyli::binary>= values))))

(defun associate-operation (operation values)
  (labels ((iterate (values result)
	     (cond ((null values)
		    result)
		   (t (iterate (rest values)
			       `(,operation ,result ,(first values)))))))
    (iterate (rest values) (first values))))

(defmacro weyli::max (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to max"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::max-pair values))))

(defun weyli::%max (&rest values)
  (if (null values)
      (error "Illegal number of arguments to max")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::max-pair (first vals) (next-loop (rest vals))))))
	 (next-loop values))))  

(defmacro weyli::min (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to min"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::min-pair values))))

(defun weyli::%min (&rest values)
  (if (null values)
      (error "Illegal number of arguments to min")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::min-pair (first vals) (next-loop (rest vals))))))
	 (next-loop values))))

(defmacro weyli::+ (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to +"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::plus values))))

(defun weyli::%plus (&rest values)
  (if (null values)
      (error "Illegal number of arguments to +")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::plus (first vals) (next-loop (rest vals))))))
	 (next-loop values))))

(defmacro weyli::- (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to -"))
	((null (rest values))
	 `(weyli::minus ,(first values)))
	(t (associate-operation 'weyli::difference values))))

(defun weyli::%difference (&rest values)
  (if (null values)
      (error "Illegal number of arguments to -")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::plus (first vals) (next-loop (rest vals))))))
	 (if (null (rest values))
	     (weyli::minus (first values))
	     (next-loop values)))))

(defmacro weyli::* (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to *"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::times values))))

(defun weyli::%times (&rest values)
  (if (null values)
      (error "Illegal number of arguments to *")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::times (first vals) (next-loop (rest vals))))))
	 (next-loop values))))

(defmacro weyli::/ (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to /"))
	((null (rest values))
	 `(weyli::recip ,(first values)))
	(t (associate-operation 'weyli::quotient values))))

(defun weyli::%quotient (&rest values)
  (if (null values)
      (error "Illegal number of arguments to -")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::quotient (first vals) (next-loop (rest vals))))))
	 (if (null (rest values))
	     (weyli::recip (first values))
	     (next-loop values)))))

(defmacro weyli::gcd (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to GCD"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::binary-gcd values))))

(defun weyli::%gcd (&rest values)
  (if (null values)
      (error "Illegal number of arguments to GCD")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::binary-gcd (first vals)
					(next-loop (rest vals))))))
	 (next-loop values))))

(defmacro weyli::lcm (&rest values)
  (cond ((null values)
	 (error "Illegal number of arguments to LCM"))
	((null (rest values))
	 (first values))
	(t (associate-operation 'weyli::binary-lcm values))))

(defun weyli::%lcm (&rest values)
  (if (null values)
      (error "Illegal number of arguments to LCM")
      (labels ((next-loop (vals)
	         (if (null (rest vals))
		     (first vals)
		     (weyli::binary-lcm (first vals)
					(next-loop (rest vals))))))
	 (next-loop values))))

(defmacro weyli::floor (a &optional b)
  (if b `(weyli::floor2 ,a ,b) `(weyli::floor1 ,a)))

(defmacro weyli::ceiling (a &optional b)
  (if b `(weyli::ceiling2 ,a ,b) `(weyli::ceiling1 ,a)))

(defmacro weyli::round (a &optional b)
  (if b `(weyli::round2 ,a ,b) `(weyli::round1 ,a)))

(defmacro weyli::truncate (a &optional b)
  (if b `(weyli::truncate2 ,a ,b) `(weyli::truncate1 ,a)))

#+PCL
(defvar pcl:*compile-class-hash* (make-hash-table :test #'eq))

#+PCL
(defun pcl:COMPILE-CLASS-METHODS-1 (classes)
  (clrhash pcl:*compile-class-hash*)
  (dolist (class-spec classes)
    (let ((class (cond ((symbolp class-spec) (pcl:find-class class-spec nil))
		       ((pcl:classp class-spec) class-spec))))
      (cond (class
	     (dolist (gf (pcl:class-direct-generic-functions class))
	       (unless (gethash gf pcl:*compile-class-hash*)
		 (setf (gethash gf pcl:*compile-class-hash*) T)
		 (pcl:notice-methods-change-1 gf))))
	    (t (warn "~A is neither a class nor the name of a class" class-spec))))))

#+PCL
(defmacro weyli::compile-class-methods (&rest classes)
  `(pcl:compile-class-methods-1 ',classes))

#-PCL
(defmacro compile-class-methods (&rest classes)
  (declare (ignore classes))
  "Ignored")

#+PCL
(defun weyli::class-uncompiled-methods (class-spec &optional (function #'print))
  (let ((class (cond ((symbolp class-spec) (pcl:find-class class-spec nil))
		     ((pcl:classp class-spec) class-spec))))
    (cond (class
	   (dolist (gf (pcl:class-direct-generic-functions class))
	     (dolist (method (pcl:generic-function-methods gf))
	       (unless (or (compiled-function-p (pcl:method-function method))
			   #+Genera
			   (typep (pcl:method-function method) 'sys:lexical-closure))
		 (funcall function method)))))
	  (t (warn "~A is neither a class nor the name of a class" class-spec)))))

#+PCL
(defun weyli::all-weyl-classes (&optional (function #'print))
  (let (list)
    (labels ((find-sub-classes (class)
	       (loop for class in (pcl:class-direct-subclasses class)
		     do (unless (member class list)
			  (push class list)
			  (funcall function class)
			  (find-sub-classes class)))))
      (find-sub-classes (pcl:find-class 'weyli::domain))
      (find-sub-classes (pcl:find-class 'weyli::domain-element))
      (find-sub-classes (pcl:find-class 'weyli::morphism)))))

#+PCL
(defun weyli::all-uncompiled-weyl-methods (&optional (function #'print))
  (let (list generic)
    (weyli::all-weyl-classes
      #'(lambda (class)
	  (weyli::class-uncompiled-methods class
	    #'(lambda (method)
	        (setq generic (pcl:method-generic-function method))
		(unless (member generic list)
		  (push generic list)
		  (funcall function generic))))))))


