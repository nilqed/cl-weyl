(in-package :cl-user)

(defpackage #:closer-mop
  (:use #:common-lisp #+lispworks #:lispworks)
  (:nicknames #:c2mop)

  #+(or allegro clozure lispworks mcl)
  (:shadow #:standard-class)

  #+(or allegro clisp clozure ecl clasp lispworks sbcl)
  (:shadow #:defgeneric #:defmethod #:standard-generic-function)

  #+clozure (:shadow standard-method)

  #+(or cmu mcl) (:shadow #:typep subtypep)

  #+lispworks5.1
  (:import-from #:system #:with-hash-table-locked)
  #+(and lispworks (not (or lispworks4 lispworks5)))
  (:import-from #:hcl #:with-hash-table-locked)

  #-(or clisp scl mezzano sicl)
  (:import-from
   #+abcl      #:ext
   #+allegro   #:excl
   #+clozure   #:ccl
   #+cmu       #:pcl
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-pcl

   #:classp)

  (:import-from
   #+abcl      #:mop
   #+allegro   #:mop
   #+clisp     #:clos
   #+clozure   #:ccl
   #+cmu       #:clos-mop
   #+ecl       #:clos
   #+clasp     #:clos
   #+lispworks #:clos
   #+mcl       #:ccl
   #+sbcl      #:sb-mop
   #+scl       #:clos
   #+mezzano   #:mezzano.clos
   #+sicl      #:sicl-clos

   #:direct-slot-definition
   #:effective-slot-definition
   #-lispworks #:eql-specializer
   #:forward-referenced-class
   #-lispworks #:funcallable-standard-class
   #-lispworks4 #:funcallable-standard-object
   #:metaobject
   #:slot-definition
   #-(or lispworks4 lispworks5 scl) #:specializer
   #:standard-accessor-method
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #-lispworks4.3 #:accessor-method-slot-definition
   #-scl #:add-dependent
   #-scl #:add-direct-method
   #:add-direct-subclass
   #-scl #:class-default-initargs
   #-scl #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #-(or clozure lispworks mcl) #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #-(or lispworks4 lispworks5) #:compute-default-initargs
   #-clozure #:compute-discriminating-function
   #-(or clozure scl) #:compute-effective-method
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function-using-class
   #-lispworks #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #-lispworks #:find-method-combination
   #-(or lispworks scl) #:funcallable-standard-instance-access
   #-allegro #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #-lispworks #:intern-eql-specializer
   #-(or allegro clisp clozure lispworks mcl scl mezzano) #:make-method-lambda
   #-scl #:map-dependents
   #-clozure #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #-lispworks4.3 #:reader-method-class
   #-scl #:remove-dependent
   #-scl #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #-lispworks #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #-lispworks #:standard-instance-access
   #-scl #:update-dependent
   #:validate-superclass
   #-lispworks4.3 #:writer-method-class)

  (:export
   #:built-in-class
   #:class
   #:direct-slot-definition
   #:effective-slot-definition
   #:eql-specializer
   #+lispworks #:eql-specializer*
   #:forward-referenced-class
   #:funcallable-standard-class
   #:funcallable-standard-object
   #:generic-function
   #:metaobject
   #:method
   #:method-combination
   #:slot-definition
   #:specializer
   #:standard-accessor-method
   #:standard-class
   #:standard-generic-function
   #:standard-direct-slot-definition
   #:standard-effective-slot-definition
   #:standard-method
   #:standard-object
   #:standard-reader-method
   #:standard-slot-definition
   #:standard-writer-method

   #:defclass
   #:defgeneric
   #:define-method-combination
   #:defmethod

   #:classp
   #:ensure-finalized
   #:ensure-method
   #:fix-slot-initargs
   #:required-args
   #:subclassp

   #:accessor-method-slot-definition
   #:add-dependent
   #:add-direct-method
   #:add-direct-subclass
   #:class-default-initargs
   #:class-direct-default-initargs
   #:class-direct-slots
   #:class-direct-subclasses
   #:class-direct-superclasses
   #:class-finalized-p
   #:class-precedence-list
   #:class-prototype
   #:class-slots
   #:compute-applicable-methods-using-classes
   #:compute-class-precedence-list
   #:compute-default-initargs
   #:compute-discriminating-function
   #:compute-effective-method
   #:compute-effective-method-function
   #:compute-effective-slot-definition
   #:compute-slots
   #:direct-slot-definition-class
   #:effective-slot-definition-class
   #:ensure-class
   #:ensure-class-using-class
   #:ensure-generic-function
   #:ensure-generic-function-using-class
   #:eql-specializer-object
   #:extract-lambda-list
   #:extract-specializer-names
   #:finalize-inheritance
   #:find-method-combination
   #:funcallable-standard-instance-access
   #:generic-function-argument-precedence-order
   #:generic-function-declarations
   #:generic-function-lambda-list
   #:generic-function-method-class
   #:generic-function-method-combination
   #:generic-function-methods
   #:generic-function-name
   #:intern-eql-specializer
   #+lispworks #:intern-eql-specializer*
   #:make-method-lambda
   #:map-dependents
   #:method-function
   #:method-generic-function
   #:method-lambda-list
   #:method-specializers
   #:reader-method-class
   #:remove-dependent
   #:remove-direct-method
   #:remove-direct-subclass
   #:set-funcallable-instance-function
   #:slot-boundp-using-class
   #:slot-definition-allocation
   #:slot-definition-initargs
   #:slot-definition-initform
   #:slot-definition-initfunction
   #:slot-definition-location
   #:slot-definition-name
   #:slot-definition-readers
   #:slot-definition-writers
   #:slot-definition-type
   #:slot-makunbound-using-class
   #:slot-value-using-class
   #:specializer-direct-generic-functions
   #:specializer-direct-methods
   #:standard-instance-access
   #:subtypep
   #:typep
   #:update-dependent
   #:validate-superclass
   #:writer-method-class

   #:warn-on-defmethod-without-generic-function))

(in-package :closer-mop)

(macrolet ((define-closer-common-lisp-package ()
             (loop with symbols = (nunion (loop for sym being the external-symbols of :common-lisp
                                                if (find-symbol (symbol-name sym) :c2mop)
                                                collect it
                                                else collect sym)
                                          (loop for sym being the external-symbols of :c2mop
                                                collect sym))
                   with map = '()
                   for symbol in symbols do
                   (push (symbol-name symbol)
                         (getf map (symbol-package symbol)))
                   finally (return
                            `(defpackage #:closer-common-lisp
                               (:nicknames #:c2cl)
                               (:use)
                               ,@(loop for (package symbols) on map by #'cddr
                                       collect `(:import-from ,(package-name package) ,@symbols))
                               (:export ,@(mapcar #'symbol-name symbols)))))))
  (define-closer-common-lisp-package))

(defpackage #:closer-common-lisp-user
  (:nicknames #:c2cl-user)
  (:use #:closer-common-lisp))
(in-package :closer-mop)

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (typep class 'class)
    (unless (class-finalized-p class)
      (finalize-inheritance class))
    (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))

      (loop with class = (get-class class)
            with superclass = (get-class superclass)

            for superclasses = (list class)
            then (set-difference
                  (union (class-direct-superclasses current-class) superclasses)
                  seen)

            for current-class = (first superclasses)

            while current-class

            if (eq current-class superclass) return t
            else collect current-class into seen

            finally (return nil))))

#+(or allegro clozure lispworks mcl)
(progn
  ;; validate-superclass for metaclass classes is a little bit
  ;; more tricky than for class metaobject classes because
  ;; we don't want to make all standard-classes compatible to
  ;; each other.

  ;; Our validate-superclass may get passed a class-prototype
  ;; as its second argument, so don't expect its readers to
  ;; yield useful information. (In ANSI parlance, "the
  ;; consequences are undefined...")

  (defmacro define-validate-superclass-method (class superclass)
    `(cl:defmethod validate-superclass ((class ,class) (superclass ,superclass))
       (or (when (eq (class-of class) (find-class ',class))
             (or (eq (class-of superclass) (find-class ',superclass))
                 (eq (class-of superclass) (find-class ',class))))
           (call-next-method)
           (when (eq (class-of superclass) (find-class ',superclass))
             (validate-superclass class (class-prototype (find-class ',class))))))))

#+(or clisp scl mezzano)
(progn
  (declaim (inline classp))

  (define-compiler-macro classp (thing)
    `(typep ,thing 'class))

  (defun classp (thing)
    (typep thing 'class)))

#+(or allegro clisp clozure ecl clasp lispworks sbcl)
(progn ;;; New generic functions.

  (defclass standard-generic-function (cl:standard-generic-function)
    (#+(or clozure lispworks) (argument-order :accessor argument-order)
     #-(or abcl sbcl)         (initial-methods :initform '()))

    (:metaclass
     #-lispworks funcallable-standard-class
     #+lispworks clos:funcallable-standard-class)

    #+clozure
    (:default-initargs :name (copy-symbol :name) :method-class (find-class 'standard-method)))

  #+clozure
  (progn
    (cl:defgeneric method-function (method)
      (:method ((method method))
       (ccl:method-function method)))

    (defclass standard-method (cl:standard-method)
      ((fn :initarg :real-function :reader method-function))))

  #-(or ecl clasp)
  (progn
    (declaim (inline m-function))

    (defun m-function (m)
      (method-function m))

    (define-compiler-macro m-function (m)
      (handler-case (method-function m)
        (error () `(the function (method-function (the method ,m)))))))

  (defun compute-argument-order (gf nof-required-args)
    (loop with specialized-count = (make-array nof-required-args :initial-element 0)

          for method in (generic-function-methods gf) do
          (loop for specializer in (method-specializers method)
                for index from 0
                unless (eq specializer (find-class 't))
                do (incf (svref specialized-count index)))

          finally

          (loop for arg in (generic-function-argument-precedence-order gf)
                for pos = (position arg (generic-function-lambda-list gf))
                when (> (svref specialized-count pos) 0)
                collect pos into argument-order
                finally (return-from compute-argument-order
                          (coerce argument-order 'simple-vector)))))

  (defun parse-method-body (body error-form)
    (loop with documentation = nil
          for (car . cdr) = body then cdr
          while (or (and cdr (stringp car))
                    (and (consp car) (eq (car car) 'declare)))
          if (stringp car)
          do (setq documentation
                   (if (null documentation) car
                     (warn "Too many documentation strings in ~S." error-form)))
          else append (cdr car) into declarations
          finally (return (values documentation declarations (cons car cdr)))))

  (defun block-name (function-name)
    (cond ((symbolp function-name)
           function-name)
          ((and (consp function-name)
                (eql (first function-name) 'setf)
                (consp (cdr function-name))
                (null (cddr function-name)))
           (second function-name))
          (t (error "~S is not a valid function name." function-name))))

  #-(or abcl sbcl) (cl:defgeneric make-method-lambda (generic-function method lambda-expression environment))

  #-(or ecl clasp)
  (cl:defmethod make-method-lambda ((gf standard-generic-function) (method standard-method)
                                    lambda-expression environment)
    (declare (ignore environment) (optimize (speed 3) (space 0) (compilation-speed 0)))
    #+(or abcl clozure lispworks sbcl)
    (when (only-standard-methods gf)
      (return-from make-method-lambda (call-next-method)))
    (let ((args (copy-symbol 'args)) (next-methods (copy-symbol 'next-methods))
          (more-args (copy-symbol 'more-args)) (method-function (copy-symbol 'method-function)))
      (destructuring-bind
          (lambda (&rest lambda-args) &body body)
          lambda-expression
        (assert (eq lambda 'lambda))
        (multiple-value-bind (documentation declarations main-body)
            (parse-method-body body lambda-expression)
          (values
           `(lambda (,args ,next-methods &rest ,more-args)
              (declare (ignorable ,args ,next-methods ,more-args))
              (flet ((call-next-method (&rest args)
                       (if ,next-methods
                           (apply (method-function (first ,next-methods))
                                  (if args args ,args) (rest ,next-methods) ,more-args)
                           (apply #'no-next-method
                                  (getf ,more-args :generic-function)
                                  (getf ,more-args :method)
                                  (if args args ,args))))
                     (next-method-p () (not (null ,next-methods))))
                (declare (inline call-next-method next-method-p)
                         (ignorable #'call-next-method #'next-method-p))
                (flet ((,method-function ,lambda-args
                         (declare ,@declarations)
                         (declare (ignorable ,@(loop for arg in lambda-args
                                                     until (member arg lambda-list-keywords)
                                                     collect arg)))
                         (block ,(block-name (generic-function-name gf))
                           ,@main-body)))
                  (declare (inline ,method-function))
                  (apply #',method-function ,args))))
           (nconc
            (when documentation
              (list :documentation documentation))
            #+clozure '(:closer-patch t)
            #-clozure '()))))))

  #+(or clozure lispworks)
  (cl:defgeneric compute-applicable-methods-using-classes (generic-function classes)
    (:method ((gf standard-generic-function) classes)
     (labels ((subclass* (spec1 spec2 arg-spec)
                (let ((cpl (class-precedence-list arg-spec)))
                  (declare (type list cpl))
                  (find spec2 (the list (cdr (member spec1 cpl :test #'eq))) :test #'eq)))
              (method-more-specific-p (m1 m2)
                (declare (type method m1 m2))
                (loop for n of-type fixnum across (argument-order gf)
                      for spec1 = (nth n (method-specializers m1))
                      for spec2 = (nth n (method-specializers m2))
                      unless (eq spec1 spec2)
                      return (subclass* spec1 spec2 (nth n classes)))))
       (let ((applicable-methods
              (sort
               (loop for method of-type method in (the list (generic-function-methods gf))
                     when (loop for class in classes
                                for specializer in (the list (method-specializers method))
                                if (typep specializer 'eql-specializer)
                                do (when (typep (eql-specializer-object specializer) class)
                                     (return-from compute-applicable-methods-using-classes (values '() nil)))
                                else if (not (subclassp class specializer)) return nil
                                finally (return t))
                     collect method)
               #'method-more-specific-p)))
         (values applicable-methods t)))))

  (cl:defgeneric compute-effective-method-function (gf effective-method options))

  #-(or ecl clasp)
  (cl:defmethod compute-effective-method-function ((gf generic-function) effective-method options)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (when #-clisp options
      #+clisp (or (cdr (assoc :arguments options))
                  (cdr (assoc :duplicates options)))
      (cerror "Ignore these options."
              "This version of compute-effective-method-function does not support method combination options: ~S"
              options))
    (let ((all-t-specializers (required-args (generic-function-lambda-list gf)
                                             (constantly (find-class 't))))
          (args (copy-symbol 'args)))
      (labels ((transform-effective-method (form)
                 (if (atom form) form
                   (case (car form)
                     (call-method (transform-effective-method
                                   (let ((the-method (transform-effective-method (cadr form)))
                                         (method-var (copy-symbol 'method-var)))
                                     `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
                                        (let ((,method-var ,the-method))
                                          (declare (ignorable ,method-var))
                                          (funcall (m-function ,(if (typep the-method 'method)
                                                                  the-method method-var))
                                                   ,args
                                                   ,@(let ((subforms
                                                            (loop for subform in (the list (cddr form))
                                                                  collect `',subform)))
                                                       (if subforms subforms '(())))
                                                   :generic-function ,gf
                                                   :method ,(if (typep the-method 'method)
                                                              the-method method-var)))))))
                     (make-method (when (cddr form)
                                    (error "Incorrect make-method form: ~S." form))
                                  (let ((method-class (generic-function-method-class gf)))
                                    #+allegro (ensure-finalized method-class)
                                    (multiple-value-bind
                                        (method-lambda method-options)
                                        (make-method-lambda
                                         gf (class-prototype method-class)
                                         `(lambda (&rest ,args)
                                            (declare (ignorable ,args))
                                            ,(transform-effective-method (cadr form))) nil)
                                      (apply #'make-instance
                                             method-class
                                             :qualifiers '()
                                             :specializers all-t-specializers
                                             :lambda-list (generic-function-lambda-list gf)
                                             :function (compile nil method-lambda)
                                             method-options))))
                     (t (mapcar #'transform-effective-method (the list form)))))))
        (let ((emf-lambda `(lambda (&rest ,args)
                             (declare (ignorable ,args))
                             ,(transform-effective-method effective-method))))
          (multiple-value-bind (function warnings failure)
              (compile nil emf-lambda)
            (declare (ignore warnings))
            (assert (not failure))
            function)))))

  #+clozure
  (cl:defgeneric compute-effective-method (generic-function combination methods))

  #+clozure
  (cl:defgeneric compute-discriminating-function (generic-function))

  (defun get-emf (gf args nof-required-args)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (let ((applicable-methods (compute-applicable-methods gf (subseq args 0 nof-required-args))))
      (if applicable-methods
        (multiple-value-bind
            (effective-method options)
            (compute-effective-method
             gf (generic-function-method-combination gf)
             applicable-methods)
          (compute-effective-method-function gf effective-method options))
        (lambda (&rest args)
          (apply #'no-applicable-method gf args)))))

  (defun get-emf-using-classes (gf args classes nof-required-args)
    (declare (type generic-function gf) (type list args classes)
             (optimize (speed 3) (space 0) (compilation-speed 0)))
    (multiple-value-bind
        (applicable-methods validp)
        (compute-applicable-methods-using-classes gf classes)
      (unless validp
        (setq applicable-methods
              (compute-applicable-methods gf (subseq args 0 nof-required-args))))
      (values
       (if applicable-methods
         (multiple-value-bind
             (effective-method options)
             (compute-effective-method
              gf (generic-function-method-combination gf)
              applicable-methods)
           (compute-effective-method-function gf effective-method options))
         (lambda (&rest args)
           (apply #'no-applicable-method gf args)))
       validp)))

  (defvar *standard-gfs*
    (list #'compute-applicable-methods #'compute-applicable-methods-using-classes
          #'compute-effective-method #'compute-effective-method-function
          #'generic-function-method-class
          #'make-method-lambda
          #+allegro #'compute-discriminating-function))

  (defun only-standard-methods (gf &rest other-gfs)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (loop for other-gf in (or other-gfs *standard-gfs*)
          always (loop for method in (generic-function-methods other-gf)
                       for specializer = (first (method-specializers method))
                       if (and (typep specializer 'class)
                               (subclassp specializer (find-class 'standard-generic-function))
                               (not (eq specializer (find-class 'standard-generic-function)))
                               (typep gf specializer))
                       return nil
                       else if (and (typep specializer 'eql-specializer)
                                    (eql (eql-specializer-object specializer) gf))
                       return nil
                       finally (return t))))

  (defun methods-all-the-same-specializers (gf)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (loop with template = (first (generic-function-methods gf))
          for method in (rest (generic-function-methods gf))
          always (loop for spec1 in (method-specializers template)
                       for spec2 in (method-specializers method)
                       always (etypecase spec1
                                (class (etypecase spec2
                                         (class (eq spec1 spec2))
                                         (eql-specializer nil)))
                                (eql-specializer
                                 (etypecase spec2
                                   (class nil)
                                   (eql-specializer
                                    (eql (eql-specializer-object spec1)
                                         (eql-specializer-object spec2)))))))))

  (defun compute-discriminator (gf compute-native-discriminator)
    (declare (optimize (speed 3) (space 0) (compilation-speed 0)))
    (let ((nof-required-args
           (length (required-args
                    (handler-case (generic-function-lambda-list gf)
                      (unbound-slot ()
                        (return-from compute-discriminator
                          (funcall compute-native-discriminator)))))))
          discriminator)
      #+(or clozure lispworks)
      (setf (argument-order gf)
            (compute-argument-order gf nof-required-args))
      (flet ((discriminate (emf-setter args &optional (classes (loop for arg in args
                                                                     repeat nof-required-args
                                                                     collect (class-of arg))))
               (declare (type list args classes) (type function emf-setter))
               (multiple-value-bind (emf validp) (get-emf-using-classes gf args classes nof-required-args)
                 (funcall emf-setter (if validp emf (lambda (&rest args)
                                                      (apply (the function (get-emf gf args nof-required-args)) args))))
                 (apply (the function emf) args))))
        (when (only-standard-methods gf #'compute-applicable-methods #'compute-applicable-methods-using-classes)
          (setq discriminator
                (if (only-standard-methods
                     gf #'compute-effective-method #'compute-effective-method-function #'make-method-lambda
                     #+allegro #'compute-discriminating-function)
                  (funcall compute-native-discriminator)
                  (let ((argument-order
                         #-(or clozure lispworks) (compute-argument-order gf nof-required-args)
                         #+(or clozure lispworks) (argument-order gf)))
                    (cond ((null (generic-function-methods gf))
                           (lambda (&rest args)
                             (apply #'no-applicable-method gf args)))
                          ((methods-all-the-same-specializers gf)
                           (let ((specializers (method-specializers (first (generic-function-methods gf))))
                                 (effective-method-function nil))
                             (declare (type list specializers))
                             (lambda (&rest args)
                               (declare (optimize (speed 3) (safety 0) (debug 0)
                                                  (compilation-speed 0)))
                               (cond ((loop for arg in args
                                            for spec in specializers
                                            always (etypecase spec
                                                     (class (typep arg spec))
                                                     (eql-specializer (eql arg (eql-specializer-object spec)))))
                                      (if effective-method-function
                                        (apply (the function effective-method-function) args)
                                        (discriminate (lambda (emf) (setq effective-method-function emf)) args)))
                                     (t (apply #'no-applicable-method gf args))))))
                          ((= (length argument-order) 1)
                           (let ((dispatch-argument-index (svref argument-order 0))
                                 (emfs (make-hash-table :test #'eq)))
                             (declare (type hash-table emfs) (type fixnum dispatch-argument-index))
                             (lambda (&rest args)
                               (declare (optimize (speed 3) (safety 0) (debug 0)
                                                  (compilation-speed 0)))
                               (let* ((dispatch-class (class-of (nth dispatch-argument-index args)))
                                      (effective-method-function (gethash dispatch-class emfs)))
                                 (if effective-method-function
                                   (apply (the function effective-method-function) args)
                                   (discriminate (lambda (emf) (setf (gethash dispatch-class emfs) emf)) args)))))))))))
        (if discriminator discriminator
          (let ((emfs (make-hash-table :test #'equal)))
            (declare (type hash-table emfs))
            (lambda (&rest args)
              (declare (optimize (speed 3) (safety 0) (debug 0)
                                 (compilation-speed 0)))
              (let* ((classes (loop for arg in args
                                    repeat nof-required-args
                                    collect (class-of arg)))
                     (effective-method-function (gethash (the list classes) emfs)))
                (if effective-method-function
                  (apply (the function effective-method-function) args)
                  (discriminate (lambda (emf) (setf (gethash (the list classes) emfs) emf)) args classes)))))))))

  #-(or clisp clozure lispworks (and sbcl sb-thread))
  (cl:defmethod compute-discriminating-function ((gf standard-generic-function))
    (if (eq (class-of gf) (find-class 'standard-generic-function))
      (lambda (&rest args)
        (let ((discriminator (compute-discriminator gf #'call-next-method)))
          (set-funcallable-instance-function gf discriminator)
          (apply discriminator args)))
      (compute-discriminator gf #'call-next-method)))

  #+(or clisp clozure lispworks (and sbcl sb-thread))
  (cl:defmethod compute-discriminating-function ((gf standard-generic-function))
    (compute-discriminator gf #'call-next-method))

  #-(or abcl sbcl)
  (progn
    (defun maybe-remove-initial-methods (function-name)
      (let ((generic-function (ignore-errors (fdefinition function-name))))
        (when (and generic-function (typep generic-function 'standard-generic-function))
          (dolist (method (slot-value generic-function 'initial-methods))
            (remove-method generic-function method)))))

    #-(or allegro lispworks)
    (defmacro without-redefinition-warnings (&body body)
      `(progn ,@body))

    #+allegro
    (defmacro without-redefinition-warnings (&body body)
      `(excl:without-redefinition-warnings ,@body))

    #+lispworks
    (defmacro without-redefinition-warnings (&body body)
      `(let ((dspec:*redefinition-action* :quiet)) ,@body))

    (defmacro defgeneric (&whole form name (&rest args) &body options &environment env)
      (loop initially (unless (every #'consp options)
                        (error "Illegal options in defgeneric form ~S." form))
            with generic-function-class-name = nil
            for option in options
            if (eq (car option) :generic-function-class) do
            (when (or (cddr option) (null (cadr option)) (not (symbolp (cadr option)))
                      generic-function-class-name)
              (error "Illegal or duplicate :generic-function-class option in defgeneric form ~S." form))
            (setq generic-function-class-name (cadr option))
            end
            if (eq (car option) :method) collect option into method-options
            else collect option into non-method-options
            finally
            (let ((gf (copy-symbol 'gf))
                  (non-standard (when generic-function-class-name
                                  (let ((standard-generic-function (find-class 'standard-generic-function t env))
                                        (this-generic-function (find-class generic-function-class-name t env)))
                                    (and (subclassp this-generic-function standard-generic-function)
                                         (not (eq this-generic-function standard-generic-function)))))))
              (return-from defgeneric
                `(progn (maybe-remove-initial-methods ',name)
                   ,(if non-standard
                      `(eval-when (:compile-toplevel :load-toplevel :execute)
                         (cl:defgeneric ,name ,args ,@non-method-options))
                      `(progn
                         (eval-when (:compile-toplevel)
                           (cl:defgeneric ,name ,args ,@non-method-options))
                         (eval-when (:load-toplevel :execute)
                           (without-redefinition-warnings
                             (cl:defgeneric ,name ,args ,@options)))))
                   (let ((,gf (fdefinition ',name)))
                     ,(when non-standard
                        `(setf (slot-value ,gf 'initial-methods)
                               (list ,@(loop for method-option in method-options
                                             collect `(defmethod ,name ,@(cdr method-option))))))
                     ,gf)))))))

  #+(or abcl sbcl)
  (defmacro defgeneric (&whole form name (&rest args) &body options)
    (unless (every #'consp options)
      (error "Illegal generic function options in defgeneric form ~S." form))
    (let ((options-without-methods (remove :method options :key #'car :test #'eq)))
      `(progn
         (eval-when (:compile-toplevel)
           (cl:defgeneric ,name ,args ,@options-without-methods))
         (eval-when (:load-toplevel :execute)
           (cl:defgeneric ,name ,args ,@options)))))

  #-(or abcl sbcl)
  (progn
    (defun create-gf-lambda-list (method-lambda-list)
      (loop with stop-keywords = '#.(remove '&optional lambda-list-keywords)
            for arg in method-lambda-list
            until (member arg stop-keywords)
            collect arg into gf-lambda-list
            finally (return (let (rest)
                              (cond ((member '&key method-lambda-list)
                                     (nconc gf-lambda-list '(&key)))
                                    ((setq rest (member '&rest method-lambda-list))
                                     (nconc gf-lambda-list (subseq rest 0 2)))
                                    (t gf-lambda-list))))))

    (defun extract-specializers (specialized-args form)
      (loop for specializer-name in (extract-specializer-names specialized-args)
            collect (typecase specializer-name
                      (symbol `(find-class ',specializer-name))
                      (specializer specializer-name)
                      (cons (cond
                             ((> (length specializer-name) 2)
                              (error "Invalid specializer ~S in defmethod form ~S."
                                     specializer-name form))
                             ((eq (car specializer-name) 'eql)
                              `(intern-eql-specializer ,(cadr specializer-name)))
                             (t (error "Invalid specializer ~S in defmethod form ~S."
                                       specializer-name form))))
                      (t (error "Invalid specializer ~S in defmethod form ~S."
                                specializer-name form)))))

    (defun load-method (name gf-lambda-list type qualifiers specializers lambda-list function options)
      (let* ((gf (if (fboundp name) (fdefinition name)
                   (ensure-generic-function name :lambda-list gf-lambda-list :generic-function-class type)))
             (method (apply #'make-instance
                            (generic-function-method-class gf)
                            :qualifiers qualifiers
                            :specializers specializers
                            :lambda-list lambda-list
                            :function function
                            options)))
        (add-method gf method)
        method)))

  (define-condition defmethod-without-generic-function (style-warning)
    ((name :initarg :name :reader dwg-name))
    (:report (lambda (c s) (format s "No generic function present when encountering a defmethod for ~S. Assuming it will be an instance of standard-generic-function." (dwg-name c)))))

  (define-symbol-macro warn-on-defmethod-without-generic-function
    #-(or abcl sbcl) t
    #+(or abcl sbcl) nil)

  #-(or abcl sbcl)
  (defmacro defmethod (&whole form name &body body &environment env)
    (loop with generic-function = (when (fboundp name) (fdefinition name))

          initially
          (when (macroexpand 'warn-on-defmethod-without-generic-function env)
            (unless generic-function
              (warn 'defmethod-without-generic-function :name name)))
          (unless (typep generic-function 'standard-generic-function)
            (return-from defmethod `(cl:defmethod ,@(cdr form))))
          (when (only-standard-methods generic-function)
            (return-from defmethod `(cl:defmethod ,@(cdr form))))

          for tail = body then (cdr tail)
          until (listp (car tail))
          collect (car tail) into qualifiers
          finally
          (destructuring-bind
              ((&rest specialized-args) &body body) tail
            (let* ((lambda-list (extract-lambda-list specialized-args))
                   (gf-lambda-list (create-gf-lambda-list lambda-list))
                   (specializers (extract-specializers specialized-args form))
                   (method-class (generic-function-method-class generic-function)))
              #+allegro (ensure-finalized method-class)
              (multiple-value-bind
                    (method-lambda method-options)
                  (make-method-lambda
                   generic-function
                   (class-prototype method-class)
                   `(lambda ,lambda-list ,@body)
                   env)
                (return-from defmethod
                  `(load-method ',name ',gf-lambda-list ',(type-of generic-function)
                                ',qualifiers (list ,@specializers) ',lambda-list
                                (function ,method-lambda) ',method-options)))))))

  #+(or abcl sbcl)
  (defmacro defmethod (&whole form name &body body &environment env)
    (declare (ignore body))
    (let ((generic-function (when (fboundp name) (fdefinition name))))
      (when (macroexpand 'warn-on-defmethod-without-generic-function env)
        (unless generic-function
          (warn 'defmethod-without-generic-function :name name)))
      `(cl:defmethod ,@(cdr form))))
)

#+(or allegro clisp cmu mcl scl)
(defun ensure-method (gf lambda-expression
                         &key (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))

  (let ((form `(defmethod ,(generic-function-name gf) ,@qualifiers
                 ,(loop for specializer in specializers
                        for (arg . rest) on lambda-list
                        collect `(,arg ,specializer) into args
                        finally (return (nconc args rest)))
                 ,@(cddr lambda-expression))))

    #+(or allegro clisp cmu scl)
    (funcall (compile nil `(lambda () ,form)))

    #+mcl (eval form)))

#+(or abcl clozure ecl clasp lispworks sbcl)
(defun ensure-method (gf lambda-expression
                         &key (method-class (generic-function-method-class gf))
                         (qualifiers ())
                         (lambda-list (cadr lambda-expression))
                         (specializers (required-args lambda-list (constantly (find-class 't)))))
  (multiple-value-bind
      (method-lambda method-args)
      (make-method-lambda
       gf (class-prototype method-class)
       lambda-expression ())
    (let ((method (apply #'make-instance
                         method-class
                         :qualifiers qualifiers
                         :lambda-list lambda-list
                         :specializers specializers
                         :function
                         #-ecl (compile nil method-lambda)
                         #+ecl (coerce method-lambda 'function)
                         method-args)))
      (add-method gf method)
      method)))

;; The following can be used in direct-slot-definition-class to get the correct initargs
;; for a slot. Use it like this:
;;
;; (defmethod direct-slot-definition-class
;;            ((class my-standard-class) &rest initargs)
;;   (destructuring-bind
;;       (&key key-of-interest &allow-other-keys)
;;       (fix-slot-initargs initargs)
;;     ...))

(defvar *standard-slot-keys*
  '(:name :documentation
    :initargs :initform :initfunction
    :readers :writers))

#+(or cmu scl)
(define-modify-macro nconcf (&rest lists) nconc)

(defun fix-slot-initargs (initargs)
  #+(or abcl allegro clisp clozure ecl clasp lispworks mcl mezzano sbcl)
  initargs

  #+(or cmu scl)
  (let* ((counts (loop with counts
                       for (key nil) on initargs by #'cddr
                       do (incf (getf counts key 0))
                       finally (return counts)))
         (keys-to-fix (loop for (key value) on counts by #'cddr
                            if (> value 1) collect key)))
    (if keys-to-fix
      (let ((multiple-standard-keys
             (intersection keys-to-fix *standard-slot-keys*)))
        (if multiple-standard-keys
          (error "Too many occurences of ~S in slot initargs ~S."
                 multiple-standard-keys initargs)
          (loop with fixed-keys
                for (key value) on initargs by #'cddr
                if (member key keys-to-fix)
                do (nconcf (getf fixed-keys key) (list value))
                else nconc (list key value) into fixed-initargs
                finally (return (nconc fixed-initargs fixed-keys)))))
      initargs)))
(in-package :closer-mop)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :closer-mop *features*))
;;; -*- Mode:Lisp; Package:CL-USER; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			    Package Definitions
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; packages.lisp,v 1.19 1995/04/05 21:39:44 chew Exp

(in-package :common-lisp-user)

(defvar *weyli-exported-symbols*
  '("*" "+" "-" "/" "=" ">" "<" ">=" "<=" "0?" "1?"

    "%MAX"
    "%MIN"
    "%PLUS"
    "%DIFFERENCE"
    "%TIMES"
    "%QUOTIENT"

    "*COERCE-WHERE-POSSIBLE*"
    "*DEFAULT-RANDOM-HEIGHT*"
    "*DOMAINS*"
    "*GENERAL*"
    "*MORPHISMS*"
    "*NEGATIVE-INFINITY*"
    "*POSITIVE-INFINITY*"
    "*PRINT-MODULUS*"

    "ABELIAN-GROUP"
    "ABELIAN-MONOID"
    "ABELIAN-SEMIGROUP"
    "ABSTRACT-POINT"
    "ABSTRACT-SPACE"
    "ADD-NEW-VARIABLE"
    "ADD-RELATION"
    "ADD-SUBSCRIPTS"
    "ADJACENCIES"
    "ALGEBRA"
    "ALGEBRAIC-EXTENSION"
    "ALL-NAMES"
    "ANGLES"
    "ARGS-OF"
    "ARGUMENT-OF"
    "BANACH-SPACE"
    "BASE-OF"
    "BIGFLOAT"
    "BOUNDARY"
    "BOUNDARY-COMPLEX-OF"
    "BOUNDARY-DOMAIN"
    "BOUND-VARS-OF"
    "CHARACTERISTIC"
    "CHOOSE"
    "COEFFICIENT"
    "COEFFICIENT-DOMAIN-OF"
    "COERCE"
    "COERCIBLE?"
    "COMBINATIONS"
    "COMPLEX-NUMBER"
    "COMPLEX-NUMBERS"
    "COMPOSE"
    "CONVERT-TO-LISP-NUMBER"
    "CREATE-MESH"
    "CROSS-PRODUCT"
    "DECLARE-DEPENDENCIES"
    "DEFINE-OPERATIONS"
    "DEGREE"
    "DELETE"
    "DEPENDS-ON?"
    "DERIV"
    "DERIVS-OF"
    "DESCRIBE-OPERATIONS"
    "DIFFERENT-KERNELS"
    "DIFFERENTIAL-RING"
    "DIMENSION-OF"
    "DIMENSIONAL-SPACE"
    "DIMENSIONS"
    "DIRECT-SUM"
    "DISPLAY"
    "DOMAIN-OF"
    "DOT-PRODUCT"
    "DRAW"
    "EQN="
    "EQN>"
    "EQN>="
    "EVEN?"
    "EUCLIDEAN-DOMAIN"
    "EVALUATE-AT"
    "EXPONENT-OF"
    "EXPR-OF"
    "EXPRS-OF"
    "EXPAND"
    "EXPT"
    "FACTOR"
    "FACTORIAL"
    "FIELD"
    "FINITE-FIELD"
    "FINITE-SET"
    "FOURIER"
    "FUNCT"
    "FUNCT-DOMAIN-OF"
    "FUNCT-OF"
    "FUNCT-RANGE-OF"
    "FUNCTION-SPACE"
    "GCD"
    "GCD-DOMAIN"
    "GE-ABS?"
    "GE-APPLICATION?"
    "GE-COS?"
    "GE-DERIV?"
    "GE-EQN=?"       
    "GE-EQN>?"
    "GE-EQN>=?"
    "GE-EQUAL"
    "GE-EXPT?"
    "GE-FOURIER?"
    "GE-FUNCTION?"
    "GE-FUNCTION-DERIV?"
    "GE-IFOURIER?"
    "GE-LOG?"
    "GE-NARY?"
    "GE-PLUS?"
    "GE-SIN?"
    "GE-TAN?"
    "GE-TIMES?"
    "GE-VARIABLE?"
    "GENERATORS-OF"
    "GET-ABSTRACT-SPACE"
    "GET-ALGEBRAIC-EXTENSION"
    "GET-AUTOMORPHISMS"
    "GET-CHAIN-MODULE"
    "GET-COMPLEX-NUMBERS"
    "GET-DIFFERENTIAL-RING"
    "GET-DIRECT-SUM"
    "GET-EUCLIDEAN-SPACE"
    "GET-FACTOR-GROUP"
    "GET-FACTOR-MODULE"
    "GET-FACTOR-RING"
    "GET-FINITE-FIELD"
    "GET-FREE-MODULE"
    "GET-FUNCTION"
    "GET-GL-N"
    "GET-HILBERT-SPACE"
    "GET-HOMOMORPHISMS"
    "GET-LISP-NUMBERS"
    "GET-MATRIX-SPACE"
    "GET-MORPHISMS"
    "GET-O-N"
    "GET-POLYNOMIAL-RING"
    "GET-PSL-N"
    "GET-QUATERNION-DOMAIN"
    "GET-QUOTIENT-FIELD"
    "GET-RATIONAL-INTEGERS"
    "GET-RATIONAL-NUMBERS"
    "GET-REAL-NUMBERS"
    "GET-SL-N"
    "GET-SO-N"
    "GET-TPOWER-SERIES-DOMAIN"
    "GET-UNIT-QUATERNION-DOMAIN"
    "GET-VARIABLE-PROPERTY"
    "GET-VARIABLE-NAME"
    "GET-VECTOR-SPACE"
    "GREATER-FUNCTION"
    "GFM"
    "GFP"
    "GROUP"
    "HEIGHT"
    "HILBERT-SPACE"
    "HOME-OF"
    "HOMOMORPHISM"
    "IFOURIER"
    "INNER-PRODUCT"
    "INSERT"
    "INSERT-BOUNDARY"
    "INTEGRAL"
    "INTEGRAL-DOMAIN"
    "INTERPOLATE"
    "JACOBIAN"
    "LEXICAL-<"
    "LEXICAL->"
    "LHS-OF"
    "LIST-OF-ELEMENTS"
    "LIST-OF-VARIABLES"
    "LIST-OPERATIONS"
    "LOCATE"
    "MAKE-APP-FUNCTION"
    "MAKE-CURVED-SEGMENT"
    "MAKE-GE-FUNCTION"
    "MAKE-GE-DERIV"
    "MAKE-GE-EXPT"
    "MAKE-GE-FUNCTION"
    "MAKE-GE-PLUS"
    "MAKE-GE-TIMES"
    "MAKE-GENERATOR"
    "MAKE-IDEAL"
    "MAKE-MESH"
    "MAKE-MESH-FROM-FILE"
    "MAKE-POINT"
    "MAKE-SAMPLED-FUNCTION"
    "MAKE-SIMPLEX"
    "MAKE-UNION"
    "MAKE-UNIVERSAL-QUANTIFIED-SET"
    "MAKE-GE-VARIABLE"
    "MAP"
    "MAP-OVER-CELLS"
    "MAP-OVER-MAXIMAL-CELLS"
    "MAP-OVER-ELEMENTS"
    "MAP-OVER-EXPRESSIONS"
    "MAP-OVER-FACES"
    "MAP-WITH-DOMAIN"
    "MATRIX-DIMENSIONS"
    "MAX"
    "MEMBER"
    "MEMOIZE"
    "MESH"
    "MIN"
    "MINIMAL-POLYNOMIAL"
    "MINUS?"
    "MONOID"
    "MORPHISM"
    "MULTIPLICATIVE-ORDER"
    "MUTABLE-SET"
    "NAME"
    "NAME-OF"
    "NAME-REGION"
    "NARGS-OF"
    "NORM"
    "NUMBER-OF-ELEMENTS"
    "NUMBER?"
    "ODD?"
    "ONE"
    "ONE-MATRIX"
    "OPERATION-ARGUMENTS"
    "OPERATION-VALUES"
    "OPPOSITE"
    "ORDERED-ABELIAN-GROUP"
    "ORDERED-RING"
    "ORDERED-SET"
    "ORDERED-SET-OF-PAIRS"
    "ORDERED-SIMPLE-SET"
    "PARTIAL-DERIV"
    "PARTITION"
    "PERMUTE"
    "PLUS?"
    "POCHHAMMER"
    "POINT"
    "POLYNOMIAL"
    "POWER-OF?"
    "PRIME?"
    "PROJECTIVE-SPACE"
    "QUOTIENT-FIELD"
    "QUOTIENT-RING"
    "RATIONAL-INTEGER"
    "RATIONAL-INTEGERS"
    "RATIONAL-NUMBER"
    "RATIONAL-NUMBERS"
    "READ-MESH"
    "REAL-NUMBER"
    "REAL-NUMBERS"
    "RECIP"
    "REDUCE-BASIS"
    "REF"
    "REFINE-MESH"
    "RELATIONS"
    "REMAINDER"
    "REPLACE"
    "REQUIRED-OPERATIONS"
    "RESET-DOMAINS"
    "RESULTANT"
    "REVERSION"
    "REVLEX->"
    "RHS-OF"
    "RING"
    "RING-VARIABLES"
    "RNG"
    "SCALAR?"
    "SEGMENT?"
    "SEMIGROUP"
    "SET"
    "SET-ELEMENTS"
    "SET-OF-PAIRS"
    "SIMPLE-FIELD-EXTENSION"
    "SIMPLE-RING"
    "SIMPLE-SET"
    "SIMPLEX"
    "SIMPLEX-SIZE"
    "SIMPLICIAL-COMPLEX"
    "SIMPLIFY"
    "SKEW-FIELD"
    "SPLIT"
    "SQUARE-FREE"
    "STRING-OF"
    "SUBFACE?"
    "SUBSTITUTE"
    "TAYLOR"
    "TERMS-OF"
    "TETRAHEDRON?"
    "TILDE"
    "TOTAL->"
    "TOTIENT"
    "TRANSPOSE"
    "TRIANGLE?"
    "TRUNCATE-ORDER"
    "TUPLE"
    "UNIQUE-FACTORIZATION-DOMAIN"
    "UNIVERSAL-QUANTIFIED-SET"
    "VAR-OF"
    "VAR-DOMAIN-OF"
    "VARIABLE-DERIVATION"
    "VARIABLE-INDEX"
    "VARLIST-OF"
    "VECTOR-SPACE"
    "VERTICES-OF"
    "WITH-MATRIX-DIMENSIONS"
    "WITH-NUMERATOR-AND-DENOMINATOR"
    "WRITE-MESH"
    "ZERO"
    "ZERO-MATRIX")
  "Symbols exported from the internal WEYL package.")

(defvar *weyli-shadowed-symbols*
  '(coerce set + - * / = > < >= <= minus expt abs random
    gcd lcm floor ceiling truncate round max min
    complex conjugate realpart imagpart
    sqrt exp log phase signum minusp zerop plusp
    sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh
    numerator denominator reduce 
    map delete member replace substitute getf union intersection
    apply funcall variable
    type-of)
  "Common lisp symbols shadowed in the internal WEYL package.")

(defvar *weyl-exported-symbols*
  '("MAKE-ELEMENT"
    "MAKE-UPOLYNOMIAL")
  "The WEYL package export these symbols in addition to those exported
form WEYLI.")

;; This should probably only be used to create the WEYL package
(defun use-weyli-package (package)
  "Shadow import the shadowed symbols in the WEYLI package and then
use it."
  (declare (special *weyli-shadowed-symbols*))
  (shadowing-import (loop for sym in *weyli-shadowed-symbols*
			  collect (intern (symbol-name sym) 'weyli))
		    package)
  (use-package (find-package 'weyli) package))

(defun use-weyl-package (package)
  "Shadow import the shadowed symbols in the WEYL package and then use
it."
  (declare (special *weyli-shadowed-symbols*))
  (shadowing-import (loop for sym in *weyli-shadowed-symbols*
			  collect (intern (symbol-name sym) 'weyl))
		    package)
  (use-package (find-package 'weyl) package))

(defun intern-in-package (package-name symbols)
  (loop for sym in symbols
	with  package = (find-package package-name)
	collect (intern sym package)))

(defpackage "WEYLI"
  (:use :common-lisp))

(shadow *weyli-shadowed-symbols* 'weyli)
(export (intern-in-package "WEYLI" *weyli-exported-symbols*) 'weyli)

(defpackage "WEYL"
  (:use :common-lisp))

(use-weyli-package 'weyl)

(export (intern-in-package "WEYL" *weyli-exported-symbols*) 'weyl)
(export (intern-in-package "WEYL" *weyl-exported-symbols*) 'weyl)

;;  Create the basic-graphics package.  This is package is needed if
;;  the system Mesh-Draw is loaded.
#-(and)
(unless (find-package "BASIC-GRAPHICS")
  #-Genera
  (make-package "BASIC-GRAPHICS" :nicknames '(BG)
		:use '(#-MCL LISP #+MCL CL #+PCL PCL
		       #+(and CLOS (not MCL)) CLOS))
  #+Genera
  (make-package "BASIC-GRAPHICS" :nicknames '(BG)
		:use '(#-Rel8 LISP #+Rel8 FUTURE-COMMON-LISP
		       #+PCL PCL #+CLOS CLOS)
		:colon-mode :external))
;;; -*- Mode:Lisp; Package:User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Lisp Support
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; lisp-support.lisp,v 1.8 1994/10/21 18:16:43 rz Exp

(in-package :common-lisp-user)

;;Infinities...
(defvar weyli::*positive-infinity*
  #+Genera si:infinite-positive-double-float
  #+Lucid system:float-positive-infinity
  #-(or Lucid Genera) most-positive-double-float)

(defvar weyli::*negative-infinity*
  #+Genera si:infinite-negative-double-float
  #+Lucid system:float-negative-infinity
  #-(or Genera Lucid) most-negative-double-float)

#|

 Extend defmethod slightly

 1. Allows the use of OR when defining the method specializer.

 DELETE : It is not clear how DEFMETHOD is being extended. Regardless,
 the code should be written to use the standard DEFMETHOD. When that is
 complete and verified, these macros should be deleted.

 This version is SBCL specific.
#+SB-PCL
(defmacro weyli::defmethod (&rest args &environment env)
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
	(sb-pcl::parse-defmethod args)
      (multiple-value-bind (proto-gf proto-method)
          (sb-pcl::prototypes-for-make-method-lambda name)
        `(progn
          ,@(loop for ll in (duplicate-arglist lambda-list)
                  collect
                  (sb-pcl::expand-defmethod name proto-gf proto-method qualifiers ll body env)))))))

#+(or MCL (and Genera CLOS))
(defun clos-parse-defmethod (form)
  (let ((name (pop form))
	qualifiers)
    (loop while (and (atom (first form))
		     (not (null (first form))))
	  do (push (pop form) qualifiers))
    (values name (reverse qualifiers) (first form) (rest form))))

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
|#

;;; The following predicate determines if class is a subclass of super-class.
;;; FIXME : Relies on the MOP and is SBCL specific.
(defun weyli::subclass-of? (class super-class)
  (when (symbolp class)
    (setq class (find-class class)))
  (when (symbolp super-class)
    (setq super-class (find-class super-class)))
  (labels ((search-list (list)
	     (or (member class list)
		 (loop for c in list
		       when (search-list (closer-mop:class-direct-superclasses c))
                       return t))))
    (or (eql class super-class)
	(search-list (closer-mop:class-direct-superclasses super-class)))))

(defmacro weyli::%apply (function &rest args)
  `(common-lisp:apply ,function ,@args))

(defun weyli::accum-apply-args (args)
  (if (null (rest args))
      (first args)
      (cons (first args) (weyli::accum-apply-args (rest args)))))

(defgeneric weyli::apply (function &rest args)
  (:documentation
   "Extend the common lisp standard APPLY."))

(defmethod weyli::apply (function &rest args)
  "Use the common lisp APPLY by default."
  (if (null args)
      (error "The function APPLY was called with too few arguments")
      (common-lisp:apply function (weyli::accum-apply-args args))))

(defmacro weyli::%funcall (function &rest args)
  `(common-lisp:funcall ,function ,@args))

(defgeneric weyli::funcall (function &rest args)
  (:documentation
   "Extend the common lisp standard FUNCALL."))

(defmethod weyli::funcall (function &rest args)
  "Rely on apply for performing the FUNCALL."
  (weyli::apply function args))

(defmacro weyli::%getf (place indicator &optional (default nil))
  (if default
      `(common-lisp:getf ,place ,indicator ,default)
      `(common-lisp:getf ,place ,indicator)))

(defgeneric weyli::getf (place indicator &optional default)
  (:documentation
   "Extend the common lisp standard GETF."))

(defmethod weyli::getf (place indicator &optional (default nil))
  "Use the common lisp GETF by default."
  (common-lisp:getf place indicator default))

(defgeneric weyli::putf (place indicator value)
  (:documentation
   "Extend the common lisp standard PUTF."))

(defmethod weyli::putf (place indicator value)
  "Use SETF and the common list standard GETF by default."
  (setf (common-lisp:getf place indicator) value))

(defsetf weyli::getf weyli::putf
    "Define weyli::getf settable.")

(defgeneric weyli::delete (item set &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard DELETE."))

(defmethod weyli::delete (item (sequence sequence) &rest args)
  "Use the common lisp DELETE function by default."
  (apply #'common-lisp:delete item sequence args))

(defgeneric weyli::member (item list &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard member."))

(defmethod weyli::member (item (list list) &rest args)
  "Use the common lisp MEMBER function by default."
  (apply #'common-lisp:member item list args))

(defgeneric weyli::replace (item list &key &allow-other-keys)
  (:documentation
   "Extend the common lisp standard replace."))

(defmethod weyli::replace ((item sequence) (list sequence) &rest args)
  "Use the common lisp REPLACE function by default."
  (apply #'common-lisp:replace item list args))

(defgeneric weyli::substitute (newitem olditem sequence &key &allow-other-keys)
  (:documentation
   "Extend the common lisp SUBSTITUTE function."))

(defmethod weyli::substitute (newitem olditem (seq sequence) &rest args)
  "Use the common lisp SUBSTITUTE function by default."
  (apply #'common-lisp:substitute newitem olditem seq args))

(defgeneric weyli::map (result-type function sequence &rest sequences)
  (:documentation
   "Extend the common lisp MAP function."))

(defmethod weyli::map (result-type function sequence &rest sequences)
  "Use the common lisp MAP function by default."
  (apply #'common-lisp:map result-type function sequence sequences))

(defgeneric weyli::reduce (function sequence &rest options)
  (:documentation
   "Extend the common lisp REDUCE function."))

(defmethod weyli::reduce (function (sequence sequence) &rest options)
  "Use the common lisp REDUCE function for sequences."
  (apply #'common-lisp:reduce function sequence options))

(defgeneric weyli::union (arg1 arg2 &rest rest)
  (:documentation
   "Extend the common lisp UNION function."))

(defmethod weyli::union ((arg1 list) (arg2 list) &rest rest)
  "Use the common lisp UNION function for lists."
  (apply #'common-lisp:union arg1 arg2 rest))

(defgeneric weyli::intersection (arg1 arg2 &rest rest)
  (:documentation
   "Extend the common lisp INTERSECTION function."))

(defmethod weyli::intersection ((arg1 list) (arg2 list) &rest rest)
  "Use the common lisp INTERSECTION function for lists."
  (apply #'common-lisp:intersection arg1 arg2 rest))

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
    (si:link-symbol-value-cells 'weyli::+ 'zl:+)))

#+Lucid
(setf (symbol-function 'lucid-old-top-level-eval) #'lucid::top-level-eval)

#+Lucid
(defun  lucid::top-level-eval (&rest arguments)
  (declare (special weyli::* weyli::+ cl:* cl:+))
  (multiple-value-prog1 (apply #'lucid-old-top-level-eval arguments)
    (setq weyli::* cl:*)
    (setq weyli::+ cl:+)))

;;; DELETE : defsubst is an archaic technique. It should be replaced
;;; with function definitions and, if justified, INLINE declarations.
(defmacro weyli::defsubst (function lambda-list &body body)
  `(#+Genera scl:defsubst
    #+Lucid  lcl:defsubst
    #-(or Genera Lucid) defun
    ,function ,lambda-list ,@body))

(defun weyli::%copy-array-contents* (from-array to-array)
  "Perform a shallow copy of the array contents."
  (if (equal (array-dimensions from-array) (array-dimensions to-array))
      (let ((from-flat (make-array
                        (array-total-size from-array)
                        :element-type (array-element-type from-array)
                        :displaced-to from-array))
            (to-flat (make-array
                      (array-total-size to-array)
                      :element-type (array-element-type to-array)
                      :displaced-to to-array)))
        (dotimes (index (array-total-size from-array) to-array)
          (setf (aref to-flat index) (aref from-flat index))))
      (error "Array dimensions are not equal.")))

(defmacro weyli::copy-array-contents (from-array to-array)
  #+Genera
  `(scl:copy-array-contents ,from-array ,to-array)
  #-Genera
  `(weyli::%copy-array-contents* ,from-array ,to-array))

(defun weyli::circular-list (&rest arguments)
  #+Genera (apply #'scl:circular-list arguments)
  #-Genera (nconc arguments arguments))

(weyli::defsubst structure-of (x)
  (common-lisp:type-of x))

;;; The following macros deal with certain functions that should take an
;;; arbitrary number of arguments.

;;; FIXME : This symbol should not be in CL-USER.
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

;;; FIXME : This symbol should not be in CL-USER.
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
                     ;; FIXME : max-pair needs to be defined prior to this.
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
                     ;; FIXME : min-pair needs to be defined prior to this.
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
                     ;; FIXME : plus needs to be defined prior to this.
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
                     ;; FIXME : plus needs to be defined prior to this.
		     (weyli::plus (first vals) (next-loop (rest vals))))))
        (if (null (rest values))
            ;; FIXME : minus needs to be defined prior to this.
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
                     ;; FIXME : times needs to be defined prior to this.
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
                     ;; FIXME : quotient needs to be defined prior to this.
		     (weyli::quotient (first vals) (next-loop (rest vals))))))
        (if (null (rest values))
            ;; FIXME : recip needs to be defined prior to this.
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
                     ;; FIXME : binary-gcd needs to be defined prior to this.
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
                     ;; FIXME : binary-lcm needs to be defined prior to this.
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

#|

DELETE : All of the following forms can probably be deleted.

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

|#
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Domains
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; domain-support.lisp,v 1.7 1995/05/24 17:41:59 rz Exp

(in-package "WEYLI")

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defclass has-property-list ()
  ((property-list :initform nil
                  :accessor property-list-of)))

(defmethod getf ((obj has-property-list) key &optional (default nil))
  (common-lisp:getf (property-list-of obj) key default))

(defmethod putf ((obj has-property-list) key value)
  (setf (common-lisp:getf (property-list-of obj) key) value))

(defun domain-print-object (d stream)
  (format stream "#<Domain: ~A>" (class-name (class-of d))))

(defclass domain (has-property-list)
  ((operation-table :initform (make-hash-table))
   (super-domains :initform nil
		  :initarg :super-domains
		  :accessor super-domains-of)
   (morphisms-from :initform nil
                   :accessor domain-morphisms-from)
   (morphisms-to :initform nil
                 :accessor domain-morphisms-to)
   (print-function :initform #'domain-print-object
                   :initarg :print-function)))

;;; FIXME : Merge with domain-print-object.
(defmethod print-object ((d domain) stream)
  (with-slots (print-function) d
    ;; This is so that you can pretty print objects in lucid.  It
    ;; appears, that you are not supposed to use PRINC inside these
    ;; methods.
    #+Lucid
    (let ((*print-pretty* nil))
      (funcall print-function d stream))
    #-Lucid
    (funcall print-function d stream)))

(defmacro define-operations (domain &body operations)
  `(defmethod parse-operations :after ((d ,domain))
    (parse-operation-list d ',operations)))

(defgeneric parse-operation-list (domain operation-list)
  (:documentation
   "The purpose of this method is not known."))

(defmethod parse-operation-list ((d domain) operation-list)
  (with-slots (operation-table) d
    (loop for ((operation . arguments) nil values) on operation-list by #'cdddr
	  do (setf (gethash operation operation-table)
		   (list operation arguments values)))))

;;; Need a dummy primary method to hang all the :after methods on.
;;; FIXME : Organize so that the primary method is not useless.
(defgeneric parse-operations (domain)
  (:method ((domain domain))
    nil)
  (:documentation
   "The purpose of this method is not known."))

;;; FIXME : Audit for merging with parse-operations.
(defmethod initialize-instance :after ((d domain) &rest plist)
  (declare (ignore plist))
  (parse-operations d))

(defgeneric list-operations (domain)
  (:documentation
   "Return a list of operations for the domain."))

;;; FIXME : Convert the maphash to a LOOP.
(defmethod list-operations ((d domain))
  (with-slots (operation-table) d
    (let (ops)
      (maphash #'(lambda (key value)
		   (declare (ignore value))
		   (push key ops))
	       operation-table)
      ops)))

(defgeneric operation-arguments (domain operation)
  (:documentation
   "The purpose of this method is not known."))

(defmethod operation-arguments ((d domain) operation)
  (with-slots (operation-table) d
    (subst (class-name (class-of d)) 'self
	   (second (gethash operation operation-table)))))

(defgeneric operation-values (domain operation)
  (:documentation
   "The purpose of this method is not known."))

(defmethod operation-values ((d domain) operation)
  (with-slots (operation-table) d
    (subst (class-name (class-of d)) 'self
	   (third (gethash operation operation-table)))))

(defgeneric describe-operations (domain &optional no-complaints)
  (:documentation
   "The purpose of this method is not known."))

#+Genera
(defmethod describe-operations ((d domain) &optional no-complaints)
  (declare (ignore no-complaints))
  (let* ((class-name (class-name (class-of d)))
	 (domain-element (cond ((null (rest (get class-name 'domain-elements)))
				(first (get class-name 'domain-elements)))
			       (t (format nil "~A element" class-name)))))
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      (format t "~&~S is a ~A~%" d class-name)
      (fresh-line)
      (with-slots (operation-table) d
	(scl:formatting-table ()
	  (scl:with-character-style ('(nil :italic nil))
	    (scl:formatting-row ()
	      (scl:formatting-cell ()
		(princ "Operation"))
	      (scl:formatting-cell ()
		(princ "Arguments"))
	      (scl:formatting-cell ()
		(princ "Values"))))
	  (maphash #'(lambda (key value)
		       (declare (ignore key))
		       (scl:formatting-row ()
			 (scl:formatting-cell ()
			   (princ (first value)))
			 (scl:formatting-cell ()
			   (format t "~A~{, ~A~}"
				   (canonicalize-class (first (second value)))
				   (mapcar #'canonicalize-class
					   (rest (second value)))))
			 (scl:formatting-cell ()
			   (princ (canonicalize-class (third value))))))
		   operation-table))))))

#-Genera
(defmethod describe-operations ((d domain) &optional no-complaints)
  (declare (ignore no-complaints))
  (let* ((class-name (class-name (class-of d)))
	 (element-classes (get class-name 'element-classes))
	 (domain-element (cond ((and element-classes
				     (null (rest element-classes)))
				(first element-classes))
			       (t (format nil "~A element" class-name)))))
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      (format t "~&~S is a ~A~%" d class-name)
      (fresh-line)
      (with-slots (operation-table) d
	(format t "Operation Arguments Values")
	(maphash #'(lambda (key value)
		     (declare (ignore key))
		     (format t "~&(~A ~A~{, ~A~}) -> ~A~%"
			     (first value)
			     (canonicalize-class (first (second value)))
			     (mapcar #'canonicalize-class
				     (rest (second value)))
			     (canonicalize-class (third value))))
		 operation-table)))))

(defgeneric required-operations (domain &optional fun)
  (:documentation
   "The purpose of this method is not known."))

(defmethod required-operations ((d domain) &optional fun)
  (let* ((class-name (class-name (class-of d)))
	 (element-classes (get class-name 'element-classes))
	 (domain-element (cond ((and element-classes
				     (null (rest element-classes)))
				(first element-classes))
			       (t (cons 'or element-classes))))
         list)
    (labels ((canonicalize-class (name)
	       (cond ((eql name 'self) class-name)
		     ((atom name) name)
		     ((equal name '(element self))
		      domain-element)
		     (t (mapcar #'canonicalize-class name)))))
      
      (unless fun
	(setq fun #'(lambda (form)
		      (push (cons (first form)
				  (mapcar #'canonicalize-class (second form)))
			    list))))
      (with-slots (operation-table) d
        (maphash #'(lambda (key value)
                     (declare (ignore key))
                     (%funcall fun value))
                 operation-table))
      list)))

(defun map-over-arglist-combinations (self arglist fun)
  (labels ((recur (arglist types) 
	     (cond ((null arglist)
		    (%funcall fun (reverse types)))
		   ((atom (first arglist))
		    (recur (rest arglist) (cons (first arglist) types)))
		   ((eql (first (first arglist)) 'or)
		    (loop for type in (rest (first arglist))
			  do (recur (cons type (rest arglist)) types)))
		   ((eql (first (first arglist)) 'element)
		    (loop for type in (get self 'element-classes)
			  do (recur (cons type (rest arglist)) types)))
		   (t (error "Don't understand arglist entry: ~S"
			     (first arglist))))))
    (recur (first arglist) ())))  

;;; DELETE : The method does not appear to be used anywhere.
(defgeneric check-domain (domain)
  (:documentation
   "The purspose of this method is not known."))

;;; FIXME : SBCL specific. Need to abstract for other implementations.
#+SB-MOP
(defmethod check-domain ((d domain))
  (required-operations
   d
   (lambda (form)
     (let ((operation (first form))
           (args (rest form))) 
       (map-over-arglist-combinations
        (class-name (class-of d)) args
        #'(lambda (arg-names) 
            (let ((args (loop for type in arg-names
                              collect (find-class type nil))))
              (loop for method in (sb-mop:generic-function-methods
                                   (symbol-function operation))
                    do (when (equal args
                                    (sb-mop::method-specializers method))
                         (return t))
                    finally (format t "No method for ~S~%"
                                    (cons operation arg-names))))))))))

;; Domain creators

;;; FIXME : Need to make creating domains atomic so that domains are
;;; not added to the list unless they are actually created.
(defvar *domains* ()
  "List of domains currently in use")

(defvar *general* ()  
  "The general representation domain")

(defun reset-domains ()
  (setq *domains* nil)
  (setf (domain-morphisms-from *general*) nil)  
  (setf (domain-morphisms-to *general*) nil))

(defmacro add-domain (predicate &body body)
  `(add-domain-internal ,predicate #'(lambda () ,@body)))

(defun add-domain-internal (predicate body)
  (let ((domain (find nil *domains*
                      :test #'(lambda (a b) 
                                (declare (ignore a))
                                (%funcall predicate b)))))
    (when (null domain)
      (setq domain (%funcall body))
      (push domain *domains*))
    domain))

(defun false (&rest args)
  (declare (ignore args))
  nil)

(defun true (&rest args)
  (declare (ignore args))
  t)

;;; FIXME : Need to ensure that the generic function is defined prior
;;; to the methods. The exact semantics depend on how this is used. It
;;; either needs to test for the existing of a generic function and
;;; create one if it doesn't exist or just create one if there should
;;; not already be one.
(defmacro define-domain-creator (name args creator &key predicate body)
  (labels ((parse-args (args)
	     (cond ((null args)
		    args)
		   ((member (first args) '(&optional &key))
		    (parse-args (rest args)))
		   ((eql (first args) '&rest)
		    (error "Can't handle &rest args here"))
		   ((atom (first args))
		    (cons (first args) (parse-args (rest args))))
		   (t (cons (first (first args))
			    (parse-args (rest args)))))))
    (let ((internal-fun (intern (format nil "MAKE-~A*" name)))
	  (true-args (parse-args args)))
      `(progn
        (defmethod ,internal-fun ,args ,creator)
        (defmethod ,(intern (format nil "MAKE-~A" name)) ,args
          (add-domain #'false (,internal-fun ,@true-args)))
        ,@(when predicate
                `((defmethod ,(intern (format nil "GET-~A" name)) ,args
                    (add-domain ,predicate (,internal-fun ,@true-args)))))
        ,@(when body
                `((defmethod ,(intern (format nil "GET-~A" name)) ,args
                    ,body)))))))

(defmacro with-new-weyl-context ((plist) &body body)
  `(let ((*domains* nil)
	 (*allow-coercions*
	  ,(or (%getf plist :allow-coercions) '*allow-coercions*)))
    ,@body))  

;; All elements of a domain should include this class

(defclass domain-element ()
  ((domain :initarg :domain
	   :reader domain-of)))

(defmacro define-domain-element-classes (domain &body element-classes)
  `(progn
    ;; At one time we thought there would be a one to one
    ;; correspondence between classes of domains and the classes of
    ;; their elements.  This isn't the case.  In addition, no uses
    ;; the element-class to domain-class correspondence, as you would
    ;; expect, so I'm not bothering to keep track of it.   --RZ 7/12/94
    #+ignore
    ,@(loop for element-class in element-classes
            collect
            `(cond ((eql (get ',element-class 'domain-class) ',domain))
              (t
               (when (get ',element-class 'domain-class)
                 (format t "WARNING: Reset domain-class of ~S~%"
                         ',element-class))
               (setf (get ',element-class 'domain-class) ',domain))))
    (setf (get ',domain 'element-classes) ',element-classes)))

(defgeneric domain-element-classes (domain)
  (:method ((domain domain))
    (get (class-name (class-of domain)) 'element-classes))
  (:documentation
   "The purpose of this method is not known."))

;; This is so that you can pretty print objects in lucid.  It appears,
;; that you are not supposed to use PRINC inside these methods.
#+Lucid
;; This must be an :around method since it must come before all the
;; primary methods.
(defmethod print-object :around ((object domain-element) stream)
  (let ((*print-pretty* nil))
    (call-next-method)))

(defgeneric coerce (elt domain)
  (:documentation
   "Coerce the element into the domain."))

(defgeneric coercible? (elt domain)
  (:documentation
   "Return true if the element is coercible into the domain."))

(defmacro defmethod-sd (op (x-spec y-spec) &body body)
  #+Genera
  (declare (zwei:indentation . wei:indent-for-clos-defmethod))
  (let ((x (if (atom x-spec) x-spec (first x-spec)))
	(y (if (atom y-spec) y-spec (first y-spec))))
    `(defmethod ,op (,x-spec ,y-spec)
      (let ((domain (domain-of ,x)))
        (cond ((eql domain (domain-of ,y))
               ,@body)
              (t (call-next-method)))))))

;; These are often of use when defining generic operations for domains.

(defvar *domain* ()
  "Within the context of an operation, the current domain")

(defgeneric %bind-dynamic-domain-context (domain function)
  (:documentation
   "The purpose of this method is not known.")
  (:method ((domain domain) function)
    (let ((*domain* domain))
      (%funcall function))))

(defmacro bind-domain-context (domain &body body)
  `(%bind-dynamic-domain-context ,domain 
    (lambda ()
      #+Genera (declare (sys:downward-function))
      ,@body)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Algebraic Domains
;;; ===========================================================================
;;; (c) Copyright 1989, 1994 Cornell University

;;; algebraic-domains.lisp,v 1.18 1995/05/24 17:41:57 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.18")

(defclass set (domain)
  ()
  (:documentation
   "A class for finite, unordered sets"))

;;; Some objects and/or domains have names. If so the following class
;;; should always be included.

(defclass has-name ()
  ((name :initarg :name :accessor name-of))
  (:documentation
   "Include this class in objects and/or domains that have names."))

(defgeneric binary= (x y)
  (:documentation
   "Elements of a set are assumed to be comparable using binary=. For
more complex set structures, the binary= operation looks up the
comparison function in EQUAL-FUNCTION slot."))

;; Ordered set's presume the existence of a > predicate.  We provide a
;; default for >= but that is likely to also be provided primitively.
;; < and <= are handled via macros since they just involve changing
;; the order of the arguments.

(defgeneric binary> (x y)
  (:documentation
   "Elements of a set are assumed to be comparable using binary>. For
more complex set structures, the binary> operation looks up the
comparison function in greater-funcion slot."))

(defgeneric binary>= (x y)
  (:method (x y)
    (or (binary> x y) (binary= x y))))

(defsubst binary< (x y) (binary> y x))

(defsubst binary<= (x y) (binary>= y x))

(defgeneric make-element (domain obj &rest rest)
  (:documentation
   "The purpose of this method is unknown."))

;;; Some data structure need to have ready access to equality and
;;; inequality comparison functions.  For instance, AVL trees, the
;;; exponent comparison function of expanded polynomials, etc.  The
;;; following classes provides a canonical place to cache this
;;; information

(defclass has-equality ()
  ((equal-function
    :initform #'binary=
    :initarg :equal-function
    :accessor equal-function-of))
  (:documentation
   "A canonical place to cache equality function information."))

(defclass has-comparison ()
  ((greater-function
    :initform #'binary>
    :initarg :greater-function
    :accessor greater-function-of))
  (:documentation
   "A cononical place to cache inequality function information."))

;;; Whether or not a set is ordered is a property, but elements of an
;;; ordered set can be compared using binary>.

;;; Properties indicate additional axioms that can be true about a
;;; domain. All the properties of an algebraic domain (e.g. field-ness)
;;; could treated as axioms, and in many ways this would be preferable,
;;; but I'm not quite willing to do this at this point.  --RZ

;;; When looking up a property, two values are returned: whether the
;;; property is true and whether we are certain of the result.

;;; Non strict domains!!  Non-Strict domains have all possible
;;; properties.  I.e. if it is syntactically possible to do an
;;; operation on the operands, then Weyl will go ahead and do it
;;; without checking any properties of the domain.

(defclass non-strict-domain ()
  ())

;;; FIXME : Move these to the top of the file.
(defvar *empty-property* (list nil))

(defvar *math-properties* ())

;;; FIXME : Add support for declarations and documentation.
;;; AUDIT : Needs a detailed audit.
(defmacro define-math-property (property-name subsumed-properties)
  (let ((predicate-name (intern (format nil "~A?" property-name)))
	(keyword (intern (symbol-name property-name)
			 (find-package 'keyword)))
	(assert-name (intern (format nil "ASSERT-~A" property-name))))
    `(progn
      (defgeneric ,predicate-name (domain))
      (defmethod ,predicate-name ((domain domain))
        (let ((val (getf domain ,keyword *empty-property*)))
          (if (eql val *empty-property*)
              (values nil nil)
              (values val t))))
      (defmethod ,predicate-name ((domain non-strict-domain))
        t)
      (defgeneric ,assert-name (domain))
      (defmethod ,assert-name ((domain domain))
        ,@(loop for prop in subsumed-properties
                collect `(,(intern (format nil "ASSERT-~A" prop)) domain))
        (setf (getf domain ,keyword) t))
      (pushnew ',property-name *math-properties*))))

(define-math-property ordered-domain ())

;; The following property is used to determine if floating point
;; numbers can be elements of the domain.

(define-math-property complete-set ())

;; Define-operations for sets is in sets.lisp

(defclass semigroup (set)
  ())

(defsubst semigroup? (domain) (typep domain 'semigroup))

(define-operations semigroup
  (times (element self) (element self)) -> (element self)
  (expt (element self) positive-integer) -> (element self))

(defgeneric times (x y))

(defgeneric expt (x y))

(defclass monoid (semigroup)
  ())

(defsubst monoid? (domain) (typep domain 'monoid))

(define-operations monoid
  (one self) -> (element monoid)
  (1? (element self)) -> Boolean
  (expt (element self) integer) -> (element self))

(defgeneric one (x))

(defmethod one ((domain domain))
  (coerce 1 domain))

(defgeneric 1? (x))

(defclass group (monoid)
  ())

(defsubst group? (domain) (typep domain 'group))

(define-operations group
  (recip (element self)) -> (element self)
  (expt (element self) Integer) -> (element self))

(defgeneric recip (x))

(defclass abelian-semigroup (set)
  ())

(defsubst abelian-semigroup? (domain) (typep domain 'abelian-semigroup))

(define-operations abelian-semigroup
  (plus (element self) (element self)) -> (element self)
  (times Integer (element self)) -> (element self))

(defgeneric plus (x y))

(defclass abelian-monoid (abelian-semigroup)
  ())

(defsubst abelian-monoid? (domain) (typep domain 'abelian-monoid))

(define-operations abelian-monoid
  (zero self) -> (element self)
  (0? (element self)) -> Boolean
  (times integer (element self)) -> (element self))

(defgeneric zero (domain))

(defmethod zero ((domain domain))
  (coerce 0 domain))

(defgeneric 0? (x))

(defclass abelian-group (abelian-monoid)
  ())

(defsubst abelian-group? (domain) (typep domain 'abelian-group))

(define-operations abelian-group
  (minus (element self)) -> (element self)
  (difference (element self) (element self)) -> (element self)
  (times integer (element self)) -> (element self))

(defgeneric minus (x))

(defgeneric difference (x y))

;; This is the mathematical definition of a RING.  It has just the
;; operations plus and times, and times distributes over plus.  In
;; most cases however we usually mean somewhat more.
(defclass rng (semigroup abelian-group)
  ())

#+IGNORE
(defaxiom rng ()
  (leftdistributive times plus)
  (rightDistributive times plus))

(defclass simple-ring (rng monoid)
  ())

(defsubst simple-ring? (domain) (typep domain 'simple-ring))

(define-operations simple-ring
  (characteristic self) -> Integer
  (one self) -> (element self)
  (1? (element self)) -> (element self)
  (recip (element self)) -> (element self))

(defclass has-coefficient-domain ()
  ((coefficient-domain
    :initform nil
    :initarg :coefficient-domain
    :reader coefficient-domain-of)))

(defvar *coefficient-domain* ()
  "Within the context of a polynomial operation, the coefficient
domain")

(defmethod %bind-dynamic-domain-context
    ((domain has-coefficient-domain) function)
  (let ((*coefficient-domain* (coefficient-domain-of domain)))
    (call-next-method)))

(defclass module (abelian-group has-coefficient-domain)
  ())

(defsubst module? (domain) (typep domain 'module))

(defgeneric characteristic (domain)
  (:documentation
   "The purpose of this method is not known."))

(defmethod characteristic ((domain module))
  (characteristic (coefficient-domain-of domain)))

;; The coefficient domain of an algebra should be a SIMPLE-RING 
(defclass algebra (module semigroup)
  ())

(defsubst algebra? (domain) (typep domain 'algebra))

(defclass ring (algebra simple-ring)
  ()
  ;; Also has the distributive law
  )

(defsubst ring? (domain) (typep domain 'ring))

(defmethod-sd max-pair ((x domain-element) (y domain-element))
  (if (> x y) x y))

(defmethod-sd min-pair ((x domain-element) (y domain-element))
  (if (> x y) y x))


(define-math-property integral-domain ())

;; A GCD can be defined
(define-math-property euclidean-domain (integral-domain))

;; A GCD algorithm exists
(define-math-property gcd-domain (euclidean-domain))

(defgeneric binary-gcd (x y))

(defgeneric binary-lcm (x y))

(define-math-property unique-factorization-domain (euclidean-domain))

;; Every field is an integral domain.
(defclass field (ring)
  ())

(defsubst field? (domain) (typep domain 'field))

(defmethod initialize-instance :after ((domain field) &rest plist)
  (declare (ignore plist))  
  (assert-integral-domain domain))

(define-operations field
  (quotient (element self) (element self)) -> (element self)
  (recip (element self)) -> (element self))

(defclass finite-field (field finite-set)
  ())

(defclass factor-domain (domain)
  ((factor-numer
    :initarg :numerator
    :accessor factor-numer-of)
   (factor-denom
    :initarg :denominator
    :accessor factor-denom-of)))

(defgeneric factor-domain-print-object (domain stream))

(defmethod factor-domain-print-object (domain stream)
  (format stream "~S/~S" (factor-numer-of domain) (factor-denom-of domain)))

(defmethod initialize-instance :after ((domain factor-domain) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) domain
    (setf print-function 'factor-domain-print-object)))

(defclass factor-group (factor-domain group)
  ())

(defclass factor-module (factor-domain module)
  ())

(defclass factor-ring (factor-domain ring)
  ())

(defclass algebraic-extension (ring)
  ())

(defclass simple-field-extension (algebraic-extension field)
  ())

;; A domain that has a dimension
(defclass dimensional-domain (domain)
  ((dimension
    :initform nil
    :initarg :dimension
    :reader dimension-of)))

(defgeneric dimensions (domain))

;;;added following to conform to manual -- rsp
(defmethod dimensions ((d dimensional-domain))
  (list (dimension-of d)))

(defclass free-module (module dimensional-domain)
  ())

(defsubst free-module? (domain) (typep domain 'free-module))

(defclass vector-space (free-module)
  ()
  ;; Coefficient domain must be a field
  )

(defsubst vector-space? (domain) (typep domain 'vector-space))

(defclass projective-space (free-module)
  ())

(defsubst projective-space? (domain) (typep domain 'projective-space))

(defclass differential-ring (ring)
  ())

(defsubst differential-ring? (domain) (typep domain 'differential-ring))

(define-operations differential-ring
  (deriv (element self)) -> (element self))

(defclass quotient-ring (domain)
  ())

;; Quotient Fields

(defclass quotient-field (field)  
  ((ring :initform nil :initarg :ring
	 :reader QF-ring)
   (zero :initform nil)
   (one :initform nil)))

(defmethod characteristic ((domain quotient-field))
  (characteristic (QF-ring domain)))

;; The accessors here must not be numerator and denominator because
;; sometimes the internal structure is not a domain element and we
;; actually want to get our hands on the internal structure.
;; NUMERATOR and DENOMINATOR always return domain elements.

(defclass quotient-element (domain-element)
  ((numerator :accessor qo-numerator
	      :initarg :numerator)
   (denominator :accessor qo-denominator
		:initarg :denominator)))

#+Genera
(defmacro with-numerator-and-denominator
    ((num denom) quotient-element &body body &environment env)
  (scl:once-only (quotient-element &environment env)
    `(let ((,num (qo-numerator ,quotient-element))
	   (,denom (qo-denominator ,quotient-element)))
      ,@body)))

#-Genera
(defmacro with-numerator-and-denominator
    ((num denom) quotient-element &body body)
  `(let ((,num (qo-numerator ,quotient-element))
	 (,denom (qo-denominator ,quotient-element)))
    ,@body))

;;; Concrete classes

;; Sets

(defclass mutable-set (set)
  ()
  (:documentation "Sets built from this class can be modified"))


(defclass finite-set (set)
  ())

(defclass set-element (domain-element)
  ((key :reader element-key
        :initarg :key)))

(defclass set-element1 (set-element)
  ())

(defclass set-element2 (set-element)
  ((value :accessor element-value
          :initarg :value)))

(defclass set-elements-as-singletons (set)
  ())

(defclass set-elements-as-pairs (set)
  ())

(defclass set-with-element-list (finite-set has-equality)
  ((elements
    :accessor set-element-list
    :initform (list nil)
    :initarg :elements)))

(defclass mutable-set-with-element-list (set-with-element-list mutable-set)
  ())

(defclass set-with-sorted-element-list (set-with-element-list has-comparison)
  ())

(defclass mutable-set-with-sorted-element-list
    (mutable-set-with-element-list has-comparison)
  ())

;; The intiable sets classes

(defclass simple-set (mutable-set-with-element-list set-elements-as-singletons)
  ())

(defclass set-of-pairs (mutable-set-with-element-list set-elements-as-pairs)
  ())

(defclass ordered-simple-set
    (mutable-set-with-sorted-element-list set-elements-as-singletons)
  ())

(defclass ordered-set-of-pairs
    (mutable-set-with-sorted-element-list set-elements-as-pairs) 
  ())


;; Numbers of all sorts

;; The following class is included in the numeric and
;; general-expression classes to minimize the number of different
;; methods that have to be created.  In the general expression code,
;; we usually want to treat numeric quantities the same as
;; general-expressions.  So, instead of generating two different methods we dispatch on ge-or-numeric.   --RZ 8/3/94

(defclass ge-or-numeric (domain-element) ())

;; All numeric quantities are built from this class (including
;; transcendental elements like e and pi when they exist).
(defclass numeric (ge-or-numeric)
  ())

;; All domains that consist solely of numeric elements contain this class
(defclass numeric-domain (domain)
  ())

(defsubst number? (x)
  (or (typep x 'cl:number)
      (typep x 'numeric)))

;; Rational integers

(defclass rational-integers (ring caching-zero-and-one numeric-domain)
  ())

(defmethod characteristic ((domain rational-integers))
  0)

(defclass rational-integer (numeric)
  ((value :initarg :value
          :reader integer-value)))

;; Real numbers 

(defclass real-numbers (field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain real-numbers) &rest plist)
  (declare (ignore plist))
  (assert-ordered-domain domain)
  (assert-complete-set domain))

(defmethod characteristic ((domain real-numbers))
  0)

(defclass real-number (numeric)
  ())

(defclass floating-point-number (real-number)
  ((value :initarg :value
          :reader fp-value)))

(defclass bigfloat (real-number)
  ((mantissa
    :reader bigfloat-mantissa
    :initarg :mantissa)
   (exponent
    :reader bigfloat-exponent
    :initarg :exponent)))


(defclass complex-numbers (algebraic-extension field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain complex-numbers) &rest plist)
  (declare (ignore plist))
  (assert-complete-set domain))

(defmethod characteristic ((domain complex-numbers))
  0)

(defclass complex-number (numeric)
  ((real
    :initarg :realpart
    :reader cn-realpart)
   (imag
    :initarg :imagpart
    :reader cn-imagpart)))

;;  Rational Numbers

(defclass rational-numbers (field numeric-domain)
  ())

(defmethod initialize-instance :after ((domain rational-numbers) &rest plist)
  (declare (ignore plist))
  (assert-ordered-domain domain))

(defmethod characteristic ((domain rational-numbers))
  0)

(defclass rational-number (quotient-element numeric)
  ())

;; Finite fields 

(defclass GFp (field numeric-domain)
  ((characteristic
    :initform 0
    :initarg :characteristic
    :reader characteristic)))

(defclass GFq (GFp)
  ((degree
    :initarg :degree
    :reader field-degree)))

(defclass GFp-element (numeric)
  ((value
    :reader gfp-value
    :initarg :value)))

(defclass GF2^n (GFq)
  ((reduction-table
    :initarg :reduction-table
    :reader GFp-reduction-table)))

(defclass GFm (rng numeric-domain)
  ())

(defclass GFm-element (numeric)
  ((value :initarg :value :reader value)
   (modulus :initarg :modulus :reader modulus)))

;; Polynomials

;; These are the pieces that are common to all polynomial domains and
;; polynomial representations.
(defclass has-ring-variables ()
  ((variables
    :initform nil
    :initarg :variables
    :reader ring-variables)))

(defclass polynomial-ring (ring module has-ring-variables)
  ())

;;FIXTHIS I don't think this is quite right.  I.e. Its not a GCD
;;domain for any coefficient domain.
(defmethod initialize-instance :after ((domain polynomial-ring) &rest plist)
  (declare (ignore plist))
  (assert-integral-domain domain))

;; Multivariate Polynomial rings need some structure to manage the their
;; variables.  This class provides hash tables and accessor methods of
;; this purpose.  This class is implementational.
(defclass variable-hash-table (has-ring-variables)  
  ((variable-hash-table
    :initform nil
    :accessor variable-hash-table)
   (variable-table
    :initform nil
    :accessor variable-index-table)))

;; It is often useful to cache the values of zero and one since they are
;; often needed.  Need to include the class domain here to make
;; caching... more specific than just domain.
(defclass caching-zero-and-one (domain)
  ((zero)
   (one)))

;; Multivariate polynomials

(defclass multivariate-polynomial-ring
    (polynomial-ring variable-hash-table caching-zero-and-one)
  ())

;; This is just the root of the polynomial structural type hierarchy.
(defclass polynomial (domain-element)
  ())

;; The following are the two different representation that are used.
;; An mpolynomial uses a recursive structure in the variables, while a
;; epolynomial is an expanded representation that uses exponent vectors.

(defclass mpolynomial (polynomial)
  ((form
    :reader poly-form
    :initarg :form)))

(defclass epolynomial (polynomial has-comparison)  
  ((form
    :reader poly-form
    :initarg :form)))

;; Univariate polynomials

(defclass upolynomial (polynomial)
  ((coef-list
    :reader poly-form
    :initarg :form)))

;; Power Series

(defclass power-series-domain (has-coefficient-domain caching-zero-and-one)
  ())

(defclass tpower-series-domain (power-series-domain)
  ())

(defclass tpower-series-ring (ring has-ring-variables tpower-series-domain)
  ())

(defclass tpower-series-field (field has-ring-variables tpower-series-domain)
  ())

(defclass power-series (domain-element)
  ())

;; Elements of truncated power series domains.  valence, branch-order,
;; and order should always be LISP integers (for now), and coeffs
;; should be a LISP simple vector whose elements are elements of the
;; coefficient domain
;;
;; Here is an example illustrating the use of branch-order, valence and
;; order.
;; Consider the power series 1/x + 1 + x^1/2 + x + x^2 + ... .
;; We first eliminate the fractional exponent "1/2" by putting u = x^1/2.
;; Now the series is u^-2 + 1 + u + u^2 + u^4 + ... with branch order 1/2.
;; Now, we normalize the series by multiplying it with u^2.
;; Now, we have 1 + u^2 + u^3 + u^4 + u^6 + ... with valence -2.
;; Finally, we truncate the series using order.
;;                                                     -- Sekhar 8/2/94.

(defclass tpower-series (power-series)
  ((valence
    :initform 0
    :initarg :valence
    :reader valence)
   (branch-order
    :initform 1
    :initarg :branch-order
    :reader branch-order)
   (order
    :initform *positive-infinity*
    :initarg :order
    :reader order)
   (coeffs
    :initarg :coeffs
    :reader coeffs)))

;; Rational functions

(defclass rational-function-field (quotient-field)  
  ())

(defclass rational-function (quotient-element)
  ())

;; Morphisms

(defclass morphism ()
  ((domain
    :reader morphism-domain
    :initarg :domain)
   (map
    :reader morphism-map
    :initarg :map)
   (range
    :reader morphism-range
    :initarg :range)))

(defclass homomorphism (morphism)
  ())

(defclass automorphism (homomorphism)
  ())

;; Differential domains

(defclass differential-polynomial-ring
    (multivariate-polynomial-ring differential-ring)
  ())

;; This is the base class of all ideals.  Recall that an ideal of a
;; ring R is an R module.  The ring slot is included to indicate the
;; domain that the generators lie in.  The generators themselves are only 
(defclass ideal (module)
  ((ring
    :initarg :ring
    :reader ring-of)
   ;; The current list of generators
   (generators
    :initform ()
    :initarg :generators
    :reader generators-of)))

;; Ideals over principal ideal domains are instances of this class
(defclass PID-ideal (ideal) ())

(defclass grobner-basis (ideal has-comparison)
  ;; The exponent comparison function is managed by HAS-COMPARISON
  ((undones :initform ())
   ;; A list of triples pairs (lt f g), lt(f)<=lt(g), of elements of
   ;; GENERATORS such that if any pair is not in the list, its s-poly
   ;; is guaranteed to be writable as a linear combination of
   ;; GENERATORS, with smaller s-degs
   (reducibles :initform nil :accessor reducibles-of)
   (possibles :initform nil)))

;; Algebraic Extensions

(defclass algebraic-extension-ring 
    (algebraic-extension multivariate-polynomial-ring)
  ())

(defclass algebraic-object (mpolynomial)
  ())


;; Direct Sums

;; These are the root classes.  Classes like DIRECT-SUM-SEMIGROUP are
;; created in the direct-sum.lisp file along with several support
;; methods.

(defclass direct-sum (tuple domain) ())

(defclass direct-sum-element (tuple domain-element) ())

;; Vector Spaces

(defclass free-module-element (tuple domain-element)
  ())

(defclass vector-space-element (free-module-element)
  ())

;; This optimization is included because lisp vectors are used as
;; exponents in the expanded polynomial representation.
(defclass lisp-vector-space (vector-space)
  ())

(defclass lisp-vector (vector-space-element)
  ())

;; Projective spaces

(defclass projective-space-element (vector-space-element)
  ())

;; Matrices

;; This is is the domain of all matrices over a given ring.
(defclass matrix-space (module) ())

(defclass real-matrix-space (matrix-space) ())

(defclass complex-matrix-space (matrix-space) ())

(defclass GL-n (group has-coefficient-domain dimensional-domain) 
  ()
  (:documentation "General linear group"))

(defclass PSL-n (GL-n)
  ())

(defclass SL-n (PSL-n)
  ())

(defclass O-n (GL-n)
  ())

(defclass SO-n (O-n)
  ())

(defclass matrix-element (domain-element)
  ((value
    :initarg :value
    :reader matrix-value)))

(defclass matrix-space-element (matrix-element)
  ((dimension1 :initarg :dimension1)
   (dimension2 :initarg :dimension2)))

;; These two classes are for efficiency
(defclass real-matrix-space-element (matrix-space-element) ())

(defclass complex-matrix-space-element (matrix-space-element) ())

(defclass GL-n-element (matrix-element)
  ())

(defclass PSL-n-element (GL-n-element)
  ())

(defclass SL-n-element (PSL-n-element)
  ())

(defclass O-n-element (GL-n-element)
  ())

(defclass SO-n-element (O-n-element)
  ())

;; Quaternions

(defclass quaternion-domain (vector-space algebra)
  ()
  (:documentation "algebra of quaternions"))

(define-operations quaternion-domain
  (conjugate (element self)) ->(element self))

(defclass unit-quaternion-domain
    (group dimensional-domain has-coefficient-domain)
  ()
  (:documentation "group of unit quaternions"))

(defclass quaternion-with-multiplication ()
  ())

(defclass quaternion-domain-element
    (quaternion-with-multiplication vector-space-element)
  ())

(defclass unit-quaternion-domain-element
    (quaternion-with-multiplication tuple domain-element)
  ())
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Space Classes
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

;;; space-classes.lisp,v 1.11 1995/05/30 18:10:14 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;; Topological Domains

;; Abstract spaces don't necessarily have a well defined dimension
(defclass abstract-space (domain) ())

(defclass dimensional-space (abstract-space dimensional-domain) ())

(defclass euclidean-space (vector-space dimensional-space) ())

;;  FIXTHIS: This class should probably be in a different file.
;;  Saves any results of coercions in a coercion cache accessible via
;;  Coerce.
(defclass has-coercion-cache ()
  ((coercion-cache
    :initform nil
    :accessor %coercion-cache-of)))

;;  Associates a unique id-number with each instance.
(defclass has-id-number ()
  ((global-counter
    :initform 0
    :allocation :class
    :accessor %global-id-counter-of)
   (id-number
    :reader id-number-of)))

(defclass has-name ()
  ((name :initarg :name :accessor name-of)))

(defmethod initialize-instance :after ((obj has-id-number) &rest ignore)
  (declare (ignore ignore))
  (with-slots (id-number) obj
    (setf id-number (incf (%global-id-counter-of obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Points.

;;  Base class of different types of points. 
(defclass abstract-point (has-id-number domain-element) ())

;; An abstract point with a name.  Points with the same name in the
;; same space are identical.
(defclass named-point (has-name abstract-point) ())

;;  A point may have different coordinates in different spaces.  The
;;  appropriate coordinates of a point are found using Coerce.
;;  General-Point is for point in possibly non-euclidean coordinate
;;  systems (polar, spherical, etc.)
(defclass general-point (tuple has-coercion-cache abstract-point) ())

(defclass point (vector-space-element general-point) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cells and Cell-Complex.

;;  To fit within a complex, each cell class must have the following
;;  functions: cell-id, facets, and dimension-of.  Function cell-id
;;  returns something (it doesn't really matter what) so that two
;;  cell-ids are #'equal iff the two cells are equivalent.  [It also
;;  has to hash efficiently, so for Lucid, we have to use id-numbers
;;  instead of points because all points hash to the same location.]
;;  Function facets returns all the subcells that are one dimension
;;  lower than the cell.  Function dimension-of does what you'd
;;  expect.
(defclass cell (has-id-number)
  ((orient :initform t :initarg :orient :accessor orient-of)))

(defclass simplex (cell)
  (;;  Maintained in order of id-number.
   (vertices :initform nil :initarg :vertices :reader vertices-of)))


;;; there must be a better place for ORIENTED-SORT
(defun oriented-sort (list)
  "Sort keeping track of the number of swaps"
  ;; bubble sort
  (loop with orient = t
	for l1 on list do
	(loop for l2 on (rest l1) do
	      (when (cl:< (id-number-of (first l2)) (id-number-of (first l1)))
		(setf orient (null orient))
		(psetf (first l1) (first l2)
		       (first l2) (first l1))))
	finally (return (values list orient))))

(defmethod initialize-instance :after ((simplex simplex)
				       &rest ignore &key home)
  (declare (ignore ignore home))
  (with-slots (vertices orient) simplex
    (multiple-value-bind (v o)
        (oriented-sort (copy-list vertices))
      (setf vertices v
            orient o))))

(defclass polygon (cell)
  ( ;;  Maintained with smallest id-number first, then adjacent
   ;;  vertex with smaller id-number, followed by other vertices in
   ;;  order around the polygon.
   (vertices :initform nil :initarg :vertices :reader vertices-of)))

(defmethod initialize-instance :after ((polygon polygon) &rest ignore)
  (declare (ignore ignore))
  (warn "Polygons are not completely implemented."))

(defclass cell-complex ()
  ( ;;  Used to recognize cells that are equivalent.
   (cell-table :initform (make-hash-table :test #'equal)
               :reader cell-table-of)
   (facet-table :initform (make-hash-table) :reader facet-table-of)
   (cofacet-table :initform (make-hash-table) :reader cofacet-table-of)))

(defclass simplicial-complex (cell-complex) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Chains.

(defclass chain-module (module)
  ((complex :initarg :complex :reader complex-of)
   (dim :initform 0 :initarg :dimension :reader dimension-of)))

(defclass cochain-module (module)
  ((complex :initarg :complex :reader complex-of)
   (dim :initform 0 :initarg :dimension :reader dimension-of)))

;; Chains are elements of chain-modules
(defclass chain (domain-element)
  ((terms :initarg :terms :accessor chain-terms-of)))

;; Mathematically, cochains would probably not inherit from chains.
;; This is done to simplify implemention -- since chains and cochains
;; are structurally identical, we'll inherit all functionality from
;; chains.  Fix this later???
(defclass cochain (chain)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Spaces

(defclass function-space (vector-space dimensional-space ring)
  ((funct-domain :initarg :domain :reader funct-domain-of)
   (funct-range :initarg :range :reader funct-range-of)))

(defclass function-space-element (domain-element)
  ())

(defmethod initialize-instance :after ((h function-space) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) h
    (setf print-function 'function-space-print-function)))

(defun function-space-print-object (h  stream)
  (format stream "C(~S->~S)" (funct-domain-of h) (funct-range-of h)))

;; The domain and range for a Banach space should both be geometric domains
(defclass Banach-space (function-space)
  ())

;;Both Banach and Hilbert spaces are supposed to be complete under the
;;norm.  I wonder how that is defined.

(define-operations Banach-space
  (norm (element self)) -> REAL-NUMBERS)

(defclass Hilbert-space (Banach-space)
  ())

(define-operations Hilbert-space
  (inner-product (element self) (element self)) -> REAL-NUMBERS)

(defclass hilbert-space-element (function-space-element)
  ())

(defmethod initialize-instance :after ((h hilbert-space) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) h
    (setf print-function 'hilbert-space-print-function)))

(defun hilbert-space-print-object (h stream)
  (format stream "Hilb(~S, ~S)" (funct-domain-of h) (funct-range-of h)))

;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		    General Representation Classes
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; general-classes.lisp,v 1.5 1995/05/24 17:42:00 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(defclass has-memoization ()
  ((memos :initform (make-hash-table :test #'equal))))

;;; Classes for General expressions

(defvar *global-functions* ()
  "These are the functions known by everyone")

(defclass general-expressions (has-memoization non-strict-domain domain)
  ((variables
    :initform ()
    :accessor ge-variables)
   (functions
    :initform nil
    :accessor ge-functions)
   (context
    :initform ()
    :accessor ge-context)))

(defclass general-expression (ge-or-numeric)
  ((simplified?
    :initform nil
    :accessor simplified?)))

;; This class is used to define those general expressions that are
;; indivisible.
(defclass ge-atom () ())

(defsubst ge-atom? (x) (typep x 'ge-atom))

(defclass ge-variable (general-expression has-property-list ge-atom)
  ((symbol
    :initarg :symbol
    :accessor symbol-of)
   (string
    :initarg :string
    :accessor string-of)))

;; N-ary operators are built from this class 
(defclass ge-nary (general-expression)
  ((terms
    :initform nil
    :initarg :terms
    :accessor terms-of)))

(defsubst ge-nary? (x) (typep x 'ge-nary))

(defclass ge-plus (ge-nary)
  ())

(defsubst ge-plus? (x) (typep x 'ge-plus))

(defclass ge-times (ge-nary)
  ())

(defsubst ge-times? (x) (typep x 'ge-times))

(defclass ge-expt (general-expression)
  ((base
    :initarg :base
    :accessor base-of)
   (exp
    :initarg :exp
    :accessor exponent-of)))

(defsubst ge-expt? (x) (typep x 'ge-expt))

;; FUNCTIONS

;; Functions themselves are first class objects.  So we need a representation 
;; for a function, and a second representation for a functional application. 
;; These functions should be cached just the way variables are cached. 

;; ABSTRACT-FUNCTION is the base class for all functions.  At a minimum 
;; all functions have a specified number of arguments, and a property-list 
;; in which additional information can be stored. 
(defclass abstract-function (domain-element has-property-list)
  ((nargs
    :initarg :nargs                     ; The number of arguments
    :accessor nargs-of)))

;; This separated out so that we can implemented existential as well
;; universal quantifiers.
(defclass has-bound-variables ()
  ((bound-vars
    :initarg :bound-vars
    :accessor bound-vars-of)))

;; APPLICABLE-FUNCTION indicates that this object is a symbolic 
;; lambda expression
(defclass applicable-function (abstract-function has-bound-variables)
  ((body
    :initarg :body
    :reader body-of)))

(defsubst applicable-function? (x) (typep x 'applicable-function))

;; GE-FUNCTION is the class of named functions, 
(defclass ge-function (abstract-function has-name)
  ())

(defsubst ge-function? (x) (typep x 'ge-function))

;; GE-FUNCTION-DERIV is used to represent the derivative of a function. 
;; The DERIVS slot is used to hold an order list of the derivatives.  Each 
;; element of the list is a number from 0 to nars - 1 indicating a derivative 
;; in that position.  Numbers can appear more than once and are sorted.

(defclass ge-function-deriv (ge-function)
  ((derivs
    :initform ()
    :initarg :derivs
    :accessor derivs-of)))

;; Notice that a GE-FUNCTION-DERIV is also a GE-FUNCTION
(defsubst ge-function-deriv? (x) (typep x 'ge-function-deriv))

(defclass ge-application (general-expression)
  ((funct
    :initarg :funct                     ; The function being applied
    :accessor funct-of)
   (args
    :initarg :args                      ; Arguments to the function
    :accessor args-of)))

(defsubst ge-application? (x) (typep x 'ge-application))

(defclass ge-equation (general-expression)
  ((lhs
    :initarg :lhs
    :accessor lhs-of)
   (rhs
    :initarg :rhs
    :accessor rhs-of)))

(defclass ge-eqn= (ge-equation)
  ())

(defsubst ge-eqn=? (exp)
  (typep exp 'ge-eqn=))

(defclass ge-eqn> (ge-equation)
  ())

(defsubst ge-eqn>? (exp)
  (typep exp 'ge-eqn>))

(defclass ge-eqn>= (ge-equation)
  ())

(defsubst ge-eqn>=? (exp)
  (typep exp 'ge-eqn>=))

;; The expression must be general-expression, so I am also make a univerally 
;; quantified set a domain-element
(defclass universal-quantified-set (has-bound-variables set general-expression)
  ((exprs
    :initarg :expressions  
    :accessor exprs-of)))

;; Fourier transforms 

(defclass ge-fourier (general-expression)
  ((argument
    :initarg :argument
    :accessor argument-of)
   (space-var
    :initarg :space-var
    :accessor space-var-of)
   (freq-var
    :initarg :freq-var
    :accessor freq-var-of)))

(defsubst ge-fourier? (x) (typep x 'ge-fourier))

(defclass ge-ifourier (general-expression)
  ((argument
    :initarg :argument
    :accessor argument-of)
   (space-var
    :initarg :space-var
    :accessor space-var-of)
   (freq-var
    :initarg :freq-var
    :accessor freq-var-of)))

(defsubst ge-ifourier? (x) (typep x 'ge-ifourier))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    AVL trees
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; avl.lisp,v 1.6 1994/10/24 14:23:30 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; FIXTHIS:  This is a stupid place for this code

(defmacro choose (seq (var n . options) &body body)
  (cond ((%getf options :allow-repeats)
	 `(%choose-repeats ,seq ,n #'(lambda (,var) ,@body)))
	(t `(%choose ,seq ,n #'(lambda (,var) ,@body)))))

(defgeneric %choose (variables number function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod %choose ((vars list) n fn)
  (unless (or (null n) (cl:integerp n))
    (error "Invalid count argument to CHOOSE: ~D" n))
  (labels ((pick (vars n so-far)
	     (declare (fixnum n))
	     (cond ((cl:zerop n)
		    (%funcall fn so-far))
		   (t (pick (rest vars) (cl:1- n) (cons (first vars) so-far))
		      (if (> (length vars) n)
			  (pick (rest vars) n so-far)))))
	   (pick-null (vars so-far)
	     (when vars
	       (let ((new-so-far (cons (first vars) so-far)))
 	         (%funcall fn new-so-far)
		 (pick-null (rest vars) new-so-far)
		 (when (rest vars)
		   (pick-null (rest vars) so-far))))))
    (let ((len (length vars)))
      (declare (fixnum len))
      (cond ((null n)
	     (%funcall fn nil)
	     (pick-null vars ()))
	    ((cl:> n len)
	     (error "Not that many elements in vars"))
	    ((or (cl:= n len) (cl:zerop len))
	     (%funcall fn vars))
	    ((cl:minusp n)
	     (pick vars (cl:+ len n) ()))	   
	    (t (pick vars n ()))))))

(defgeneric %choose-repeats (variables number function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod %choose-repeats ((vars list) n fn)
  (unless (or (null n) (cl:integerp n))
    (error "Invalid count argument to CHOOSE: ~D" n))
  (labels ((pick (vars n so-far)
	     (declare (fixnum n))
	     (cond ((cl:zerop n)
		    (%funcall fn so-far))
		   (t (loop while vars do
                            (pick vars (cl:1- n) (cons (first vars) so-far))
                            (setq vars (rest vars)))))))
    (let ((len (length vars)))
      (declare (fixnum len))
      (cond ((cl:> n len)
	     (error "Not that many elements in vars"))
	    ((cl:minusp n)
	     (pick vars (cl:+ len n) ()))	   
	    (t (pick vars n ()))))))

;; FIXTHIS: At some point put in code for even and odd permutations.
(defmacro permute (seq (var . options) &body body)
  (declare (ignore options))
  `(permute% ,seq #'(lambda (,var) ,@body)))

(defgeneric permute% (sequence function)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod permute% ((seq list) fun)
  (labels ((pick (vars so-far)
	     (cond ((null vars) (%funcall fun so-far))
		   (t (loop for v in vars
			    do (pick (remove v vars) (cons v so-far)))))))
    (pick seq nil)))

(defun %partition1 (n fun)
  (labels ((part-int (n min list) 
             (declare (type fixnum n))
             (cond ((cl:zerop n) (funcall fun list))
                   (t (loop for i fixnum upfrom min below (cl:+ 1 n)
                            do (part-int (cl:- n i) i (cons i list)))))))
    (part-int n 1 nil)
    (values)))

;; N is the number to be partitioned, FUN is the function to apply to
;; the partition.

;; NUM-PARTS is the number of parts to be used in the partitioning.
;; If negative, then any number of parts can be used.
;; MAXIMUM-PART is the maximum size of any component in the partition. 

(defun %partition2 (n fun num-parts minimum-part maximum-part distinct?)
  (labels ((part-int (n min parts list)
	     (declare (fixnum n parts))
             (cond ((cl:zerop n)
		    (if (not (cl:plusp parts)) (funcall fun list)))
		   ((or (cl:minusp n) (cl:zerop parts)))
                   (t (loop for i fixnum upfrom min below maximum-part
                            do (part-int (cl:- n i)
					 (if distinct? (cl:1+ i) i)
					 (cl:-  parts 1)
                                         (cons i list)))))))
    (part-int n minimum-part num-parts nil)
    (values)))

(defmacro partition ((var n . options) &body body)
  (let ((num-parts (or (getf options :number-of-parts) -1))
	(minimum-part (or (getf options :minimum-part) 1))
	(maximum-part `(cl:1+ ,(or (getf options :maximum-part) '.number.))))
    (loop for (keyword nil) on options by #'cddr
	  with bad-keywords
	  do (unless (member keyword
			     '( :number-of-parts :minimum-part :maximum-part
			       :distinct?))
	       (push keyword bad-keywords))
	  finally (when bad-keywords
		    (error "Invalid options to partition: ~S"
			   bad-keywords)))
    (cond ((and (null num-parts)
		(null maximum-part))
           `(%partition1 ,n #'(lambda (,var) ,@body)))
          (t `(let ((.number. ,n))
               (%partition2 .number. #'(lambda (,var) ,@body)
                ,num-parts ,minimum-part ,maximum-part
                ,(getf options :distinct?)))))))

#| Partition Demonstation routines |
(defun part-count (n)
  (let ((cnt 0))
    (partition (l n)
      (declare (ignore l))
      (incf cnt))
    cnt))
||#

(defmacro map-over-tree (node (root . options) &body body)
  (let ((collection-fun (or (%getf options :collection-fun) 'identity))
	(breadth-first? (%getf options :breadth-first?))
	(depth-first? (%getf options :depth-first?)))
    (when (and breadth-first? depth-first?)
      (error "Can't specify both breadth and depth first in MAP-OVER-TREE"))
    (cond (breadth-first?
	   `(let ((.collections. (list ,root))
		  .temp. .new-collections.)
             (loop while .collections. do
              (loop for ,node in .collections. do
               (macrolet ((terminate-branch ()
                            `(return-from .mapper.block. nil)))
                 (block .mapper.block.
                   ,@body
                   (when (cl:listp (setq .temp. (,collection-fun ,node)))
                     (setq .new-collections.
                           (nconc .new-collections. (copy-list .temp.)))))))
              (setq .collections. .new-collections.)
              (setq .new-collections. nil))))
	  (depth-first?
	   `(labels ((mapper.fn (,node)
                      (macrolet ((terminate-branch ()
                                   `(return-from .mapper.block. nil)))
                        (block .mapper.block.
                          ,@body
                          (let ((.collection. (,collection-fun ,node)))
                            (when (cl:listp .collection.) 
                              (loop for .node. in .collection.
                                    do (mapper.fn .node.))))))))
             (mapper.fn ,root)))
	  (t (error "Must specify either breadth or depth first in MAP-OVER-TREE")))))	     

;; Need to do the non-mutating version also.  I think this can be done
;; by just changin update-node...

;; This code comes is derived from code that was originally written by
;; Bruce Donald.

;; AVL trees

(defclass avl-tree (has-comparison)
  ((root
    :initform nil
    :accessor avl-root)))

(defclass avl-node (set-element)
  ((left
    :initform nil
    :initarg :left
    :accessor avl-left)
   (right
    :initform nil
    :initarg :right
    :accessor avl-right)
   (balance
    :initform 0
    :initarg balance
    :accessor avl-balance)))

(defclass avl-tree-elements-as-singletons (set-elements-as-singletons)
  ())

(defclass avl-node-as-pairs (set-element2 avl-node)
  ())

(defclass avl-tree-elements-as-pairs (set-elements-as-pairs)
  ())

(defclass simple-avl-tree (avl-tree avl-tree-elements-as-singletons)
  ())

(defclass avl-tree-of-pairs (avl-tree avl-tree-elements-as-pairs)
  ())

(defgeneric avl-size (tree)
  (:documentation
   "Return the size of the avl-tree."))

(defmethod avl-size ((tree avl-tree))
  (let ((root (avl-root tree)))
    (if root (avl-size root)
	0)))

(defmethod avl-size ((node avl-node))
  (let ((left (avl-left node))
	(right (avl-right node)))
    (1+ (cl:+ (if left (avl-size left) 0)
              (if right (avl-size right) 0)))))

(defmethod print-object ((tree avl-tree) stream)
  (format stream "#<AVL tree: ~D elts>" (avl-size tree)))

(defmethod print-object ((node avl-node) stream)
  (format stream "<AVL~[-~;=~;+~]: ~S>"
	  (1+ (avl-balance node)) (element-key node)))

;;; This is for debugging
#+ignore
(defmethod pretty-print-object ((node avl-node) &optional (stream *standard-output*))
  (labels ((indent (n)
	     (loop for i below n do (princ " " stream)))
	   (pp (node indent)
	     (when (avl-left node)
	       (indent indent)
	       (format stream  "L: ~S~%" (avl-left node))
	       (pp (avl-left node) (cl:+ 2 indent)))
	     (when (avl-right node)
	       (indent indent)
	       (format stream  "R: ~S~%" (avl-right node))
	       (pp (avl-right node) (cl:+ 2 indent)))))
    (format stream "~&Root: ~S~%" node)
    (pp node 2)))

(defgeneric update-node (node balance left right &rest args)
  (:documentation
   "This is used to pudate a node with new information since we don't
know all the information that could be stored in a node we've assumed
they can all be lumped into args."))

(defmethod update-node ((node avl-node) balance left right &rest args)
  (declare (ignore args))
  (setf (avl-balance node) balance)
  (setf (avl-left node) left)
  (setf (avl-right node) right)
  node)

(defgeneric empty? (tree)
  (:documentation
   "Return true if the tree is empty.")
  (:method ((tree avl-tree))
    (null (avl-root tree))))

(defgeneric avl-height (tree)
  (:documentation
   "This determines the height of an AVL tree and also checks if your
tree is out of balance or 'Koyaanisquatsi' in Hopi Indian. Actual
height difference should be the same as the balance value, and should
be in the range {-1,0,1}."))

(defmethod avl-height ((tree avl-tree))
  (let ((root (avl-root tree)))
    (if root (avl-height root) 0)))

(defmethod avl-height ((node avl-node))
  (let ((hl (if (avl-left node) (avl-height (avl-left node))
		0))
	(hr (if (avl-right node) (avl-height (avl-right node))
		0)))
    (cond ((not (eql (cl:- hr hl) (avl-balance node)))
	   (format t "~
              The actual height difference ~S does not agree with the ~%~
              balance entry ~S for node ~S"
		   (cl:- hr hl) (avl-balance node) node))
	  ((cl:> (cl:abs (avl-balance node)) 1)
	   (format t "Node ~S is Koyaanisquatsi, its balance value is ~S"
		   node (avl-balance node))))
    (cl:1+ (cl:max hl hr))))

(defgeneric left-most (node)
  (:documentation
   "Return true if the node is the left most node of the tree."))

(defmethod left-most ((node avl-node))
  (labels ((find-left-most (node)
	     (cond ((null (avl-left node)) node)
		   (t (find-left-most (avl-left node))))))
    (find-left-most node)))

(defgeneric avl-maximum (tree)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((tree avl-tree))
    (left-most (avl-root tree))))

(defgeneric map-over-each-node (tree function)
  (:documentation
   "Map over each node of the tree applying the function."))

(defmethod map-over-each-node ((tree avl-tree) function)
  (labels ((map-over (node)
	     (unless (null (avl-left node))
	       (map-over (avl-left node)))
	     (%funcall function node)
	     (unless (null (avl-right node))
	       (map-over (avl-right node)))))
    (let ((root (avl-root tree)))
      (when root 
	(map-over root)))))

(defgeneric make-generator (tree)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-generator ((tree avl-tree))
  (let (stack)
    (macrolet ((current-state () `(first (first stack)))
	       (set-current-state (state) `(setf (first (first stack)) ,state))
	       (current-node () `(rest (first stack)))
	       (new-node (node) `(push (cons :left ,node) stack)))
      (labels ((scan ()
		 (cond ((null stack) nil)
		       ((eql (current-state) :left) 
			(cond ((null (avl-left (current-node))) 
			       (set-current-state :right)
			       (current-node))
			      (t (set-current-state :here)
				 (new-node (avl-left (current-node)))
				 (scan))))
		       ((eql (current-state) :here)
			(set-current-state :right)
			(current-node))
		       (t ;; (eql (current-state) :right)
			(cond ((null (avl-right (current-node)))
			       (pop stack)
			       (scan))
			      (t (new-node
				  (prog1 (avl-right (current-node))
				    (pop stack)))
				 (scan))))
		       )))
	(new-node (avl-root tree))
	#'scan))))    

(defgeneric right-most (node)
  (:documentation
   "Return true if the node is the right most of the tree."))

(defmethod right-most ((node avl-node))
  (labels ((find-right-most (node)
	     (cond ((null (avl-right node)) node)
		   (t (find-right-most (avl-right node))))))
    (find-right-most node)))

(defgeneric avl-minimum (tree)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((tree avl-tree))
    (right-most (avl-root tree))))

(defgeneric avl-next (key tree-or-node)
  (:documentation
   "Return the next item of the tree or node."))

(defmethod avl-next (key (tree avl-tree))
  (avl-next key (avl-root tree)))

(defmethod avl-next (key (node avl-node))
  (labels ((next-loop (node)
	     (and node
		  (if (not (> node key))
		      (next-loop (avl-right node))
		      (or (next-loop (avl-left node))
			  node)))))
    (next-loop node)))

(defgeneric avl-previous (key tree-or-node)
  (:documentation
   "Return the next item of the tree or node."))

(defmethod avl-previous (key (tree avl-tree))
  (avl-previous key (avl-root tree)))

(defmethod avl-previous (key (node avl-node))
  (labels ((next-loop (node)
	     (and node
		  (if (not (> key node))
		      (next-loop (avl-left node))
		      (or (next-loop (avl-right node))
			  node)))))
    (next-loop node)))

(defgeneric balance-right (node height-change)
  (:documentation
   "Balance a TREE that is right-Koyaanisquatsi, i.e. the right
subtree is 2 levels higher than the left subtree. HEIGHT-CHANGE is the
height of TREE relative to its value before the delete/insert
operation. Balance-right returns a node and the height of that node
relative to the original height of TREE."))

(defmethod balance-right ((node avl-node) height-change)
  (let ((r (avl-right node)))
    (cond ((cl:plusp (avl-balance r))
	   (setq node (update-node node 0 (avl-left node) (avl-left r)))
	   (setq r (update-node r 0 node (avl-right r)))
	   (values r (1- height-change)))
	  ((cl:zerop (avl-balance r))
	   (setq node (update-node node 1 (avl-left node) (avl-left r)))
	   (setq r (update-node r -1 node (avl-right r)))
	   (values r height-change))
	  (t (let ((lr (avl-left r)))
	       (setq r (update-node r (if (cl:minusp (avl-balance lr)) 1 0)
				    (avl-right lr) (avl-right r)))
	       (setq node (update-node node
				       (if (cl:plusp (avl-balance lr)) -1 0)
				       (avl-left node) (avl-left lr)))
	       (setq lr (update-node lr 0 node r))
	       (values lr (1- height-change)))))))

(defgeneric balance-left (node height-change)
  (:documentation
   "Balance a TREE that is left-Koyaanisquatsi, i.e. the left subtree
is 2 levels higher than the right subtree. HEIGHT-CHANGE is the height
of TREE relative to its value before the delete/insert operation.
Balance-left returns a node and the height of that node relative to
the original height of TREE."))

(defmethod balance-left ((node avl-node) height-change)
  (let ((l (avl-left node)))
    (cond ((cl:minusp (avl-balance l))
	   (setq node (update-node node 0 (avl-right l) (avl-right node)))
	   (setq l (update-node l 0 (avl-left l) node))
	   (values l (1- height-change)))
	  ((cl:zerop (avl-balance l))
	   (setq node (update-node node -1 (avl-right l) (avl-right node)))
	   (setq l (update-node l 1 (avl-left l) node))
	   (values l height-change))
	  (t (let ((rl (avl-right l)))
	       (setq l (update-node l (if (cl:plusp (avl-balance rl)) -1 0)
				    (avl-left l) (avl-left rl)))
	       (setq node (update-node node (if (cl:minusp (avl-balance rl))
						1 0)
				       (avl-right rl) (avl-right node)))
	       (setq rl (update-node rl 0 l node))
	       (values rl (1- height-change)))))))

(defgeneric insert (item tree &rest args)
  (:documentation
   "The first interesting operation on AVL trees. This inserts THING
into the tree and returns a new tree and an integer which is the
change in height of the tree."))

(defmethod insert (item (tree avl-tree) &rest args)
  (labels
      ((avl-insert (node)
	 (if (null node)
	     (values (%apply #'make-element tree item args) 1)
	     (cond ((= item node)
		    (values ;; Just update the value field if necessary
                     (%apply #'update-node node (avl-balance node)
			     (avl-left node) (avl-right node)
			     args)
                     0))
		   ((> item node) 
		    (multiple-value-bind (subtree height-change)
			(avl-insert (avl-right node)) 
		      (setq node 
			    (update-node node
					 (cl:+ (avl-balance node)
                                               height-change)
					 (avl-left node) subtree))
		      (if (cl:> (avl-balance node) 1)
			  (balance-right node 1)
			  (values node (if (cl:plusp (avl-balance node))
					   height-change
					   0)))))
		   (t (multiple-value-bind (subtree height-change)
			  (avl-insert (avl-left node))
			(setq node
			      (update-node node
					   (cl:- (avl-balance node)
                                                 height-change)
					   subtree
					   (avl-right node)))
			(if (cl:< (avl-balance node) -1)
			    (balance-left node 1)
			    (values node
				    (if (cl:minusp (avl-balance node))
					height-change
					0)))))))))
    (setf (avl-root tree) (avl-insert (avl-root tree)))
    tree))

(defgeneric delete-head (tree)
  (:documentation
   "This returns the head (leftmost element) in the tree, and removes
it from the tree. Useful for implementing priority queues as AVL
trees. Values returned are the value of the leftmost element, the
modified tree, and the change in height of the tree."))

(defmethod delete-head ((tree avl-tree))  
  (multiple-value-bind (tail new-root height-change)
      (delete-head (avl-root tree))
    (setf (avl-root tree) new-root)
    (values tail height-change)))

(defmethod delete-head ((node avl-node))
  (cond ((null node) nil)
	((null (avl-left node))
	 (values node (avl-right node) -1))
	(t (multiple-value-bind (head-value subnode height-change)
	       (delete-head (avl-left node))
	     (setq node (update-node node (cl:- (avl-balance node)
                                                height-change)
				     subnode (avl-right node)))
	     (if (> (avl-balance node) 1)
		 (multiple-value-setq (node height-change)
		   (balance-right node 0))
		 (if (not (cl:zerop (avl-balance node)))
		     (setq height-change 0)))
	     (values head-value node height-change))))) 

(defgeneric delete-tail (tree)
  (:documentation
   "This returns the tail (rightmost element) in the tree, and removes
it from the tree.  Values returned are the value of the rightmost
element, the modified tree, and the change in height of the tree."))

(defmethod delete-tail ((tree avl-tree))
  (multiple-value-bind (tail new-root height-change)
      (delete-tail (avl-root tree))
    (setf (avl-root tree) new-root)
    (values tail height-change)))

(defmethod delete-tail ((node avl-node))
  (cond ((null node) nil)
	((null (avl-right node))
	 (values node (avl-left node) -1))
	(t (multiple-value-bind (tail-value subnode height-change)
	       (delete-tail (avl-right node))
	     (setq node (update-node node (cl:+ (avl-balance node)
                                                height-change)
				     (avl-left node) subnode))
	     (if (cl:< (avl-balance node) -1)
		 (multiple-value-setq (node height-change)
		   (balance-left node 0))
		 (if (not (cl:zerop (avl-balance node)))
		     (setq height-change 0)))
	     (values tail-value node height-change)))))

(defgeneric erase-node (node)
  (:documentation
   "This gets rid of a value that has been found in the tree. NODE is
the node containing the value. If the right subtree of NODE is higher
than its left, replace the value of NODE with the value of the
left-most leaf of the right subtree, and remove this leaf from the
right subtree. Otherwise replace NODE's value with the value of the
right-most leaf of the left subtree of NODE, and remove this leaf from
the left subtree."))

(defmethod erase-node ((node avl-node))
  (cond ((and (null (avl-left node)) (null (avl-right node)))
	 (values nil -1))
	((cl:plusp (avl-balance node))
	 (multiple-value-bind (head-node subtree height-change)
	     (delete-head (avl-right node))
	   (setq node (update-node head-node
				   (cl:+ (avl-balance node)
                                         height-change)
				   (avl-left node) subtree))
	   (values node height-change)))
	(t (multiple-value-bind (tail-node subtree height-change)
	       (delete-tail (avl-left node))
	     (setq node (update-node tail-node
				     (cl:- (avl-balance node) height-change)
                                     subtree (avl-right node)))
	     (values node (if (cl:zerop (avl-balance node))
			      height-change 0))))))

(defmethod delete (item (tree avl-tree) &rest rest)
  "This deletes an entry from an AVL tree."
  (declare (ignore rest))
  (let ((root (avl-root tree)))
    (labels
        ((delete-left (node parent)
           (cond ((null node)
                  (values nil 0))
                 ((= item node)
                  (multiple-value-bind (new-left height-change) (erase-node node)
                    (setf (avl-left parent) new-left)
                    (values new-left height-change)))
                 (t (avl-delete node))))
         (delete-right (node parent)
           (cond ((null node)
                  (values nil 0))
                 ((= item node)
                  (multiple-value-bind (new-right height-change)
                      (erase-node node)
                    (setf (avl-right parent) new-right)
                    (values new-right height-change)))
                 (t (avl-delete node))))
         (avl-delete (node)
           (cond ((> item node)
                  (multiple-value-bind (subtree height-change)
                      (delete-right (avl-right node) node)
                    (setq node (update-node node
                                            (cl:+ (avl-balance node)
						  height-change)
                                            (avl-left node) subtree))
                    (if (cl:< (avl-balance node) -1)
                        (balance-left node 0)
                        (values node (if (cl:zerop (avl-balance node))
                                         height-change 0)))))
                 (t (multiple-value-bind (subtree height-change)
                        (delete-left (avl-left node) node)
                      (setq node (update-node node
                                              (cl:- (avl-balance node)
						    height-change)
                                              subtree (avl-right node)))
                      (if (cl:> (avl-balance node) 1)
                          (balance-right node 0)
                          (values node (if (cl:zerop (avl-balance node))
                                           height-change 0))))))))
      (cond ((null root)
	     (values nil 0))
	    ((= item root)
	     (setf (avl-root tree) (erase-node root)))
	    ((> item root)
	     (delete-right (avl-right root) root))
	    (t (delete-left (avl-left root) root)))
      tree)))

(defmethod member (item (tree avl-tree) &rest rest)
  (declare (ignore rest))
  (labels ((search-node (node)
	     (cond ((null node) nil)
		   ((= item node) node)
		   ((> item node)
		    (search-node (avl-right node)))
		   (t (search-node (avl-left node))))))
    (search-node (avl-root tree))))

(defmethod make-element ((tree avl-tree-elements-as-singletons) key &rest rest)
  (declare (ignore rest))
  (make-instance 'avl-node :domain tree :key key))

(defmethod make-element ((tree avl-tree-elements-as-pairs) key &rest rest)
  (make-instance 'avl-node-as-pairs :domain tree :key key :value (first rest)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*- 
;;; ===========================================================================
;;;				 Lisp Numbers 
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; lisp-numbers.lisp,v 1.6 1995/05/24 17:42:03 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; Contains a bunch of routines that allow one to use the Weyl generic
;; functions on Lisp numbers.  There are a number of special
;; arithmetic routines here also.  This is in not in LISP-SUPPORT
;; because it is more convenient to put write this code in the WEYLI
;; package. 

;; The Lisp number domain is a special field whose elements are
;; represented as Lisp numbers.  This class is unique.

(defgeneric numerator (number)
  (:documentation
   "Return the numerator of the number.")
  (:method ((number integer)) number)
  (:method ((number ratio)) (common-lisp:numerator number)))

(defgeneric denominator (number)
  (:documentation
   "Return the denominator of the number.")
  (:method ((number integer)) 1)
  (:method ((number ratio)) (common-lisp:denominator number)))

(defgeneric factorial (number)
  (:documentation
   "Return the factorial of the number."))

(defmethod factorial ((n integer))
  (labels ((fact (n)
	     (if (cl:< n 2) 1
		 (cl:* n (fact (cl:1- n))))))
    (if (cl:minusp n)
	(error "Illegal argument to factorial: ~D" n)
	(fact n))))

(defgeneric pochhammer (number1 number2)
  (:documentation
   "Return the falling, or lower, factorial."))

(defmethod pochhammer ((n integer) (k integer))
  (cond ((cl:minusp k)
	 (error "Illegal arguments to Pochhammer: (~D, ~D)"
		n k))
	((cl:zerop k) 1)
	(t (let ((ans n))
	     (loop for i upfrom 1 below k
		   do (setq ans (cl:* ans (cl:+ n i))))
	     ans))))

(defgeneric combinations (number1 number2)
  (:documentation
   "Return a combination of the upper and lower factorial.")
  (:method ((number1 integer) (number2 integer))
    (common-lisp:/ (pochhammer (+  number1 (- number2) 1) number2) (factorial number2))))

(defun faster-isqrt (n)
  "Argument n must be a non-negative integer"
  (let (n-len-quarter n-half n-half-isqrt init-value q r iterated-value)
    (cond
      ((> n 24)
       ;; theoretically (> n 7) ,i.e., n-len-quarter > 0
       (setq n-len-quarter (ash (integer-length n) -2))
       (setq n-half (ash n (- (ash n-len-quarter 1))))
       (setq n-half-isqrt (faster-isqrt n-half))
       (setq init-value (ash (1+ n-half-isqrt) n-len-quarter))
       (multiple-value-setq (q r) (cl:floor n init-value))
       (setq iterated-value (ash (+ init-value q) -1))
       (if (eq (logbitp 0 q) (logbitp 0 init-value)) ; same sign
	   ;; average is exact and we need to test the result
	   (let ((m (- iterated-value init-value)))
	     (if (> (* m m) r)
		 (- iterated-value 1)
		 iterated-value))
	   ;; average was not exact, we take value
	   iterated-value))
      ((> n 15) 4)
      ((> n  8) 3)
      ((> n  3) 2)
      ((> n  0) 1)
      ((> n -1) 0)
      (t nil))))

(defun integer-nth-root (x n)
  (cond ((cl:zerop x) x)
	((cl:plusp x)
	 (let ((n-1 (cl:- n 1))
	       (root (ash 1 (cl:truncate (integer-length x) n)))
	       new-root)
	   (loop for root^n-1 = (cl:expt root n-1)
		 do (setq new-root
			  (cl:round (cl:+ (cl:* n-1 root root^n-1) x)
                                    (* n root^n-1)))
                 (if (cl:= new-root root)
                     (return new-root)
                     (setq root new-root)))))
	((oddp n)
	 (- (integer-nth-root (cl:- x) n)))
	(t nil)))

;;; FIXME : Move the prime number related routines to a separate file
;;; and implement recent algorithms.

(defvar *pointbound* 2147483629 
  "Should be largest prime that fits in a word")

(defvar *big-primes* ()
  "List of large primes by decending size")

;; These two really should be in GFP, but because of LUCID brain damage,
;; they have to be here to avoid warnings.

(defun reduce-modulo-integer (value modulus)
  (unless (cl:zerop modulus)
    (setq value (cl:rem value modulus)))
  (if (cl:< value 0) (cl:+ value modulus)
      value))

(defun expt-modulo-integer (base expt modulus)  
  (%funcall (repeated-squaring
             #'(lambda (a b) (reduce-modulo-integer (cl:* a b) modulus))
             1)
            base expt)) 

(defgeneric prime? (number)
  (:documentation
   "Return true if the number is prime."))

(defmethod prime? ((p integer))
  (and (cl:> p 1)
       (or (cl:< p 14.)
	   (and (cl:= 1 (expt-modulo-integer 13. (1- p) p))
		(cl:= 1 (expt-modulo-integer 3 (1- p) p))))
       (null (cdr (setq p (factor p))))
       (cl:= 1 (cdar p))))

;; Rabin's probabilistic primality algorithm isn't used here because it
;; isn't much faster than the simple one for numbers about the size of a
;; word.
(defun find-smaller-prime (p)
  "Finds biggest prime less than fixnum p"
  (if (evenp p) (setq p (1- p)))
  (loop for pp = (cl:- p 2) then (cl:- pp 2) until (cl:< pp 0)
	when (prime? pp)
        do (return pp)))

;; Return the next prime less than its argument, and that fits into a
;; word.  
(defun newprime (&optional p)
  (if (null p) *pointbound*
      (do ((pl *big-primes* (cdr pl)))
          ((null pl) (setq p (find-smaller-prime p))
           (setq *big-primes* (nconc *big-primes* (list p)))
           p)
        (if (cl:< (car pl) p) (return (car pl))))))

;; Computes a list of primes whose product is greater than the given limit.
(defun choice-primes (limit &optional
                      (prime-list
                       (list (find-smaller-prime
                              most-positive-fixnum))))
  (let ((p (car prime-list)))
    (if (< limit p)
        prime-list
        (choice-primes (ceiling limit p)
                       (cons (newprime p) prime-list)))))

;; Computes (mod a b) symmetric around 0.  a and b are assumed to be
;; lisp integers.
(defun sym-mod (a b)
  (let* ((b (cl:abs b))
	 (c (cl:mod a b)))
    (if (cl:> c (cl:floor (cl:/ b 2)))
        (cl:- c b)
        c)))

(defun repeated-squaring (mult one)
  (lambda (base exp)
    (if (cl:zerop exp) one
        (let ((prod one))
          (loop
           (if (oddp exp)
               (setq prod (%funcall mult prod base)))
           (setq exp (cl:truncate exp 2))
           (if (cl:zerop exp)
               (return prod))
           (setq base (%funcall mult base base)))))))

(defgeneric power-of? (number &optional opt-number)
  (:documentation
   "Return true if number is a power of opt-number."))

(defmethod power-of? ((m integer) &optional n)
  (cond ((typep n 'integer)
	 (loop for test = n then (cl:* test n)
	       for i upfrom 1
	       do (cond ((cl:= test m)
			 (return (values n i)))
			((cl:> test m)
			 (return nil)))))
	(t (error "Haven't implemented the rest of the cases"))))

(defvar *factor-method* 'simple-integer-factor)

(defmacro count-multiple-integer-factors (N divisor)
  `(loop with i = 0
    do (multiple-value-bind (quo rem) (cl:truncate ,N ,divisor)
         (when (not (cl:zerop rem))
           (if (not (cl:zerop i))
               (push (cons ,divisor i) ans))
           (return t))
         (setq ,N quo)
         (incf i))))

(defun uniformize-factor-list (ans)
  (loop for pairs on (sort ans #'(lambda (a b) (< (first a) (first b))))
	when (or (null (rest pairs))
		 (not (cl:= (first (first pairs))
			    (first (second pairs)))))
        collect (first pairs)
	else do (incf (rest (second pairs)))))

(defgeneric factor (number)
  (:documentation
   "Return the factors of the number."))

(defmethod factor ((N integer))
  (let ((*factor-method* *factor-method*)
	ans factors)
    (when (cl:minusp N)
      (push (cons -1 1) ans)
      (setq N (cl:- N)))
    (count-multiple-integer-factors N 2)
    (count-multiple-integer-factors N 3)
    (count-multiple-integer-factors N 5)
    (unless (cl:= N 1)
      (loop
	(multiple-value-setq (N factors) (%funcall *factor-method* N))
	(setq ans (append factors ans))
	(if (cl:= N 1) (return t))))
    (uniformize-factor-list ans)))

(defun all-divisors (n)
  (let ((factors (factor n)))
    (loop with divisors = (list 1)
	  for (prime . times) in factors
	  do (loop for i from 1 to times
		   appending (loop for divisor in divisors
				   collect (* divisor (cl:expt prime i)))
                   into temp
		   finally (setq divisors (append temp divisors)))
          finally (return (sort divisors #'cl:<)))))

;; In general each factorization method should return just one factor.

(defvar *skip-chain-for-3-and-5* (circular-list 4 2 4 2 4 6 2 6))

(defun simple-integer-factor (N)
  (let ((increments *skip-chain-for-3-and-5*)
	(divisor 7)
	ans)
    (flet ((simple-integer-factor-internal (N)
	     (let ((limit (cl:isqrt N)))
	       (loop 
                (cond ((cl:= N 1)
                       (return (values N ans)))
                      ((cl:> divisor limit)
                       (return (values 1 (cons (cons N 1) ans))))
                      (t (count-multiple-integer-factors N divisor)))
                (setq divisor (cl:+ divisor (pop increments)))))))      
      (setq *factor-method* #'simple-integer-factor-internal)
      (simple-integer-factor-internal N))))

(defun fermat-integer-factor (N)
  (loop for x = (1+ (cl:isqrt N)) then (+ x 1)
	for w = (cl:- (cl:* x x) N)
	for y = (cl:isqrt w)
	do (when (cl:zerop (cl:- w (cl:* y y)))
	     (let ((u (cl:+ x y))
		   (v (cl:- x y)))
	       (return (if (1? v)
			   (values 1 (list (cons u 1)))
			   (values u (factor v))))))))
	
#| Knuth's addition-subtraction version of Fermat's algorithm |

(defun fermat-integer-factor (N)
  (let* ((isqrt-N (cl:isqrt N))
	 (x (1+ (* 2 isqrt-N)))
	 (y 1)
	 (r (- (* isqrt-N isqrt-N) N)))
    (loop
      (cond ((= r 0)
	     (return 
	       (let ((f (/ (+ x y -2) 2))
		     (g (/ (- x y) 2)))
		 (if (= g 1)
		     (values 1 (list (cons f 1)))
		     (values 1 (append (factor f) (factor g)))))))
	    ((< r 0)
	     (incf r x)
	     (incf x 2)))
      (decf r y)
      (incf y 2))))

(defun list-of-primes (N)
  (cons 2
	(loop for p upfrom 3 by 2 below N
	      when (prime? p) collect p)))

(defun make-integer-GCD-list (max-prime size-limit)
  (let ((GCD-list ()))
    (loop for p in (list-of-primes max-prime)
	  with prod = 1 and prime-list = ()
	  do (setq prod (* prod p))
	     (cond ((> prod size-limit)
		    (push (list (/ prod p) prime-list)
			  GCD-list)
		    (setq prod p)
		    (setq prime-list (list p)))
		   (t (push p prime-list))))
    GCD-list))

	    
||#

(defun totient (x)
  (do ((factors (factor x) (rest factors))
       (totient 1 (cl:* totient
                        (cl:- (cl:expt (caar factors) (cdar factors))
                              (cl:expt (caar factors) (1- (cdar factors)))))))
      ((null factors)
       totient)))

(defgeneric sin (number)
  (:documentation
   "Return the sine of the number.")
  (:method ((number number)) (common-lisp:sin number)))

(defgeneric cos (number)
  (:documentation
   "Return the cosine of the number.")
  (:method ((number number)) (common-lisp:cos number)))

(defgeneric tan (number)
  (:documentation
   "Return the tangent of the number.")
  (:method ((number number)) (common-lisp:tan number)))

(defgeneric asin (number)
  (:documentation
   "Return the arc-sine of the number.")
  (:method ((number number)) (common-lisp:asin number)))

(defgeneric acos (number)
  (:documentation
   "Return the arc-cosine of the number.")
  (:method ((number number)) (common-lisp:acos number)))

(defgeneric atan (number1 &optional number2)
  (:documentation
   "Return the arc-tangent of the number1 or optionally
number1/number2."))

;;; FIXME : This is a simple example of the motivation for predicate
;;; dispatching as described in AITR-2001-006.
(defmethod atan ((number1 number) &optional number2)
  (cond
    ((null number2)
     (common-lisp:atan number1))
    ((numberp number2)
     (common-lisp:atan number1 number2))
    (t (atan (coerce number1 (domain-of number2)) number2))))

(defgeneric sinh (number)
  (:documentation
   "Return the hyperbolic sine of the number.")
  (:method ((number number)) (common-lisp:sinh number)))

(defgeneric cosh (number)
  (:documentation
   "Return the hyperbolic cosine of the number.")
  (:method ((number number)) (common-lisp:cosh number)))

(defgeneric tanh (number)
  (:documentation
   "Return the hyperbolic tangent of the number.")
  (:method ((number number)) (common-lisp:tanh number)))

(defgeneric asinh (number)
  (:documentation
   "Return the hyperbolic arc sine of the number.")
  (:method ((number number)) (common-lisp:asinh number)))

(defgeneric acosh (number)
  (:documentation
   "Return the hyperbolic arc cosine of the number.")
  (:method ((number number)) (common-lisp:acosh number)))

(defgeneric atanh (number)
  (:documentation
   "Return the hyperbolic arc tangent of the number.")
  (:method ((number number)) (common-lisp:atanh number)))

(defgeneric exp (number)
  (:documentation
   "Return the exponential of the number.")
  (:method ((number number)) (common-lisp:exp number)))

;;; FIXME : The functions log2 and log need to be merged. This
;;; provides another simple example of the motivation for predicate
;;; dispatching as described in AITR-2001-006.
(defgeneric log2 (number base)
  (:documentation
   "Return the base logarithm of the number.")
  (:method ((number number) (base number)) (common-lisp:log number base)))

(defgeneric log (number)
  (:documentation
   "Return the natural logarithm of the number.")
  (:method ((number number)) (common-lisp:log number)))

(defgeneric signum (number)
  (:documentation
   "Returns a numerical value that indicates whether number is
negative, zero or positive.")
  (:method ((number number)) (common-lisp:signum number)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			           Sets 
;;; ===========================================================================
;;; (c) Copyright 1989, 1991 Cornell University

;;; sets.lisp,v 1.6 1995/05/24 17:42:11 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;; Tuples are just indexed lists.
(defclass tuple ()
  ((value
    :initarg :values
    :initform ()
    :reader value-of)))

;;; DELETE : This is a major update, so we're not concerned with
;;; backward compatibility.
(defgeneric tuple-value (tuple)
  (:documentation
   "A wrapper function for value-of for backward compatibility.")
  (:method ((tuple tuple)) (value-of tuple)))

(defmethod initialize-instance :after ((object tuple) &rest plist)
  (declare (ignore plist))
  (with-slots (value) object
    (cond ((null value)
	   (error "Values need to be provided for ~A"
		  (class-name (class-of object))))
	  ((listp value)
	   (setq value (%apply #'vector value))))))
	      
(defmethod print-object ((tuple tuple) stream)
  (%apply #'format stream "<~S~@{, ~S~}>"
          (loop with v = (value-of tuple)
                for i below (array-dimension v 0)
                collect (aref v i))))

(defgeneric ref (tuple &rest args)
  (:documentation
   "Refernce the item of tuple specified by the first arg.")
  (:method ((tuple tuple) &rest args)
    (aref (value-of tuple) (first args))))

;;; FIXME : Merge set-ref and (defsetf ref).
(defgeneric set-ref (tuple new-value &rest args)
  (:documentation
   "Set the element specified by args to new-value."))

(defmethod set-ref ((tuple tuple) new-value &rest args)
  (setf (aref (value-of tuple) (first args)) new-value))

(defsetf ref (vect &rest indices) (new-value)
  `(set-ref ,vect ,new-value ,@indices))

(defgeneric list-of-elements (tuple)
  (:documentation
   "Return a list of the tuple elements."))

(defmethod list-of-elements ((tuple tuple))
  (let ((array (value-of tuple)))
    (loop for i fixnum below (array-dimension array 0)
	  collect (aref array i))))

(defmethod map (type function (seq tuple) &rest sequences)
  (setq type (cond ((null type) (class-of seq))
		   ((typep type 'class) type)
		   ((typep type 'symbol) (find-class type))
		   (t (error "Bad type specifier: ~S" type))))
  (let ((values (loop with v = (value-of seq)
		      for i below (array-dimension v 0)
		      collect (%apply function (aref v i)
				      (loop for seq in sequences
					    collect (ref seq i))))))
    (if (subclass-of? 'domain-element type)
        (if (typep seq 'domain-element)
            (make-instance type :domain (domain-of seq)
                           :values values)
            (error "Can't determine domain of resulting sequence"))
        (make-instance type :values values))))

(defgeneric map-with-domain (type domain function sequence &rest sequences)
  (:documentation
   "Map the values of the sequences into the domain."))

(defmethod map-with-domain (type domain function (seq tuple) &rest sequences)
  (make-instance
   type :domain domain
   :values
   (loop with v = (value-of seq)
         for i below (array-dimension v 0)
         collect (%apply function (aref v i)
                         (loop for seq in sequences
                               collect (ref seq i))))))

;; (empty? set)
;; (insert key set &rest args)
;; (delete item set &rest args)
;; (member item set &rest args) 
;; (map-over-elements set function)
;; (make-generator set) -> function

(define-operations set
  (= (element self) (element self)) -> Boolean
  (coerce default self) -> (element self)
  (member (element self) self) -> Boolean
  (make-generator self) -> (-> (element self))
  (print-object (element self) stream) -> Null
  (number-of-elements self) -> integer)

;; Default version of this...
#+ignore
(defmethod binary= (x y) (equal x y))

(define-operations mutable-set
  (insert (element self) self) -> Null
  (delete (element self) self) -> Null)

#+IGNORE
(define-operations ordered-set
  (< (element self) (element self)) -> Boolean
  (> (element self) (element self)) -> Boolean
  (max (element self) (element self)) -> (element self)
  (min (element self) (element self)) -> (element self))

#+IGNORE
(defmethod initialize-method :after ((set ordered-set) &rest plist)
  (unless (%getf plist :compare-function)
    (error "Must provide a comparison function for ordered sets")))

(define-operations finite-set
  (size self) -> Integer
  (random self) -> (element self))

;; Set elements are also objects in Weyl.  They behave like
;; domain-elements (they are domain-elements).  The function
;; (element-key ..) gets their key.  They are two basic types of
;; set-elements.  SET-ELEMENT1 is a class where the elements are the
;; keys themselves.  There are many applications where we want to have
;; sets of pairs (key, value).  The class SET-ELEMENT2 is used for this
;; purpose.  Set-elements can be compared with =, and > in which case
;; the comparison will use the comparision function of the set.

;;When building more complex structures, (AVL trees etc.) the nodes of
;;the datastructures should be built out of these classes.

(defmethod print-object ((element set-element) stream)
  (format stream "~S" (element-key element)))

;; This is used for sets whose elements are associated with a value.

(defmethod print-object ((element set-element2) stream)
  (format stream "(~S, ~S)" (element-key element) (element-value element)))

(defmethod-sd binary= ((e1 set-element) (e2 set-element))
  (%funcall (equal-function-of domain) (element-key e1) (element-key e2)))

(defmethod binary= ((e1 set-element) e2)
  (%funcall (equal-function-of (domain-of e1)) (element-key e1) e2))

;; The following needs to be an around method so that it doesn't come
;; at the end of the precidence list (after (number domain-element)
;; defined in morphisms).
(defmethod binary= :around (e1 (e2 set-element))
  (%funcall (equal-function-of (domain-of e2)) e1 (element-key e2)))

(defmethod-sd binary> ((e1 set-element) (e2 set-element))
  (%funcall (greater-function-of domain) (element-key e1) (element-key e2)))

(defmethod binary> ((e1 set-element) e2)
  (%funcall (greater-function-of (domain-of e1)) (element-key e1) e2))

;; The following needs to be an around method so that it doesn't come
;; at the end of the precidence list (after (number domain-element)
;; defined in morphisms).
(defmethod binary> :around (e1 (e2 set-element))
  (%funcall (greater-function-of (domain-of e2)) e1 (element-key e2)))

;; In building real sets one should include one of these classes to
;; indicate how elements of the set will be represented.

(defmethod make-element ((set set-elements-as-singletons) key &rest rest)
  (declare (ignore rest))
  (make-instance 'set-element1 :domain set :key key))

(defmethod make-element ((set set-elements-as-pairs) key &rest rest)
  (make-instance 'set-element2 :domain set
		 :key key :value (first rest)))

;; Here are some simple sets that we might use in a program.

(defgeneric set-elements (set)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((set set-with-element-list))
    (rest (set-element-list set))))

(defun set-with-element-list-print-object (set stream)
  (let ((elts (set-elements set)))
    (if (null elts) (princ "{}" stream)
	(format stream "{~S~{, ~S~}}" (first elts) (rest elts)))))

(defmethod initialize-instance :after ((set set-with-element-list) &rest plist)
  (let ((initial-elements (loop for (item . args) in (%getf plist :initial-elements)
				collect (%apply #'make-element set item args))))
    #+IGNORE
    (if (typep set 'ordered-set)
	(setf initial-elements (sort initial-elements #'binary>)))
    (setf (rest (set-element-list set)) initial-elements)
    (with-slots (print-function) set
      (setf print-function 'set-with-element-list-print-object))))

(defmethod insert (key (set mutable-set-with-element-list) &rest rest)
  (let ((list (set-element-list set)))
    (loop for elt in (rest list)
	  when (= key elt)
          do (return set)
	  finally (push (%apply #'make-element set key rest) (rest list))
          (return set))))

(defmethod delete (item (set mutable-set-with-element-list) &rest args)
  (declare (ignore args))
  (flet ((not-an-element ()
	   (error "~S is not an element of ~S" item set)))
    (loop for elts on (set-element-list set)
          when (null (rest elts))
          do (not-an-element)
          when (= item (second elts))
          do (setf (rest elts) (rest (rest elts)))
          (return set)
          finally (not-an-element))))

(defmethod member (key (set set-with-element-list) &rest args)
  (declare (ignore args))
  (loop for elt in (set-elements set)
	when (= key elt)
        do (return elt)
	finally (return nil)))

(defgeneric map-over-elements (set function)
  (:documentation
   "Map over the elements of the set applying the function."))

(defmethod map-over-elements ((set set-with-element-list) function)
  (loop for elt in (set-elements set) do
        (%funcall function elt)))

(defmethod make-generator ((set set-with-element-list))
  (let ((list (set-elements set)))
    (lambda () (pop list))))

;; This is just a variant on the previous class.  The inclusion of the
;; ordered-set class causes the initialize-instance method to put the
;; elements in the set ordered.

(defmethod insert (key (set mutable-set-with-sorted-element-list) &rest rest)
  (loop for elts on (set-element-list set) do
        (cond ((or (null (rest elts)) (> key (second elts)))
               (setf (rest elts) (cons (%apply #'make-element set key rest) (rest elts)))
               (return set))
              ((= key (second elts))
               (return set)))))

(defmethod delete (item (set mutable-set-with-sorted-element-list) &rest args)
  (declare (ignore args))
  (flet ((not-an-element ()
	   (error "~S is not an element of ~S" item set)))
    (loop for elts on (set-element-list set)
          when (null (rest elts))
          do (not-an-element)
          when (= item (second elts))
          do (setf (rest elts) (rest (rest elts)))
          (return set)
          when (> item (second elts))
          do (not-an-element)
          finally (not-an-element))))

(defmethod member (key (set set-with-sorted-element-list) &rest args)
  (declare (ignore args))
  (loop for elt in (set-elements set)
	when (= key elt)
        do (return elt)
	when (> key elt)
        do (return nil)
	finally (return nil)))

;;; FIXME : It would be better to define this as length.
(defgeneric size (set)
  (:documentation
   "Return the length of the set.")
  (:method ((set set-with-element-list))
    (common-lisp:length (set-elements set))))

(defgeneric random (set &optional height)
  (:documentation
   "Return a random element of the list."))

(defmethod random ((set set-with-element-list) &optional height)
  (declare (ignore height))
  (let ((l (set-elements set)))
    (nth (cl:random (length l)) l)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Morphisms
;;; ===========================================================================
;;; (c) Copyright 1989, 1991 Cornell University

;;; morphisms.lisp,v 1.7 1994/10/21 18:16:43 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmethod print-object ((homo morphism) stream)
  (format stream
	  #+Genera "~S~S"
	  #-Genera "~S->~S"
	  (morphism-domain homo) (morphism-range homo)))

(defvar *morphism-composition-table*
    (make-hash-table))

(defgeneric compose (morphism1 morphism2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod compose ((f homomorphism) (g homomorphism))
  (let ((cache (gethash f *morphism-composition-table*))
	comp)
    (cond ((setq comp (assoc g cache))
	   (second comp))
	  ((eql (morphism-range f) (morphism-domain g))
	   (setq comp
		 (make-instance
                  'homomorphism
                  :domain (morphism-domain f)
                  :map (lambda (x)
                         (%funcall (morphism-map g)
                                   (%funcall (morphism-map f) x)))
                  :range (morphism-range g)))
	   (push (list g comp) (gethash f *morphism-composition-table*))
	   comp)
	  (t (error "Incompatible homomorphisms: ~S o ~S"
		    f g)))))

(defun get-morphisms (&key type domain range direct?)
  (let (morphisms)
    (labels ((get-morphisms-from (d predecessor)
	       (loop for morph in (domain-morphisms-from d) do
                     (when (or (null type) (typep morph type))
                       (cond ((eql range (morphism-range morph))
                              (push (if predecessor
                                        (compose predecessor morph)
                                        morph)
                                    morphisms))
                             (t
                              (setq morph (if predecessor
                                              (compose predecessor morph)
                                              morph))
                              (when (null range)
                                (push morph morphisms))
                              (when (null direct?)
                                (get-morphisms-from (morphism-range morph)
                                                    morph)))))))
	     (get-morphisms-to (d successor)
	       ;; We know that the range is null here, otherwise we
	       ;; would have called GET-MORPHISMS-FROM
	       (loop for morph in (domain-morphisms-to d) do
                     (when (or (null type) (typep morph type))
                       (setq morph (if successor
                                       (compose morph successor)
                                       morph))
                       (push morph morphisms)
                       (when (null direct?)
                         (get-morphisms-to (morphism-domain morph) morph))))))
      (cond (domain
	     (get-morphisms-from domain nil))
	    (range
	     (get-morphisms-to range nil))
	    (t (loop for d in *domains*
		     do (get-morphisms-to d nil))))
      morphisms)))

(defun make-morphism (domain map range &key (replace? t))
  (let ((old-h (get-morphisms :domain domain :range range))
	(h (make-instance 'morphism :domain domain :map map :range range)))
    (when replace?
      (loop for morph in old-h
	    do (delete-morphism morph :error? t)))
    (push h (domain-morphisms-from domain))    
    (push h (domain-morphisms-to range))
    h))

(defgeneric delete-morphism (morphism)
  (:documentation
   "Delete the morphism."))

(defmethod delete-morphism ((morph morphism))
  (let ((domain (morphism-domain morph))
	(range (morphism-range morph)))
    (setf (domain-morphisms-from domain)
	  (delete morph (domain-morphisms-from domain)))    
    (setf (domain-morphisms-to range)
	  (delete morph (domain-morphisms-to range)))))

;;; Homomorphisms

(defun make-homomorphism (domain map range)
  (let ((h (make-instance 'homomorphism :domain domain :map map :range range)))
    (push h (domain-morphisms-from domain))
    (push h (domain-morphisms-to range))
    h))

(defun get-homomorphisms (&key domain range)
  (get-morphisms :type 'homomorphism :domain domain :range range))

(defun get-embeddable-domains (domain)
  (let ((domains ()))
    (loop for homo in (domain-morphisms-from domain)
	  do (when (typep homo 'homomorphism)
	       (push (morphism-range homo) domains)))
    domains))

(defun make-automorphism (domain map &optional range)
  (declare (ignore range))
  (make-homomorphism domain map domain))

(defun get-automorphisms (&key domain)
  (get-morphisms :type 'automorphism :domain domain :range domain))

;;; Operations with morphisms
(defgeneric apply-morphism (morphism argument)
  (:documentation
   "Apply the morphism to the argument."))

(defmethod apply-morphism ((h morphism) argument)
  (when (eql (domain-of argument) (morphism-domain h))
    (%funcall (morphism-map h) argument)))

(defgeneric canonicalize-number (elt)
  (:documentation
   "Converts a LISP number into a a Weyl number in the appropriate
canonical domain. These guys CANNOT use coerce!!!!"))

(defmethod canonicalize-number ((num integer))
  (make-element (get-rational-integers) num))

(defmethod canonicalize-number ((num rational-integer))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-element (get-rational-integers) (integer-value num)))
	(t num)))

(defmethod canonicalize-number ((num ratio))
  (make-instance 'rational-number :domain (get-rational-numbers)
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod canonicalize-number ((num rational-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-instance 'rational-number :domain (get-rational-numbers)
			:numerator (qo-numerator num)
			:denominator (qo-denominator num)))
	(t num)))

(defmethod canonicalize-number ((num float))
  (make-instance 'floating-point-number
		 :domain (get-real-numbers)
		 :value num))

(defmethod canonicalize-number ((num floating-point-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-instance 'floating-point-number
			:domain (get-real-numbers)
			:value (fp-value num)))
	(t num)))

(defmethod canonicalize-number ((num bigfloat))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-bigfloat (get-real-numbers)
			(bigfloat-mantissa num)
			(bigfloat-exponent num)))
	(t num)))

(defmethod canonicalize-number ((num cl:complex))
  (make-element (get-complex-numbers)
		(cl:realpart num)
		(cl:imagpart num)))

(defmethod canonicalize-number ((num complex-number))
  (cond ((typep (domain-of num) 'general-expressions)
	 (make-element (get-complex-numbers)
		       (cn-realpart num)
		       (cn-imagpart num)))
	(t num)))

(defmethod canonicalize-number ((num GFp-element)) num)

(defmethod canonicalize-number ((num GFm-element)) num)

;;; This is what allows lisp numbers to be used relatively freely.
;;; Unfortunately, this introduces more consing than would be idea.
(defmethod apply-morphism ((map morphism) (elt number))
  (apply-morphism map (canonicalize-number elt)))

;;; Support for COERCIONS

(defun reachable-domains (domain)
  (flet ((next-domains (x)
	   (loop for m in (domain-morphisms-from x)
		 collect (morphism-range m))))
    (let (domains)
      (map-over-tree d (domain :depth-first? t
                               :collection-fun next-domains)
        (push d domains))
      domains)))

(defun find-common-domains (d1 d2)
  (flet ((next-domains (x)
	   (loop for m in (domain-morphisms-from x)
		 collect (morphism-range m))))
    (let ((d1-domains (reachable-domains d1))
	  domains)
      (map-over-tree d (d2 :breadth-first? t
			   :collection-fun next-domains)
	(when (member d d1-domains)
	  (pushnew d domains)
	  (terminate-branch)))
      domains)))

;;; FIXME : Common Lisp does have an error system, now.
;;;
;;; Since we don't have an error system in Common Lisp yet, we use the
;;; following flag to control whether an error is generated or NIL is
;;; returned from COERCE.

(defvar *coercibility-checking* nil)

;;; This method provides the the default homorphism coercions
(defmethod coerce (elt (domain domain))
  (let (homos)
    (cond ((null (typep elt 'domain-element))
	   (unless *coercibility-checking*
	     (error "Don't know how to coerce ~S to be an element of ~S"
		    elt domain)))
	  ((eql (domain-of elt) domain)
	   elt)
	  ((null (setq homos (get-homomorphisms :domain (domain-of elt)
						:range domain)))
	   (unless *coercibility-checking*
	     (error "Don't know how to coerce ~S to be an element of ~S"
		    elt domain)))
	  ((null (rest homos))
	   (apply-morphism (first homos) elt))
	  (t (error "More than one homomorphism from ~S to ~S.~%~
                     Can't do automatic coercion"
		    (domain-of elt) domain)))))

;;; This method must be primary because there are more specific
;;; versions of it.
(defmethod coerce ((elt number) (domain domain))
  (coerce (canonicalize-number elt) domain))

;; FIXTHIS: Why is this here?  Both branches are the same.
(defmethod coerce ((elt numeric) (domain domain))
  (cond ((and (typep (domain-of elt) 'general-expressions)
	      (not (typep domain 'general-expressions)))
	 (call-next-method (canonicalize-number elt) domain))
	(t (call-next-method (canonicalize-number elt) domain))))

(defmethod coercible? (elt (d domain))
  (let ((*coercibility-checking* t))
    (coerce elt d)))

;;; This code provides the default methods for binary operators.

(defvar *coerce-where-possible* t)

(defmacro def-binary-coercion (op illegal-mess ambig-mess
                               &key (numeric-numeric? t)
                               (domain-element-domain-element? t))
  `(progn
    ,(when domain-element-domain-element?
           `(defmethod ,op ((x domain-element) (y domain-element))
             (when (null *coerce-where-possible*)
               (error ,illegal-mess x y))
             (let ((domain-x (domain-of x))
                   (domain-y (domain-of y))
                   common-domains)
               (when (eql domain-x domain-y)
                 (error ,illegal-mess x y))
               (setq common-domains (find-common-domains domain-x domain-y))
               (cond ((null common-domains)
                      (error ,illegal-mess x y))
                     ((null (rest common-domains))
                      (,op (coerce x (first common-domains))
                           (coerce y (first common-domains))))
                     (t (error ,ambig-mess  x y))))))
    ;; If the domain of y is non-strict then always try to coerce x
    ;; into (domain-of y).  If the domain of y is strict, then
    ;; canoncalize x, ie, coerce x into its natural algebraic domain
    ;; R, Z, C, etc, and try again.
    (defmethod ,op ((x number) (y domain-element))
      (cond ((typep (domain-of y) 'non-strict-domain)
             (,op (coerce x (domain-of y)) y))
            (*coerce-where-possible*	       
             (,op (canonicalize-number x) y))
            (t (error ,illegal-mess x y))))
    (defmethod ,op ((x domain-element) (y number))
      (cond ((typep (domain-of x) 'non-strict-domain)
             (,op x (coerce y (domain-of x))))
            (*coerce-where-possible*	       
             (,op x (canonicalize-number y)))
            (t (error ,illegal-mess x y))))
    ;; If the domain of y is non-strict then proceed as with numbers
    ;; (always coerce).  If the domain of y is strict, but the domain
    ;; of x is not, then coerce x into its natural algebraic domain
    ;; and try again.
    (defmethod ,op ((x numeric) (y domain-element))
      (cond ((eql (domain-of x) (domain-of y))
             (call-next-method))
            ((typep (domain-of y) 'non-strict-domain)
             (,op (coerce x (domain-of y)) y))
            ((and *coerce-where-possible*
                  (typep (domain-of x) 'non-strict-domain))
             (,op (canonicalize-number x) y))
            (t (call-next-method))))
    (defmethod ,op ((x domain-element) (y numeric))
      (cond ((eql (domain-of x) (domain-of y))
             (call-next-method))
            ((typep (domain-of x) 'non-strict-domain)
             (,op x (coerce y (domain-of x))))
            ((and *coerce-where-possible*
                  (typep (domain-of y) 'non-strict-domain))
             (,op x (canonicalize-number y)))
            (t (call-next-method))))
    ,(when numeric-numeric?
           `(defmethod ,op ((x numeric) (y numeric))
             (let ((x-domain (domain-of x))
                   (y-domain (domain-of y)))
               (cond ((eql x-domain y-domain)
                      (call-next-method))
                     ((typep (domain-of x) 'non-strict-domain)
                      (,op x (coerce y (domain-of x))))
                     ((typep (domain-of y) 'non-strict-domain)
                      (,op (coerce x (domain-of y)) y))
                     (t (call-next-method))))))))

(def-binary-coercion binary=
    "No way to compare ~S and ~S"
  "Ambiguous coercion for = (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary>
    "No way to compare ~S and ~S"
  "Ambiguous coercion for > (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary>=
    "No way to compare ~S and ~S"
  "Ambiguous coercion for >= (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion plus
    "No way to add ~S and ~S"
  "Ambiguous coercion for addition (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion difference
    "No way to subtract ~S and ~S"
  "Ambiguous coercion for subtraction (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion times
    "No way to multiply ~S and ~S"
  "Ambiguous coercion for multiplication (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for quotient.
(def-binary-coercion quotient
    "No way to compute the quotient of ~S and ~S"
  "Ambiguous coercion for division (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion expt
  "No way to raise ~S to the ~S power"
  "Ambiguous coercion for exponentiation (~S, ~S)")

;;; FIXME : Explicitly define a generic function for remainder.
(def-binary-coercion remainder
  "No way to compute the remainder of ~S and ~S"
  "Ambiguous coercion for remainder (~S, ~S)"
  :numeric-numeric? nil)

;; The numeric-numeric method for GCD and LCM are in numbers.lisp
(def-binary-coercion binary-gcd
    "No way to compute the GCD of ~S and ~S"
  "Ambiguous coercion for gcd (~S, ~S)"
  :numeric-numeric? nil)

(def-binary-coercion binary-lcm
    "No way to compute the LCM of ~S and ~S"
  "Ambiguous coercion for lcm (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for max-pair.
(def-binary-coercion max-pair
    "No way to compute the maximum of ~S and ~S"
  "Ambiguous coercion for max (~S, ~S)"
  :domain-element-domain-element? nil
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for min-pair.
(def-binary-coercion min-pair
    "No way to compute the minimum of ~S and ~S"
  "Ambiguous coercion for min (~S, ~S)"
  :domain-element-domain-element? nil
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for floor2.
(def-binary-coercion floor2
    "No way to compute the floor of ~S modulo ~S"
  "Ambiguous coercion for floor (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for ceiling2.
(def-binary-coercion ceiling2
    "No way to compute the ceiling of ~S modulo ~S"
  "Ambiguous coercion for ceiling (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for round2.
(def-binary-coercion round2
    "No way to round ~S modulo ~S"
  "Ambiguous coercion for round (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for truncate2.
(def-binary-coercion truncate2
    "No way to truncate ~S modulo ~S"
  "Ambiguous coercion for truncate (~S, ~S)"
  :numeric-numeric? nil)

;;; FIXME : Explicitly define a generic function for dot-product.
(def-binary-coercion dot-product
    "No way to compute the dot-product of ~S and ~S"
  "Ambiguous coercion for dot-product (~S, ~S)")
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Quotient Field Routines
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; quotient-fields.lisp,v 1.10 1995/05/24 17:42:10 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.10")

(defgeneric make-quotient-field (field)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((ring field)) ring))

(defgeneric get-quotient-field (field)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((ring field)) ring))

;;; These two methods are actually given in rational-numbers.lisp when
;;; the functions they call are defined.

;;;(defmethod make-quotient-field ((ring rational-integers))
;;;  (make-rational-numbers))

;;;(defmethod get-quotient-field ((ring rational-integers))
;;;  (get-rational-numbers))

(defgeneric make-quotient-element (domain numerator denominator)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-quotient-element
    ((domain quotient-field) numerator denominator)
  (make-instance 'quotient-element :domain domain
		 :numerator numerator :denominator denominator))

(define-domain-creator quotient-field ((ring ring))
  (progn
    (unless (gcd-domain? ring)
      (error "Can only create quotient fields of GCD domains: ~S"
             ring))
    (let ((qf (make-instance 'quotient-field :ring ring
			     :print-function 'quotient-field-print-object)))
      (with-slots (zero one) qf
        (setq zero (make-quotient-element qf (zero ring) (one ring)))
        (setq one (make-quotient-element qf (one ring) (one ring))))
      (make-homomorphism ring #'(lambda (x)
				  (make-quotient-element qf x (one ring)))
			 qf)
      qf))
  :predicate #'(lambda (d)
	         (and (typep d 'quotient-field) (eql (qf-ring d) ring))))

(defun quotient-field-print-object (qf stream)
  (with-slots (ring) qf
    (format stream "QF(~S)" ring)))

(defmethod coerce ((qe quotient-element) (d general-expressions))
  (let ((num (coerce (numerator qe) d))
	(den (coerce (denominator qe) d)))
    (setq den (if (number? den) (recip den)
		  (make-ge-expt d den (make-element d -1))))
    (cond ((1? num) den)
	  ((1? den) num)
	  (t (simplify (make-ge-times d (list num den)))))))

(defmethod print-object ((ratfun quotient-element) stream)
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((1? denominator)
	   (prin1 numerator stream))
	  (t (princ "(" stream)
	     (prin1 numerator stream)
	     (princ ")/(" stream)
	     (prin1 denominator stream)
	     (princ ")" stream)))))

(defmethod numerator ((r quotient-element))
  (qo-numerator r))

(defmethod denominator ((r quotient-element))
  (qo-denominator r))

(defmethod zero ((qf quotient-field))
  (with-slots (zero) qf
    zero))

(defmethod one ((qf quotient-field))
  (with-slots (one) qf
    one))

(defmethod 0? ((r quotient-element))
  (with-slots (numerator) r
    (0? numerator)))

(defmethod 1? ((r quotient-element))
  (with-slots (numerator denominator) r
    (and (1? numerator)
	 (1? denominator))))

(defgeneric height (object)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod height ((r quotient-element))
  (max (height (numerator r)) (height (denominator r))))

(defmethod minus ((r quotient-element))
  (let ((domain (domain-of r)))
    (with-numerator-and-denominator (numerator denominator) r
      (make-quotient-element domain (minus numerator) denominator))))

(defgeneric minus? (object)
  (:documentation
   "Return true if the object is negative."))

(defmethod minus? ((r quotient-element))
  (minus? (qo-numerator r)))

(defun quotient-reduce* (qf num &optional den)
  (with-slots (ring) qf
    (when (null den)
      (setq den (one ring))))
  (if (0? num) (zero qf)
      (let ((common-gcd (gcd num den)))
	(unless (1? common-gcd)
	  (setq num (/ num common-gcd)
		den (/ den common-gcd)))
	(when (minus? den)
	  (setq num (minus num)
		den (minus den)))
	(make-quotient-element qf num den))))

(defgeneric quotient-reduce (field numerator &optional denominator)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod quotient-reduce ((qf quotient-field) num &optional den)
  (with-slots (ring) qf
    (when (not (eql (domain-of num) ring))
      (error "The numerator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of num) ring))
    (when (not (eql (domain-of den) ring))
      (error "The denominator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of den) ring))
    (quotient-reduce* qf num den)))

(defmethod-sd plus ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (cond ((0? n1) r2)
	    ((0? n2) r1)
	    (t (quotient-reduce* domain
				 (+ (* n1 d2) (* n2 d1))
				 (* d1 d2)))))))

(defmethod-sd difference ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (cond ((0? n1)
	     (make-quotient-element domain (- n2) d1))
	    ((0? n2) r1)
	    (t (quotient-reduce* domain
				 (- (* n1 d2) (* n2 d1))
				 (* d1 d2)))))))

(defmethod-sd times ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (let (common-gcd)
	(cond ((and (1? n1) (1? d1))
	       r2)
	      ((and (1? n2) (1? d2))
	       r1)
	      (t (setq common-gcd (gcd n1 d2))
		 (if (not (1? common-gcd))
		     (setq n1 (/ n1 common-gcd)
			   d2 (/ d2 common-gcd)))
		 (setq common-gcd (gcd n2 d1))
		 (if (not (1? common-gcd))
		     (setq n2 (/ n2 common-gcd)
			   d1 (/ d1 common-gcd)))
		 (setq d1 (* d1 d2)
		       n1 (* n1 n2))
		 (if (minus? d1)
		     (setq d1 (minus d1) n1 (minus n1)))
		 (make-quotient-element domain n1 d1)))))))

(defmethod-sd quotient ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (let (common-gcd)
	(cond ((and (1? n1) (1? d1))
	       (make-quotient-element domain d2 n2))
	      ((and (1? n2) (1? d2))
	       r1)
	      (t (setq common-gcd (gcd n1 n2))
		 (if (not (1? common-gcd))
		     (setq n1 (/ n1 common-gcd)
			   n2 (/ n2 common-gcd)))
		 (setq common-gcd (gcd d1 d2))
		 (if (not (1? common-gcd))
		     (setq d2 (/ d2 common-gcd)
			   d1 (/ d1 common-gcd)))
		 (setq n1 (* n1 d2)
		       d1 (* d1 n2))
		 (if (minus? d1)
		     (setq d1 (minus d1) n1 (minus n1)))
		 (make-quotient-element domain n1 d1)))))))

(defmethod recip ((r1 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (if (minus? n1)
	(setq n1 (minus n1) d1 (minus d1)))
    (make-quotient-element (domain-of r1) d1 n1)))

(defun expt-quotient (domain quo exp)
  (with-numerator-and-denominator (n1 d1) quo
    (if (minus? exp)
	(if (minus? n1)
	    	(make-quotient-element
		  domain (expt (minus d1) (- exp)) (expt (minus n1) (- exp)))
		(make-quotient-element
		  domain (expt d1 (- exp)) (expt n1 (- exp))))
	(make-quotient-element domain (expt n1 exp) (expt d1 exp)))))

(defmethod expt ((r1 quotient-element) (exp integer))
  (expt-quotient (domain-of r1) r1 exp))

(defmethod expt ((r1 quotient-element) (exp rational-integer))
  (expt-quotient (domain-of r1) r1 (integer-value exp)))

(defmethod-sd binary-gcd ((r1 quotient-element) (r2 quotient-element))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (make-quotient-element domain (gcd n1 n2) (lcm d1 d2)))))

(defmethod coerce (x (domain quotient-field))
  (let ((temp (coercible? x (qf-ring domain))))
    (if temp (make-quotient-element domain temp (one (qf-ring domain)))
	(call-next-method))))

(defmethod coerce ((x quotient-element) (domain field))
  (let ((num (coercible? (numerator x) domain)))
    (if num (/ num (coerce (denominator x) domain))
        (call-next-method))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      General Representation
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; general.lisp,v 1.14 1995/05/24 17:42:01 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.14")

(defgeneric set-memoization (domain key value)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod set-memoization ((domain has-memoization) key value)
  (with-slots (memos) domain
    (setf (gethash key memos) value)
    value))

;;; FIXME : Merge this into a single setf function.
(defgeneric get-memoization (domain key)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-memoization ((domain has-memoization) key)
  (with-slots (memos) domain
    (gethash key memos)))

(defsetf get-memoization set-memoization)

(defmacro %memoize (domain expression &body body)
  `(let ((.expr. ,expression))
    (with-slots (memos) ,domain
      (multiple-value-bind (value found?) (gethash .expr. memos)
        (if found? value
            (setf (get-memoization ,domain .expr.) (progn ,@body)))))))

(defmacro memoize (expression &body body)
  `(%memoize *general* ,expression ,@body))

#+ignore
(defun fib-memo (n)
  (memoize `(fib ,n)
    (if (< n 2) 1
	(+ (fib-memo (- n 1)) (fib-memo (- n 2))))))

(defgeneric display (expression &optional stream &rest ignore)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((express general-expression) &optional stream &rest ignore)
    (declare (ignore ignore))
    (princ express stream)))

(defgeneric simplify (expression)
  (:documentation
   "Simplify the expression.")
  (:method ((expression general-expression)) expression))

(defgeneric ge-equal (expression1 expression2)
  (:documentation
   "The purpose of this method is unknown.")
  (:method (expression1 expression2)
    (declare (ignore expression1 expression2))
    nil))

(defmethod ge-equal ((x general-expression) (y general-expression))
  (eql x y))

(defgeneric ge-great (expression1 expression2)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((x general-expression) (y general-expression))
    (declare (ignore x y))
    nil))

;; Numbers and Variables

(defmethod make-quotient-element
    ((domain general-expressions) (x integer) (y integer))
  (make-instance 'rational-number :domain domain
		 :numerator x :denominator y))

(defmethod make-element ((domain general-expressions) (x integer) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'rational-integer :domain domain :value x))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x ratio) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'rational-number :domain domain
			:numerator (cl:numerator x)
			:denominator (cl:denominator x)))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x float) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'floating-point-number :domain domain
			:value x))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x cl:complex)
			 &rest ignore)
  (declare (ignore ignore))
  (make-instance 'complex-number :domain domain
		 :realpart (cl:realpart x)
		 :imagpart (cl:imagpart x)))

(defmethod coerce ((num number) (domain general-expressions))
  (make-element domain num))

(defmethod coerce ((num rational-integer) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-element domain (integer-value num))))

(defmethod coerce ((num rational-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-instance 'rational-number :domain domain 
                     :numerator (qo-numerator num)
                     :denominator (qo-denominator num))))

(defmethod coerce ((num floating-point-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-element domain (fp-value num))))

(defmethod coerce ((num bigfloat) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-bigfloat domain (bigfloat-mantissa num) (bigfloat-exponent num))))

(defmethod coerce ((num cl:complex) (domain general-expressions))
  (make-instance 'complex-number :domain domain
		 :realpart (realpart num)
		 :imagpart (imagpart num)))

(defmethod coerce ((num complex-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-instance 'complex-number :domain domain
		     :realpart (cn-realpart num)
		     :imagpart (cn-imagpart num))))

(defmethod simplify ((x number))
  (make-element *general* x))

(defmethod simplify ((x numeric)) x)

;;; AUDIT : These methods were defined as a single method using an OR
;;; specializer. Might be useful to revisit that extension.
(defmethod ge-equal ((x number)  (y number))  (= x y))
(defmethod ge-equal ((x numeric) (y number))  (= x y))
(defmethod ge-equal ((x number)  (y numeric)) (= x y))
(defmethod ge-equal ((x numeric) (y numeric)) (= x y))

(defmethod ge-equal ((x number) y)
  (declare (ignore y))
  nil)

(defmethod ge-equal ((x numeric) y)
  (declare (ignore y))
  nil)

(defmethod ge-equal (x (y number))
  (declare (ignore x))
  nil)

(defmethod ge-equal (x (y numeric))
  (declare (ignore x))
  nil)

(defmethod ge-great ((x number)  (y number))  (> x y))
(defmethod ge-great ((x numeric) (y number))  (> x y))
(defmethod ge-great ((x number)  (y numeric)) (> x y))
(defmethod ge-great ((x numeric) (y numeric)) (> x y))

(defmethod ge-great ((x number) y)
  (declare (ignore y))
  nil)

(defmethod ge-great ((x numeric) y)
  (declare (ignore y))
  nil)

(defmethod ge-great (x (y number))
  (declare (ignore x))
  t)

(defmethod ge-great (x (y numeric))
  (declare (ignore x))
  t)

;; Variables

(defgeneric reparse-print-string (variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod reparse-print-string ((var ge-variable))
  (let ((string (cond ((atom (symbol-of var))
		       (string-downcase (symbol-of var)))
		      (t (format nil "[~A]" (symbol-of var)))))
	temp)
    (when (setq temp (getf var :subscripts))
      (setq string 
	    (format nil "~A(~S~{,~S~})"
		    string (first temp) (rest temp))))
    (setf (string-of var) string)))

(defmethod initialize-instance :after ((var ge-variable) &rest ignore)
  (declare (ignore ignore))
  (reparse-print-string var))

(defgeneric make-ge-variable (domain variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-variable ((domain general-expressions) var)
  (loop for v in (ge-variables domain)
	do (when (equal (symbol-of v) var)
	     (return v))
	finally
         (setq var (make-instance 'ge-variable :domain domain :symbol var))
	 (push var (ge-variables domain))
	 (return var)))

(defmethod coerce ((var symbol) (domain general-expressions))
  (make-ge-variable domain var))

(defmethod print-object ((var ge-variable) stream)
  (let ((sym (string-of var)))
    (cond ((and (not (null sym)) (atom sym))
	   #+Genera
	   (format stream "~'i~A~" sym)
	   #-Genera
	   (princ sym stream))
	  (t (princ (symbol-of var) stream)))))

;; This function is only to be applied to general expressions. 
(defsubst ge-variable? (x)
  (typep x 'ge-variable))

(defgeneric add-subscripts (variable &rest subscripts)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod add-subscripts ((var ge-variable) &rest subscripts)
  (setq var (coerce var *general*))
  (let* ((symbol (symbol-of var))
	 (subscripts (append (getf var :subscripts) (copy-list subscripts)))
	 (canonical-var 
	  (member symbol (ge-variables *general*)
		  :test #'(lambda (a b)
			    (and (equal a (symbol-of b))
				 (equal subscripts (getf b :subscripts)))))))
    (cond (canonical-var
	   (first canonical-var))
	  (t (setq var (make-instance 'ge-variable :domain (domain-of var)
				      :symbol symbol))
	     (setf (getf var :subscripts) subscripts)
	     (reparse-print-string var)
	     (push var (ge-variables *general*))
	     var))))

(defmethod add-subscripts ((var symbol) &rest subscripts)
  (%apply #'add-subscripts (coerce var *general*) subscripts))

(defmethod ge-equal ((x ge-variable) (y ge-variable))
  (eql x y))

(defmethod ge-great ((x ge-variable) (y ge-variable))
  (string-greaterp (string-of x) (string-of y)))

(defmethod ge-great ((x ge-variable) (y  ge-plus))  
  (loop for w in (terms-of y)
	unless (ge-great x w)
        do (return nil)
	finally (return t)))

(defmethod ge-great ((x ge-variable) (y ge-times))  
  (loop for w in (terms-of y)
	unless (ge-great x w)
        do (return nil)
	finally (return t)))

(defmethod ge-great ((x ge-plus) (y ge-variable))  
  (loop for w in (terms-of x)
	unless (ge-great w y)
        do (return t)
	finally (return nil)))

(defmethod ge-great ((x ge-times) (y ge-variable))  
  (loop for w in (terms-of x)
	unless (ge-great w y)
        do (return t)
	finally (return nil)))

;; Functions

(defun search-for-function (list name nargs)
  (loop for f in list
        do (when (and (not (typep f 'ge-function-deriv))
                      (string= name (name-of f)))
             (when (and nargs (not (cl:= nargs (nargs-of f))))
               (error "Wrong number of arguments specified for function ~A"
                      name))
             (return f))))  

(defgeneric get-function (domain name &optional args)
  (:documentation
   "The purpose of this method is not known."))

(defmethod get-function ((domain general-expressions) name &optional nargs)
  (setq name (string-downcase (string name)))
  (or (search-for-function (ge-functions domain) name nargs)
      (search-for-function *global-functions* name nargs)))

(defmethod get-function ((domain (eql nil)) name &optional nargs)
  (setq name (string-downcase (string name)))
  (search-for-function *global-functions* name nargs))

(defgeneric make-function (domain name &optional nargs)
  (:documentation
   "The purpose of this functions is unknown."))

(defmethod make-function ((domain general-expressions) name &optional nargs)
  (setq name (string-downcase (string name)))
  (let ((fun (or (search-for-function (ge-functions domain) name nargs)
                 (search-for-function *global-functions* name nargs))))
    (unless fun
      (when (null nargs)
        (error "Number of arguments to ~A must be specified" name))
      (setq fun (make-instance 'ge-function :domain domain
                               :name name :nargs nargs))
      (push fun (ge-functions domain)))
    fun))

(defmethod make-function ((domain (eql nil)) name &optional nargs)
  (setq name (string-downcase (string name)))
  (let ((fun (search-for-function *global-functions* name nargs)))
    (unless fun
      (when (null nargs)
        (error "Number of arguments to ~A must be specified" name))
      (setq fun (make-instance 'ge-function :domain domain
                               :name name :nargs nargs))
      (push fun *global-functions*))
    fun))

(defmethod derivs-of ((f ge-function)) nil)

(defun add-function-to-domain (domain name nargs &optional derivs)
  (let ((function-class (if derivs 'ge-function-deriv 'ge-function))
        deriv)
    (loop for f in (ge-functions domain)
          do (when (and (typep f function-class)
                        (eql (name-of f) name)
                        (equal (derivs-of f) derivs))
               (setq deriv f)
               (return t)))
    (unless deriv
      (setq deriv (make-instance function-class :domain domain
                                 :name name :nargs nargs
                                 :derivs derivs))
      (push deriv (ge-functions domain)))
    deriv))

;;; FIXME : This needs to be merged into the generic function.
(defmethod minus? ((x t))
  (declare (ignore x))
  nil)

;; For compatibility with Common Lisp
(defun minusp (x) (minus? x))
(defun plusp (x) (plus? x))
(defun zerop (x) (0? x))

(defgeneric make-function-deriv (function derivative)
  (:documentation
   "The purpose of this method is unknown."))

;; The copy-list's in the following functions is necessary because
;; sort destructively modifies its argument.  --RZ
(defmethod make-function-deriv ((fun ge-function) (i integer))
  (when (or (minusp i)
            (not (< i (nargs-of fun))))
    (error "Illegal derivative of ~S in position ~D" fun i))
  (add-function-to-domain (domain-of fun)
                          (name-of fun)
                          (nargs-of fun)
                          (if (typep fun 'ge-function-deriv)
                              (sort (cons i (copy-list (derivs-of fun)))
                                    #'cl:<)
                              (list i))))

(defmethod make-function-deriv ((fun ge-function) (derivs list))
  (add-function-to-domain (domain-of fun)
                          (name-of fun)
                          (nargs-of fun)
			  (sort (if (typep fun 'ge-function-deriv)
				    (append derivs (copy-list (derivs-of fun)))
				    derivs)
				#'cl:<)))

(defgeneric make-function-integrate (function integrand)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-function-integrate ((fun ge-function) (i integer))
  (when (or (minusp i)
            (not (< i (nargs-of fun))))
    (error "Illegal derivative of ~S in position ~D" fun i))
  (let ((derivs (if (typep fun 'ge-function-deriv)
                    (derivs-of fun) nil)))
    (cond ((member i derivs)
           (setq derivs (remove i derivs :count 1)))
          (t (error "Don't have representation for integrals yet")))
    (add-function-to-domain (domain-of fun) (name-of fun) (nargs-of fun)
                            derivs)))

(defmethod print-object ((fun ge-function) stream)
  #+Genera
  (format stream "~'i~A~" (name-of fun))
  #-Genera
  (princ (name-of fun) stream))

(defmethod print-object ((fun ge-function-deriv) stream)
  #+Genera
  (format stream "~'i~A~" (name-of fun))
  #-Genera
  (princ (name-of fun) stream)
  (princ "_{" stream)
  (loop for n in (derivs-of fun)
        do (princ n stream))
  (princ "}" stream))

(defgeneric make-ge-funct (domain function &rest args)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-funct ((domain general-expressions) funct &rest args)
  (make-instance 'ge-application :domain domain
		 :funct (if (ge-function? funct) funct
                            (make-function domain funct (length args)))
                 :args (copy-list args)))

(defmethod apply ((fun ge-function) &rest args)
  (let ((domain (domain-of fun)))
    (flet ((check-domain (dom)
             (cond ((null domain)
                    (if (typep dom 'general-expressions)
                        (setq domain dom)
                        (error "GE function of ~D" dom)))
                   ((not (eql domain dom))
                    (error "Incompatible domains apply ~S to ~S" fun args)))))
      (setq args (accum-apply-args args))
      (loop for arg in args
            do (when (or (typep arg 'general-expression)
                         (typep arg 'numeric))
                 (check-domain (domain-of arg))))
      (when (null domain)
        (setq domain *general*))
      (simplify
       (make-instance 'ge-application :domain domain
                      :funct fun
                      :args (loop for arg in args
                                  collect (coerce arg domain)))))))

(defmacro funct (function &rest args)
  `(make-ge-funct *general* ',function
    ,@(mapcar #'(lambda (q) `(coerce ,q *general*))
              args)))

(defgeneric display-list (objects &optional stream)
  (:documentation "Display a list of objects, paying attention to
*print-length*.  No surrounding delimiters.  This is a method so that
we can define similar functions for sets of objects embedded in
arrays."))

(defmethod display-list
    ((objects list) &optional (stream *standard-output*))
  (when objects
    (let ((cnt (or *print-length* -1)))
      (declare (fixnum cnt))
      (print-object (first objects) stream)
      (cl:decf cnt)
      (loop for var in (rest objects)
	    do (princ ", " stream)
	       (when (cl:zerop cnt)
		 (princ "..." stream)
		 (return))
	       (print-object var stream)
	       (cl:decf cnt)))))

(defmethod print-object ((x ge-application) stream)
  (print-object (funct-of x) stream)
  (write-char #\( stream)
  (display-list (args-of x) stream)
  (write-char #\) stream))

(defmethod simplify ((x ge-application))
  (let ((args (mapcar #'simplify (args-of x)))
        (simplifier (getf (funct-of x) 'simplify))
	new-x)
    ;; This is the function application with the simplified arguments.
    (setq new-x (apply #'make-ge-funct (domain-of x) (funct-of x) args))
    (if simplifier
        (apply simplifier (domain-of x) new-x args)
        new-x)))

;; Contexts

(defvar *initialize-contexts-funs* ())

(defun initialize-contexts ()
  (setq *general* (make-instance 'general-expressions))
  (loop for fun in *initialize-contexts-funs*
	do (funcall fun)))

(defmacro with-new-context (&body body)
  `(let ((*general* (make-instance 'general-expressions)))
    ,@body))

(defmacro check-point-context (&body body)
  `(let ((.old-variables. (ge-variables *general*))
	 (.old-context. (ge-context *general*)))
    (unwind-protect (progn ,@body)
      (setf .old-variables. (ge-variables *general*))
      (setf .old-context. (ge-context *general*)))))

(defgeneric make-ge-plus (domain terms)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-plus ((domain general-expressions) terms)
  (make-instance 'ge-plus :domain domain :terms terms))

(defgeneric make-ge-times (domain terms)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-times ((domain general-expressions) terms)
  (make-instance 'ge-times :domain domain :terms terms))

(defgeneric make-ge-expt (domain base exp)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-expt ((domain general-expressions) base exp)
  (make-instance 'ge-expt :domain domain :base base :exp exp))

(defmethod coerce ((exp list) (domain general-expressions))
  (flet ((coerce-obj (x)
	   (coerce x domain)))
    (cond ((eql (first exp) '+)
	   (make-ge-plus domain
			 (mapcar #'coerce-obj (rest exp))))
	  ((eql (first exp) '*)
	   (make-ge-times domain
                          (mapcar #'coerce-obj (rest exp))))
	  ((eql (first exp) '-)
	   (if (null (rest (rest exp)))
	       (make-ge-times domain (list -1 (coerce-obj (second exp))))
	       (make-ge-plus domain
                             (list (coerce-obj (second exp))
                                   (make-ge-times domain
                                                  (cons (make-element domain -1)
                                                        (mapcar #'coerce-obj (rest (rest exp)))))))))
	  ((eql (first exp) '/)
	   (make-ge-times domain
                          (list (coerce-obj (second exp))
                                (make-ge-expt domain
                                              (make-ge-times domain
                                                             (mapcar #'coerce-obj (rest (rest exp))))
                                              (make-element domain -1)))))
	  (t (error "Don't know how to coerce ~S into ~S"
		    exp domain)))))			     

(defun parenthesized-display (expr stream)
  (princ "(" stream)
  (print-object expr stream)
  (princ ")" stream))

(defun safe-display (expr stream)
  (if (or (and (number? expr) (not (typep expr 'complex-number)))
	  (and (typep expr 'complex-number)
	       (0? (realpart expr)))
	  (ge-variable? expr)
	  (ge-expt? expr))
      (print-object expr stream)      
      (parenthesized-display expr stream)))

;; Ordering functions for general expressions

;; Some operators may choose to ignore various parameters here.

(defun ge-lequal (x y)
  (loop
   (when (and (null x) (null y))
     (return-from ge-lequal t))
   (when (or (null x) (null y)
             (not (ge-equal (first x) (first y))))
     (return-from ge-lequal nil))
   (pop x) (pop y)))

(defun ge-lgreat (x y)
  (loop 
   (cond ((null x)
          (return nil))
         ((null y)
          (return t))
         ((ge-equal (first x) (first y)))
         ((ge-great (first x) (first y))
          (return t))
         (t (return nil)))
   (pop x) (pop y)))

(defgeneric real? (object)
  (:documentation
   "Return true if the object is real valued.")
  (:method ((object number))
    (not (cl:complexp object)))
  (:method ((object bigfloat))
    (declare (ignore object))
    t)
  (:method ((object numeric))
    (not (typep object 'complex-number))))

(defun ge-minus? (x)
  (cond ((and (number? x) (real? x)) (minus? x))
	((ge-times? x)
	 (let ((lead-term (first (terms-of x))))
	   (and (and (number? lead-term)
		     (real? lead-term)
		     (minus? lead-term)))))
	(t nil)))

;; This works by converting the sum into a list of dotted pairs.  The
;; first element of the list is a number, while the second is a list
;; of product terms.  This makes combining new elements quite easy.
;; After the combination, everything is converted back to the standard
;; representation. 
(defmacro merge-terms-in-sum (terms &body body)
  `(let ((,terms (list nil)))
    (labels ((add-term (base order) 
               (loop with terms = ,terms do
                     (cond ((or (null (rest terms))
                                (ge-lgreat base (rest (second terms))))
                            (push (cons order base) (rest terms))
                            (return t))
                           ((ge-lequal base (rest (second terms)))
                            (setf (first (second terms))
                                  (+ (first (second terms)) order))
                            (when (0? (first (second terms)))
                              (setf (rest terms) (rest (rest terms))))
                            (return t)))
                     (pop terms))))
      ,@body)))

(defun simp-plus-terms (domain old-terms)
  (merge-terms-in-sum terms
    (let ((const 0))
      (labels ((loop-over-terms (terms)
		 (loop for term in terms
		       do (setq term (simplify term))
                       (cond ((number? term) 
                              (setq const (+ const term)))
                             ((ge-plus? term)
                              (loop-over-terms (terms-of term)))
                             ((ge-times? term)
                              (setq term (terms-of term))
                              (cond ((number? (first term))
                                     (add-term (rest term) (first term)))
                                    (t (add-term term 1))))
                             (t (add-term (list term) 1))))))
	(loop-over-terms old-terms)
	(setq terms (loop for (c . term-l) in (rest terms)
			  collect
			  (if (or (eql c 1) (eql c 1.0))
			      (if (null (rest term-l))
				  (first term-l)
				  (simplify
				   (make-ge-times domain term-l)))
			      (simplify
			       (make-ge-times domain (cons c term-l)))))) 
	(cond ((not (0? const))
	       (if (null terms) const
		   (make-ge-plus domain (cons const terms))))
	      ((null terms)
	       (make-element domain 0))
	      ((null (rest terms))
	       (first terms))
	      (t (make-ge-plus domain terms)))))))

(defun simp-times-terms (domain old-terms)
  (merge-terms-in-sum terms 
    (let ((const 1))
      (labels ((loop-over-terms (terms) 
		 (loop for term in terms do
		   (setq term (simplify term))
		   (cond ((number? term)
			  (when (0? term)
			    (return-from simp-times-terms
			      (make-element domain 0)))
			  (setq const (* const term)))
			 ((ge-times? term)
			  (loop-over-terms (terms-of term)))
			 ((ge-expt? term)
			  (let ((exp (exponent-of term))
				(base (base-of term)))
			    (cond ((number? (exponent-of term))
				   (add-term (list base) exp))
				  (t (add-term (list base)
					       (make-element domain 1))))))
			 (t (add-term (list term) 1))))))
	(loop-over-terms old-terms)
	(setq terms (loop for (exp base) in (rest terms)
			  collect
			  (if (1? exp) base
			      (make-ge-expt domain base exp))))
	(cond ((not (1? const))
	       (if (null terms)
		   const
		   (make-ge-times domain (cons const terms))))
	      ((null terms)
	       (make-element domain 1))
	      ((null (rest terms))
	       (first terms))
	      (t (make-ge-times domain terms)))))))

(defmethod print-object ((sum ge-plus) stream)
  (let ((terms (terms-of sum)))
    (print-object (first terms) stream)
    (loop for x in (rest terms)
	  do (cond ((and (number? x) (real? x))
		    (if (plus? x)
			(format stream " + ~S" x)
			(format stream " - ~S" (minus x))))
		   ((ge-minus? x)
		    (princ " - " stream)
		    (safe-display 
		     (simp-times-terms (domain-of sum) (list -1 x))
		     stream))
		   (t (princ " + " stream)
		      (print-object x stream))))))

(defmethod simplify ((x ge-plus))
  (simp-plus-terms (domain-of x) (terms-of x)))

(defmethod ge-equal ((x ge-plus) (y ge-plus))
  (ge-lequal (terms-of x) (terms-of y)))

(defmethod ge-great ((x ge-plus) (y ge-plus))
  (ge-lgreat (terms-of x) (terms-of y)))

(defmethod print-object ((x ge-times) stream)
  (let ((terms (terms-of x)))
    (safe-display (first terms) stream)
    (loop for x in (rest terms)
	  do (princ " " stream)
	     (safe-display x stream))))

(defmethod simplify ((x ge-times)) 
  (simp-times-terms (domain-of x) (terms-of x)))

(defmethod ge-equal ((x ge-times) (y ge-times))
  (ge-lequal (terms-of x) (terms-of y)))

(defmethod ge-great ((x ge-times) (y ge-times))
  (ge-lgreat (terms-of x) (terms-of y)))

(defmethod simplify ((x ge-expt))
  (let ((exp (simplify (exponent-of x)))
	(base (base-of x)))
    (cond ((0? exp) 1)
	  ((1? exp) (simplify base))
	  ((and (number? (setq base (simplify base)))
		(number? exp))
	   (expt base exp))
	  ((ge-expt? base)
	   (simplify 
	    (make-ge-expt (domain-of x) (base-of base)
			  (* (exponent-of base) exp))))
	  (t (make-ge-expt (domain-of x) (simplify (base-of x)) exp)))))

(defmethod print-object ((expr ge-expt) stream)
  (safe-display (base-of expr) stream)
  (princ "^" stream)
  (safe-display (exponent-of expr) stream))

(defmethod ge-equal ((x ge-expt) (y ge-expt))
  (and (ge-equal (base-of x) (base-of y))
       (ge-equal (exponent-of x) (exponent-of y))))

(defmethod ge-great ((x ge-expt) (y ge-expt))
  (cond ((ge-great (base-of x) (base-of y))
	 t)
	((ge-equal (base-of x) (base-of y))
	 (ge-great (exponent-of x) (exponent-of y)))
	(t nil)))

(defmethod ge-equal ((x ge-application) (y ge-application))
  (and (eql (funct-of x) (funct-of y))
       (ge-lequal (args-of x) (args-of y))))

(defmethod ge-equal ((x ge-function) (y ge-function))
  (eql x y))

(defgeneric get-variable-property (domain variable key)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-variable-property ((domain domain) (var ge-variable) key)
  (loop for var-prop in (ge-context domain)
	do (when (eql (first var-prop) var)
	     (return (%getf (rest var-prop) key)))
	finally (progn 
		  (push (list var) (ge-context domain))
		  (return nil))))

(defgeneric set-variable-property (domain variable key value)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod set-variable-property (domain (var ge-variable) key value)
  (loop for var-prop in (ge-context domain)
	do (when (eql (first var-prop) var)
	     (setf (%getf (rest var-prop) key) value)
	     (return value))	
	finally (progn 
		  (push (list var key value) (ge-context domain))
		  (return value))))

(defsetf get-variable-property set-variable-property)

;; Variable dependencies and DEPENDS-ON? 

(defgeneric declare-dependencies (variable &rest variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod declare-dependencies ((var ge-variable) &rest vars)
  (let ((depends (get-variable-property (domain-of var) var :dependencies))
	(domain (domain-of var)))
    (loop for v in vars
	  do (setq v (coerce v domain))
          (unless (member v depends :test #'ge-equal)
            (push v depends)))
    (setf (get-variable-property (domain-of var) var :dependencies) depends)))

(defgeneric depends-on? (expression &rest variables)
  (:documentation
   "Return true if the expression depends on any of the variables"))

(defmethod depends-on? ((exp list) &rest vars)
  (loop for arg in exp
	do (when (apply #'depends-on? arg vars)
	     (return t))
	finally (return nil)))

(defmethod depends-on? ((exp number) &rest vars)
  (declare (ignore vars))
  nil)

(defmethod depends-on? ((exp numeric) &rest vars)
  (declare (ignore vars))
  nil)

(defmethod depends-on? ((exp ge-variable) &rest vars)
  (or (member exp vars :test #'ge-equal)
      (loop
       for var in (get-variable-property
                   (domain-of exp) exp :dependencies)
       do (when (member var vars :test #'ge-equal)
            (return t))
       finally (return nil))))

(defmethod depends-on? ((exp ge-function) &rest vars)
  (loop for var in vars
	do (when (and (typep var 'ge-function)
		      (eql (name-of exp) (name-of var)))
	     (return t))
	finally (return nil)))

(defmethod depends-on? ((exp ge-application) &rest vars)
  (or (apply #'depends-on? (funct-of exp) vars)
      (apply #'depends-on? (args-of exp) vars)))		     

(defmethod depends-on? ((exp ge-plus) &rest vars)
  (apply #'depends-on? (terms-of exp) vars))

(defmethod depends-on? ((exp ge-times) &rest vars)
  (apply #'depends-on? (terms-of exp) vars))

(defmethod depends-on? ((exp ge-expt) &rest vars)
  (or (apply #'depends-on? (base-of exp) vars)
      (apply #'depends-on? (exponent-of exp) vars)))

;; Derivatives

(defgeneric ge-deriv (expression variable)
  (:documentation
   "Return the derivate of the expression with respect to variable.")
  (:method (expression variable)
    (error "Don't know how to take the derivative of ~S wrt ~S."
           expression variable)))

(defgeneric deriv (expression &rest variables)
  (:documentation
   "The purspose of this method is unknown."))

(defmethod deriv ((exp number) &rest vars)
  (if (null vars)
      (make-element *general* exp)
      (make-element *general* 0)))

(defmethod deriv ((exp numeric) &rest vars)
  (if (null vars) exp
      (make-element (domain-of exp) 0)))

(defmethod deriv ((exp symbol) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for v in vars
	do (setq exp (ge-deriv exp (coerce v *general*))))
  exp)

(defmethod deriv ((exp general-expression) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for v in vars
	do (setq exp (ge-deriv exp (coerce v *general*))))
  exp)

(defmethod deriv ((fun ge-function) &rest args)
  (loop for i in args
	with nargs = (nargs-of fun)
	do (when (or (minusp i) (not (< i nargs)))
	     (error "Illegal derivative of ~S in position ~D" fun i)))
  (cond ((null args)
	 fun)
	((getf fun 'deriv)
	 (apply #'deriv (nth (first args) (getf fun 'deriv))
		(rest args)))
	(t (make-function-deriv fun (copy-list args)))))

(defmethod ge-deriv ((exp general-expression) (var symbol))
  (ge-deriv exp (coerce var (domain-of exp))))

(defmethod-sd ge-deriv ((exp numeric) (var ge-atom))
  (make-element domain 0))

(defmethod-sd ge-deriv ((exp ge-atom) (var ge-atom))
  (cond ((ge-equal exp var) (make-element domain 1))
        #+ignore
	((depends-on? exp var)
	 (make-ge-deriv domain exp `((,var 1))))
	(t (make-element domain 0))))

(defmethod-sd ge-deriv ((exp ge-plus) (var ge-atom))
  (simplify
   (make-ge-plus domain (loop for x in (terms-of exp)
			      collect (ge-deriv x var)))))

(defmethod-sd ge-deriv ((exp ge-times) (var ge-atom))
  (let ((terms (terms-of exp)))
    (simplify
     (make-ge-plus
      domain
      (loop for x in terms
            collect
            (simplify
             (make-ge-times domain
                            (cons (ge-deriv x var) (remove x terms)))))))))

(defmethod-sd ge-deriv ((exp ge-expt) (var ge-atom))
  (let ((base (base-of exp))
	(power (exponent-of exp)))
    (cond ((depends-on? power var)
	   (error "Not yet implemented"))
	  ((and (number? power) (= power 2))
	   (* 2 base (ge-deriv base var)))
	  (t (* power (expt base (- power 1)))))))

(defmethod-sd ge-deriv ((exp ge-application) (var ge-atom))
  (let* ((args (args-of exp))
         (fun (funct-of exp))
         (dargs (loop for arg in args
                      collect (ge-deriv arg var)))
         (derivs (getf fun 'deriv))
         ans)
    (when (null derivs)
      (setq derivs (loop for i below (nargs-of fun)
                         collect (make-function-deriv fun i)))
      (setf (getf fun 'deriv) derivs))
    (loop for darg in dargs
          for deriv in derivs
          do  (unless (0? darg)
                (push (if (1? darg)
                          (apply deriv args)
                          (* darg (apply deriv args)))
                      ans)))
    (cond ((null ans)
	   (zero domain))
	  ((rest ans) (make-ge-plus domain ans))
	  (t (first ans)))))

(defgeneric make-ge-eqn= (domain lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-eqn= ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn= :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn=) stream)
  (print-object (lhs-of eqn) stream)
  (princ " = " stream)
  (print-object (rhs-of eqn) stream))

(defgeneric eqn= (lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod eqn= (lhs rhs)
  (make-ge-eqn= *general*
		(simplify (coerce lhs *general*))
		(simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn=))
  (make-ge-eqn= (domain-of eqn)
		(simplify (lhs-of eqn))
		(simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn=) (var ge-atom))
  (make-ge-eqn= domain
		(ge-deriv (lhs-of eqn) var)
		(ge-deriv (rhs-of eqn) var)))

(defgeneric make-ge-eqn> (domain lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-eqn> ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn> :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn>) stream)
  (print-object (lhs-of eqn) stream)
  (princ " > " stream)
  (print-object (rhs-of eqn) stream))

(defgeneric eqn> (lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod eqn> (lhs rhs)
  (make-ge-eqn> *general*
		(simplify (coerce lhs *general*))
		(simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn>))
  (make-ge-eqn> (domain-of eqn)
		(simplify (lhs-of eqn))
		(simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn>) (var ge-atom))
  (make-ge-eqn> domain
		(ge-deriv (lhs-of eqn) var)
		(ge-deriv (rhs-of eqn) var)))

(defgeneric make-ge-eqn>= (domain lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-eqn>= ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn>= :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn>=) stream)
  (print-object (lhs-of eqn) stream)
  (princ " >= " stream)
  (print-object (rhs-of eqn) stream))

(defgeneric eqn>= (lhs rhs)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod eqn>= (lhs rhs)
  (make-ge-eqn>= *general*
		 (simplify (coerce lhs *general*))
		 (simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn>=))
  (make-ge-eqn>= (domain-of eqn)
		 (simplify (lhs-of eqn))
		 (simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn>=) (var ge-atom))
  (make-ge-eqn>= domain
		 (ge-deriv (lhs-of eqn) var)
		 (ge-deriv (rhs-of eqn) var)))

;;; Scalar versus scalar is covered by numbers.lisp
;;; These are just supposed to be the other cases.  
;;; FIXME : Expand this macro.
(defmacro define-ge2-standard-methods (op)
  `(progn
    (defmethod ,op ((x number) (y symbol))
      (,op (coerce x *general*) (coerce y *general*)))
    (defmethod ,op ((x symbol) (y symbol))
      (,op (coerce x *general*) (coerce y *general*)))
    (defmethod ,op ((x symbol) (y number))
      (,op (coerce x *general*) (coerce y *general*)))
    (defmethod ,op ((x general-expression) (y symbol))
      (,op x (coerce y (domain-of x))))
    (defmethod ,op ((x symbol) (y general-expression))
      (,op (coerce x (domain-of y)) y))))

;;; When a numeric from a different domain is added to a general
;;; expression, the code in morphisms.lisp will coerce the numeric to
;;; the *general* (a non-strict-domain) and then come back here.

(define-ge2-standard-methods plus)

(defmethod-sd plus ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-plus domain (list x y))))

(defmethod-sd plus ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (make-ge-eqn= domain
		(+ (lhs-of eq1) (lhs-of eq2))
		(+ (rhs-of eq1) (rhs-of eq2))))

(defmethod-sd plus ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (+ (lhs-of eq1) exp) (+ (rhs-of eq1) exp)))

(defmethod-sd plus ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (+ (lhs-of eq1) exp) (+ (rhs-of eq1) exp)))

(define-ge2-standard-methods difference)

(defmethod-sd difference ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-plus
             domain
             (list x (make-ge-times
                      domain
                      (list (make-element domain -1) y))))))

(defmethod-sd difference ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (make-ge-eqn= domain
		(- (lhs-of eq1) (lhs-of eq2))
		(- (rhs-of eq1) (rhs-of eq2))))

(defmethod-sd difference ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (- (lhs-of eq1) exp) (- (rhs-of eq1) exp)))

(defmethod-sd difference ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (- exp (lhs-of eq1)) (- exp (rhs-of eq1))))

(defmethod minus ((x symbol))
  (- (coerce x *general*)))

(defmethod minus ((x general-expression))
  (let ((domain (domain-of x)))
    (simplify
     (make-ge-times domain (list (make-element domain -1) x)))))

(defmethod minus ((eq1 ge-eqn=))
  (make-ge-eqn= (domain-of eq1) (- (lhs-of eq1)) (- (rhs-of eq1))))

(define-ge2-standard-methods times)

(defmethod-sd times ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-times domain (list x y))))

(defmethod-sd times ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't multiply two equations"))

(defmethod-sd times ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (* (lhs-of eq1) exp) (* (rhs-of eq1) exp)))

(defmethod-sd times ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (* (lhs-of eq1) exp) (* (rhs-of eq1) exp)))

(define-ge2-standard-methods quotient)

(defmethod-sd quotient ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-times
             domain
             (list x (make-ge-expt domain y (make-element domain -1))))))

(defmethod-sd quotient ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't divide two equations"))

(defmethod-sd quotient ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (/ (lhs-of eq1) exp) (/ (rhs-of eq1) exp)))

(defmethod-sd quotient ((exp domain-element) (eq1 ge-eqn=))
  (error "Can't divide by an equation"))

(defmethod recip ((x symbol))
  (recip (coerce x *general*)))

(defmethod recip ((x general-expression))
  (let ((domain (domain-of x)))
    (simplify (make-ge-expt domain x (make-element domain -1)))))

(defmethod recip ((eq1 ge-eqn=))
  (make-ge-eqn= (domain-of eq1) (/ (lhs-of eq1)) (/ (rhs-of eq1))))

(define-ge2-standard-methods expt)

(defmethod-sd expt ((x general-expression) (y ge-or-numeric))
  (simplify (make-ge-expt domain x y)))

(defmethod-sd expt ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't exponentiate two equations"))

(defmethod-sd expt ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (expt (lhs-of eq1) exp) (expt (rhs-of eq1) exp)))

(defmethod-sd expt ((exp ge-or-numeric) (eq1 ge-eqn=))
  (error "Can't put an equation in an exponent"))

(defgeneric make-union (variable set expression &rest expressions)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-union ((var symbol) (set set) (expr general-expression)
		       &rest rest-exprs)
  (apply #'make-union (coerce var (domain-of expr)) set expr rest-exprs))

(defun make-universal-quantified-set (domain bound-vars expressions)
  (make-instance 'universal-quantified-set
		 :domain domain
		 :bound-vars bound-vars
		 :expressions expressions
		 :print-function 'uq-set-print-object))

(defun uq-set-print-object (set stream)
  (let ((bound-vars (bound-vars-of set)))
    (format stream "{ForAll ~S in ~S~:{, ~S in ~S~} . ~S~{, ~S~}}" 
	    (first (first bound-vars)) (second (first bound-vars))
	    (rest bound-vars)
	    (first (exprs-of set)) (rest (exprs-of set)))))

;; This used to be just variables, but it makes more sense for one to be
;; able to use quantifiers over any atomic object.
(defmethod make-union (var set (expr general-expression)
		       &rest rest-exprs)
  (let ((domain (domain-of expr)))
    ;;; Make sure that the union is sensible.
    (dolist (x rest-exprs)
      (if (not (eq domain (domain-of x)))
	  (error "Can't union incompatible domains.")))
    (simplify
     (make-universal-quantified-set domain (list (list var set))
                                    (cons expr rest-exprs)))))

;; The following function examines each element of EXPRS to see if
;; they contain any bound variables.  If so, its bound variables are
;; merged with BOUND-VARS.  Two values are returned the list of bound
;; variables and expressions.  The variable TYPE indicates the type of
;; quantification.
(defun merge-bound-vars (type bound-vars exprs)
  (let ((new-exprs nil))
    (flet ((merge-new-bv (var set)
	     (loop for (v) in bound-vars
		   do (when (ge-equal var v)
			(error "Don't know how deal with scoping"))
		   finally (push (list var set) bound-vars))))
      (loop for expr in exprs
	    do (setq expr (simplify expr))
            (cond ((not (typep expr type))
                   (push expr new-exprs))
                  (t (loop for (var set) in (bound-vars-of expr)
                           do (merge-new-bv var set))
                     (setq new-exprs (append new-exprs (exprs-of expr))))))
      (values bound-vars new-exprs))))       
  
(defmethod simplify ((set universal-quantified-set))
  (multiple-value-bind (bv exprs)
      (merge-bound-vars 'universal-quantified-set
                        (bound-vars-of set)
                        (exprs-of set))

    (make-universal-quantified-set (domain-of set) bv exprs)))

;; Different kernels

(defgeneric different-kernels (expression kernels)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((expression numeric) (kernels list))
    (declare (ignore expression))
    kernels))

;; If we don't know anything about the object, then its a kernel.
(defmethod different-kernels (exp (kernels list))
  (if (member exp kernels :test #'ge-equal)
      kernels
      (cons exp kernels)))

(defmethod different-kernels ((exp ge-plus) (kernels list))
  (loop for term in (terms-of exp)
	do (setq kernels (different-kernels term kernels)))
  kernels)

(defmethod different-kernels ((exp ge-times) (kernels list))
  (loop for term in (terms-of exp)
	do (setq kernels (different-kernels term kernels)))
  kernels)

(defmethod different-kernels ((exp ge-expt) (kernels list))
  (if (or (cl:integerp (exponent-of exp))
	  (typep (exponent-of exp) 'rational-integer))
      (different-kernels (base-of exp) kernels)
      (call-next-method)))

(defmethod different-kernels ((exp ge-equation) (kernels list))
  (different-kernels (lhs-of exp)
		     (different-kernels (rhs-of exp) kernels)))

(defmethod different-kernels ((exp list) (kernels list))
  (loop for e in exp
	do (setq kernels (different-kernels e kernels)))
  kernels)

(defmethod substitute (value var expr &rest ignore)
  (declare (ignore value var ignore))
  expr)

(defmethod substitute (value (var symbol) expr &rest ignore)
  (declare (ignore ignore))
  (substitute value (coerce var (domain-of expr)) expr))

(defmethod substitute (value (var ge-variable) (expr number)
                       &rest ignore)
  (declare (ignore value ignore))
  expr)

(defmethod substitute (value (var ge-variable) (expr numeric)
                       &rest ignore)
  (declare (ignore value ignore))
  expr)

(defmethod substitute (value (var ge-variable) (expr ge-variable) &rest ignore)
  (declare (ignore ignore)) 
  (if (eql var expr) (coerce value (domain-of expr)) expr))

(defmethod substitute
    (value (var ge-function) (expr ge-function) &rest ignore)
  (declare (ignore ignore))
  (if (eql var expr) value expr))

(defmethod substitute (value var (expr ge-plus) &rest ignore)
  (declare (ignore ignore))
  (apply #'%plus (mapcar #'(lambda (q) (substitute value var q))
			 (terms-of expr))))

(defmethod substitute (value var (expr ge-times) &rest ignore)
  (declare (ignore ignore))
  (apply #'%times (mapcar #'(lambda (q) (substitute value var q))
                          (terms-of expr))))

(defmethod substitute (value var (expr ge-expt) &rest ignore)
  (declare (ignore ignore))
  (expt (substitute value var (base-of expr))
	(substitute value var (exponent-of expr))))

(defmethod substitute (value (var ge-variable) (expr ge-application)
                       &rest ignore)
  (declare (ignore ignore))
  (apply (funct-of expr)
         (mapcar #'(lambda (q) (substitute value var q))
                 (args-of expr))))

;; FIXTHIS Bummed to make finite element work......
(defvar *fem-kludge* nil)

(defmethod substitute (value (var ge-function) (expr ge-application)
                       &rest ignore)
  (declare (ignore ignore))
  (let ((name (name-of var))
	(funct (funct-of expr)))
    (if *fem-kludge*
	(cond ((string= name (name-of funct))
	       (if (typep funct 'ge-function-deriv)
		   (apply #'deriv value (derivs-of funct))
		   value))
	      (t (funct-of expr)))
	(apply (cond ((string= name (name-of funct))
		      (if (typep funct 'ge-function-deriv)
			  (apply #'deriv value (derivs-of funct))
			  value))
		     (t (funct-of expr)))
	       (mapcar #'(lambda (q) (substitute value var q))
		       (args-of expr))))))

(defmethod substitute (value var (expr ge-equation) &rest ignore)
  (declare (ignore ignore))
  (make-instance (class-of expr)
		 :domain (domain-of expr)
		 :lhs (substitute value var (lhs-of expr))
		 :rhs (substitute value var (rhs-of expr))))

(defgeneric expand (expression)
  (:documentation
   "The purpose of this method is unknown.")
  (:method ((expression t)) expression))

(defun expand-product1 (terms)
  (flet ((expand-plus (term rest)
	   (loop with expanded-terms = (expand-product1 rest)
		 for x in (terms-of term)
		 append (loop for et in expanded-terms
			      collect (cons x et))))
	 (expand-other (term rest)
	   (loop for et in (expand-product1 rest)
		 collect (cons term et))))
    (cond ((null terms)
	   (list ()))
	  ((ge-plus? (first terms))
	   (expand-plus (first terms) (rest terms)))
	  ((ge-expt? (first terms))
	   (let ((temp (expand (first terms))))
		(cond ((ge-plus? temp)
		       (expand-plus temp (rest terms)))
		      (t (expand-other temp (rest terms))))))
	  (t (expand-other (first terms) (rest terms))))))

(defun expand-product (exp)
  (loop for list in (expand-product1 (terms-of exp))
	collect (simp-times-terms (domain-of exp) list)))

(defmethod expand ((exp ge-times))
  (let ((forms (expand-product exp)))
    (if (null (rest forms))
	(first forms)
	(simp-plus-terms (domain-of exp) forms))))

(defmethod expand ((exp ge-plus))
  (let ((expanded-terms nil)
	(terms (terms-of exp))
	term)
    (loop while terms do
          (setq term (expand (pop terms)))
          (if (ge-plus? term)
              (setq terms (append terms (terms-of term)))
              (push term expanded-terms)))
    (simp-plus-terms (domain-of exp) expanded-terms)))

(defun expand-binomial-form (terms n)
  (cond ((null (rest terms))
	 (list (expt (first terms) n)))
	(t (let ((sum ())
		 (a (first terms))
		 (b (rest terms)))
	     (loop for i below (1+ n)
		   for coef = (combinations n i)
		   do (loop for term in (expand-binomial-form b i)
			    do (push (* coef (expt a (- n i)) term) sum)))
	     sum))))

(defmethod expand ((exp ge-expt))
  (let ((base (base-of exp))
	(exponent (exponent-of exp)))
    (cond ((and (ge-plus? base)
		(typep exponent 'rational-integer))
	   (simp-plus-terms (domain-of exp)
                            (expand-binomial-form (terms-of base)
                                                  (integer-value exponent))))
	  ((ge-times? base)
	   (expand (simp-times-terms (domain-of exp)
                                     (loop for term in (terms-of base)
                                           collect (expand (expt term exponent))))))
	  (t exp))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Fourier Transforms
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; fourier.lisp,v 1.3 1994/11/15 19:55:25 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.3")

(defgeneric make-ge-fourier (domain argument svar fvar)
  (:documentation
   "The purpose of this method is not known."))

(defmethod make-ge-fourier ((domain general-expressions) argument svar fvar)
  (make-instance 'ge-fourier :domain domain :argument argument
		 :space-var svar :freq-var fvar))

(defmethod print-object ((expr ge-fourier) stream)
  (format stream "Four{~S, ~S->~S}"
	  (argument-of expr) (space-var-of expr) (freq-var-of expr)))

(defmethod ge-equal ((x ge-fourier) (y ge-fourier))
  (and (ge-equal (argument-of x) (argument-of y))
       (ge-equal (space-var-of x) (space-var-of y))
       (ge-equal (freq-var-of x) (freq-var-of y))))

(defmethod ge-great ((x ge-fourier) (y ge-fourier))
  (cond ((ge-great (argument-of x) (argument-of y)) t)
	((ge-great (argument-of y) (argument-of x)) nil)
	((ge-great (space-var-of x) (space-var-of y)) t)	
	((ge-great (space-var-of y) (space-var-of x)) nil)
	((ge-great (freq-var-of x) (freq-var-of y)) t)))

(defgeneric ge-fourier (exp svar fvar)
  (:documentation
   "The purpose of this method is unknown.")
  (:method (exp svar fvar)
    (declare (ignore fvar))
    (error "Don't know how to take the Fourier transform of ~S wrt ~S"
           exp svar)))

(defmethod ge-fourier ((exp general-expression) (svar symbol) (fvar symbol))  
  (ge-fourier exp (coerce svar (domain-of exp)) (coerce fvar (domain-of exp))))

(defmethod ge-fourier (exp (svar ge-variable) (fvar ge-variable))
  (make-ge-fourier (domain-of svar) (coerce exp (domain-of svar)) svar fvar))

(defmethod ge-fourier ((exp numeric) (svar ge-variable) (fvar ge-variable))
  exp)

(defmethod ge-fourier ((exp ge-variable) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (cond ((ge-equal exp svar) fvar)
	  ((depends-on? exp svar)
	   (make-ge-fourier domain exp svar fvar))
	  (t exp))))

(defmethod ge-fourier ((exp ge-plus) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (cond ((and (eql domain (domain-of svar))
		(eql domain (domain-of fvar)))
	   (call-next-method))
	  (t (simplify
	      (make-ge-plus domain
                            (loop for x in (terms-of exp)
                                  collect (ge-fourier x svar fvar))))))))

(defmethod ge-fourier ((exp ge-times) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp))
	terms depend-term free-terms)
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (setq terms (terms-of exp))
    (loop for term in terms
          do (when (depends-on? term svar)
               (cond ((null depend-term)
                      (setq depend-term term))
                     (t (return (setq free-terms :non-linear)))))
          finally (setq free-terms
                        (remove depend-term terms)))
    (cond ((eql free-terms :non-linear)
           (make-ge-fourier domain exp svar fvar))
          ((null depend-term)
           exp)
          (t (simplify
              (make-ge-times domain
                             (cons (ge-fourier depend-term svar fvar)
                                   free-terms)))))))

#+ignore
(defmethod ge-fourier ((exp ge-deriv) (svar ge-variable) (fvar ge-variable))
  (let ((domain (domain-of exp)))
    (unless (and (eql domain (domain-of svar))
		 (eql domain (domain-of fvar)))
      (error "Taking Fourier transform from different domains"))
    (loop for entry in (varlist-of exp)
          with varlist
          do (when (ge-equal svar (first entry))
               (setq varlist (remove entry (varlist-of exp)))
               (return
                 (simplify
                  (* (expt fvar (second entry))
                     (if (null varlist)
                         (ge-fourier (argument-of exp) svar fvar)
                         (make-ge-deriv domain
                                        (ge-fourier (argument-of exp) svar fvar)
                                        varlist))))))
          finally
          (return 
            (simplify
             (make-ge-deriv domain
                            (ge-fourier exp svar fvar)
                            (varlist-of exp)))))))

(defgeneric fourier (expression &rest variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod fourier ((exp number) &rest vars)
  (declare (ignore vars))
  (make-element *general* exp))

(defmethod fourier ((exp numeric) &rest vars)
  (declare (ignore vars))
  exp)

(defmethod fourier ((exp symbol) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for (sv fv) on vars by #'cddr
	do (setq exp (ge-fourier exp (coerce sv *general*)
                                 (coerce fv *general*))))
  exp)

(defmethod fourier ((exp general-expression) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for (sv fv) on vars by #'cddr
	do (setq exp (ge-fourier exp (coerce sv *general*)
                                 (coerce fv *general*))))
  exp)

;; Inverse Fourier Transforms

(defgeneric make-ge-ifourier (domain argument svar fvar)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-ge-ifourier ((domain general-expressions) argument svar fvar)
  (make-instance 'ge-ifourier :domain domain :argument argument
		 :space-var svar :freq-var fvar))

(defmethod print-object ((expr ge-ifourier) stream)
  (format stream "IFour{~S, ~S->~S}"
	  (argument-of expr) (space-var-of expr) (freq-var-of expr)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Special Functions 
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; functions.lisp,v 1.2 1994/11/15 19:55:26 rz Exp


(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.2")

;; Symbolic Lambda expressions

(defun make-app-function (args body)
  (let* ((domain (domain-of body))
         (old-vars (mapcar #'(lambda (x) (coerce x domain)) args))
         (new-vars nil))
    (loop for i upfrom 1
          for v in old-vars
          for new-var =  (coerce (intern (format nil "V.~D" i)) domain)
          do (cond ((ge-equal new-var v)
                    (push v new-vars))
                   (t (setq body (substitute new-var v body))
                      (push new-var new-vars))))
    (setq new-vars (reverse new-vars))
    (when (typep body 'general-expression)
      (setq body (expand body)))
    (cond ((and (typep body 'ge-application)
		(loop for v in new-vars
		      for a in (args-of body)
		      do (when (not (ge-equal a v))
			   (return nil))
		      finally (return t)))
	   (funct-of body))
	  (t (make-instance 'applicable-function 
			    :domain domain
			    :bound-vars new-vars
			    :nargs (length args)
			    :body body)))))

(defmethod print-object ((fun applicable-function) stream)
  (format stream "(lambda ~S ~S)" (bound-vars-of fun) (body-of fun)))

(defmethod apply ((fun applicable-function) &rest args)
  (setq args (accum-apply-args args))
  (unless (eql (nargs-of fun) (length args))
    (error "Argument lengths don't match"))
  (let* ((body (body-of fun))
         (domain (domain-of body)))
    (loop for arg in args
          for v in (bound-vars-of fun)
          do (setq body (substitute (coerce arg domain) v body)))
    body))

(defun canonicalize-functions (x y)
  (unless (eql (nargs-of x) (nargs-of y))
    (error "Two functions have different number of arguments"))
  (let ((x-body (body-of x))
        (y-body (body-of y)))
    (loop for x-arg in (bound-vars-of x)
          for y-arg in (bound-vars-of y)
          do (unless (ge-equal x-arg y-arg)
               (setq y-body (substitute x-arg y-arg y-body))))
    (values (bound-vars-of x) x-body y-body)))

;;; FIXME : The macro requires substantial auditing.
(defmacro define-applicable-function-binary (operator)
  `(progn
    (defmethod ,operator ((x applicable-function) y)
      (make-app-function (bound-vars-of x) (,operator (body-of x) y)))

    (defmethod ,operator (x (y applicable-function))
      (make-app-function (bound-vars-of y) (,operator x (body-of y))))

    (defmethod ,operator ((x applicable-function) (y domain-element))
      (make-app-function (bound-vars-of x) (,operator (body-of x) y)))

    (defmethod ,operator ((x domain-element) (y applicable-function))
      (make-app-function (bound-vars-of y) (,operator x (body-of y))))

    (defmethod-sd ,operator ((x applicable-function) (y applicable-function))
      (multiple-value-bind (args x-body y-body) (canonicalize-functions x y)
        (make-app-function args (,operator x-body y-body))))

    ;; FIXTHIS: Why do the following two methods need to be :around methods?
    ;; The :around is only needed for MCL!!
    (defmethod ,operator #+MCL :around ((x applicable-function) (y ge-function)) 
               (unless (= (nargs-of x) (nargs-of y))
                 (error "Can't add two functions with different arity: ~S ~S"
                        x y))
               (make-app-function (bound-vars-of x)
                                  (,operator (body-of x) (apply y (bound-vars-of x)))))

    (defmethod ,operator #+MCL :around ((y ge-function) (x applicable-function)) 
               (unless (= (nargs-of x) (nargs-of y))
                 (error "Can't add two functions with different arity: ~S ~S"
                        x y))
               (make-app-function (bound-vars-of x)
                                  (,operator (body-of x) (apply y (bound-vars-of x)))))

    (defmethod ,operator ((x ge-function) y) 
      (make-app-function '(%temp%) (,operator (funcall x '%temp%) y)))

    ;; This is needed because of precidence problems.
    ;; (number domain-elt) comes before (t ge-function)
    (defmethod ,operator (x (y ge-function))
      (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

    (defmethod ,operator ((x number) (y ge-function))
      (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

    (defmethod ,operator ((x numeric) (y ge-function))
      (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

    (defmethod-sd ,operator ((x ge-function) (y domain-element))
      (make-app-function '(%temp%) (,operator (funcall x '%temp%) y)))

    (defmethod-sd ,operator ((x domain-element) (y ge-function))
      (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

    (defmethod-sd ,operator ((x ge-function) (y ge-function))
      (unless (= (nargs-of x) (nargs-of y))
        (error "Can't combine two functions with different arity: (~S ~S ~S)"
               ',operator x y))
      (make-app-function '(%temp%)
                         (,operator (funcall x '%temp%) (funcall  y '%temp%))))))

(defmethod 0? ((x applicable-function))
  (0? (body-of x)))

(defmethod 1? ((x applicable-function))
  (1? (body-of x)))

(define-applicable-function-binary plus)
(define-applicable-function-binary difference)

(defmethod minus ((x ge-function))
  (make-app-function '(%temp%) (minus (funcall x '%temp%))))

(defmethod minus ((x applicable-function))
  (make-app-function (bound-vars-of x) (minus (body-of x))))

(define-applicable-function-binary times)
(define-applicable-function-binary quotient)

(defmethod recip ((x ge-function))
  (make-app-function '(%temp%) (recip (funcall x '%temp%))))

(defmethod recip ((x applicable-function))
  (make-app-function (bound-vars-of x) (recip (body-of x))))

(defmethod expt ((x applicable-function) (y number))
  (make-app-function (bound-vars-of x) (expt (body-of x) y)))

(defmethod expt ((x applicable-function) (y symbol))
  (make-app-function (bound-vars-of x) (expt (body-of x) y)))

(defmethod-sd expt ((x applicable-function) (y domain-element))
  (if (not (typep y 'abstract-function))
      (make-app-function (bound-vars-of x) (expt (body-of x) y))
      (call-next-method)))

(defmethod expt ((x ge-function) (y number))
  (make-app-function '(%temp%) (expt (funcall x '%temp%) y)))

(defmethod expt ((x ge-function) (y symbol))
  (make-app-function '(%temp%) (expt (funcall x '%temp%) y)))

(defmethod-sd expt ((x ge-function) (y domain-element))
  (if (not (typep y 'abstract-function))
      (make-app-function '(%temp%) (expt (funcall x '%temp%) y))
      (call-next-method)))

(defmethod deriv ((exp applicable-function) &rest vars)
  (make-app-function
   (bound-vars-of exp)
   (apply
    #'deriv
    (cons (body-of exp)
          (loop for v in vars
                collect (cond ((typep v 'symbol) v)
                              ((typep v 'ge-variable) v)
                              ((and (typep v 'integer)
                                    (not (minusp v))
                                    (< v (nargs-of exp)))
                               (elt (bound-vars-of exp) v))
                              (t (error "Cannot take deriv of ~A and ~A"
                                        exp v))))))))

;; Special functions

(defmacro def-ge-1oper (name (arg))
  (let ((maker-name (intern (format nil "MAKE-GE-~A" (string name))))
        (predicate-name (intern (format nil "GE-~A?" (string name)))))
    `(progn
      (make-function nil ',name 1)
      (defun ,maker-name (domain ,arg)
        (make-ge-funct domain (make-function domain ',name 1) ,arg))
      (defun ,predicate-name (arg)
        (and (ge-application? arg)
             (ge-function? (funct-of arg))
             (string-equal (name-of (funct-of arg)) ,(string name))))
      (defmethod ,name ((,arg symbol))
        (simplify (,maker-name *general* (coerce ,arg *general*))))
      (defmethod ,name ((,arg numeric))
        (let* ((arg ,arg)
               domain)
          (cond ((typep (domain-of arg) 'general-expressions)
                 (setq domain (domain-of arg)))
                (t (setq domain *general*)
                   (setq arg (coerce arg domain))))
          (simplify (,maker-name domain arg))))
      (defmethod ,name ((,arg general-expression))
        (simplify (,maker-name (domain-of ,arg) ,arg)))
      )))

(defmacro defsimplify-funct (name args &body body)
  (let ((simp-name (intern (format nil "SIMPF-~A" name))))
    `(progn
      (defun ,simp-name ,args ,@body)
      (setf (getf (make-function nil ',name) 'simplify) ',simp-name))))

(defmacro defderiv-funct (name &body body)
  (let ((fun-name (intern (format nil ".~A-deriv." name))))
    `(progn
      (defun ,fun-name ()
        (setf (getf (make-function nil ',name) 'deriv) 
              (list ,@body)))
      (pushnew ',fun-name *initialize-contexts-funs*))))

;;; FIXME : Track down the def-ge-1oper macro and generic function
;;; definition.
(def-ge-1oper ABS (x))
(def-ge-1oper REALPART (x))
(def-ge-1oper IMAGPART (x))

(defsimplify-funct realpart (domain whole exp)
  (declare (ignore domain))
  (cond ((or (ge-abs? exp) 
             (ge-realpart? exp)
             (ge-imagpart? exp))
         exp)
        (t whole)))

(defsimplify-funct imagpart (domain whole exp)
  (cond ((or (ge-abs? exp) 
             (ge-realpart? exp)
             (ge-imagpart? exp))
         (zero domain))
        (t whole)))

(def-ge-1oper LOG (x))

(defsimplify-funct log (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:log exp)))
        ((ge-expt? exp)
         (simplify
          (make-ge-times domain
                         `(,(exponent-of exp)
                           ,(make-ge-log (domain-of exp) (base-of exp))))))
        (t whole)))

(defderiv-funct log
  (make-app-function '(x) (expt 'x -1)))

(def-ge-1oper SIN (x))

(defsimplify-funct sin (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:sin exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-sin domain (- exp))))
        (t whole)))

(defderiv-funct sin
  (make-app-function  '(x) (cos 'x)))

(def-ge-1oper COS (x))

(defsimplify-funct cos (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:cos exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 1))
        ((ge-minus? exp)
         (make-ge-cos domain (- exp)))
        (t whole)))

(defderiv-funct cos
  (make-app-function '(x) (- (sin 'x))))

(def-ge-1oper TAN (x))

(defsimplify-funct tan (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:tan exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-tan domain (- exp))))
        (t whole)))

(defderiv-funct tan
  (make-app-function '(x) (- (expt (cos 'x) -2))))

(def-ge-1oper ASIN (x))

(defsimplify-funct asin (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:asin exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-asin domain (- exp))))
        (t whole)))

(defderiv-funct asin
  (make-app-function '(x) (- (expt (- 1 (expt 'x 2)) -1/2))))

(def-ge-1oper ACOS (x))

(defsimplify-funct acos (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:acos exp)))
        (t whole)))

(defderiv-funct acos
  (make-app-function '(x) (expt (- 1 (expt 'x 2)) -1/2)))

;;; FIXME : Correct the custom version of ATAN to account for 2
;;; arguments.
;; These are conditionalized out since, we might want to have a two
;; argument version of atan!.
#+FIXTHIS
(def-ge-1oper ATAN (x))
#+FIXTHIS
(defsimplify-funct atan (domain whole exp)
  (cond ((cl:floatp exp) (cl:atan exp))
	  (t `(atan ,exp))))

#+FIXTHIS
(defderiv-funct atan
  (make-app-function '(x) (/ (+ 1 (expt 'x 2)))))

(def-ge-1oper SINH (x))

(defsimplify-funct sinh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:sinh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-sinh domain (- exp))))
        (t whole)))

(defderiv-funct sinh 
  (make-app-function '(x) (cosh 'x)))

(def-ge-1oper COSH (x))

(defsimplify-funct cosh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:cosh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 1))
        ((ge-minus? exp)
         (make-ge-cosh domain (- exp)))
        (t whole)))

(defderiv-funct cosh
  (make-app-function '(x) (sinh 'x)))

(def-ge-1oper TANH (x))

(defsimplify-funct tanh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:tanh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-tanh domain (- exp))))
        (t whole)))

(defderiv-funct tanh
  (make-app-function '(x) (expt (cosh 'x) -2)))

(def-ge-1oper ASINH (x))

(defsimplify-funct asinh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:asinh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-asinh domain (- exp))))
        (t whole)))

(defderiv-funct asinh 
  (make-app-function '(x) (expt (+ 1 (expt 'x 2)) -1/2)))

(def-ge-1oper ACOSH (x))

(defsimplify-funct acosh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:acosh exp)))
        (t whole)))

(defderiv-funct acosh 
  (make-app-function '(x) (expt (+ 1 (expt 'x 2)) -1/2)))

#+FIXTHIS
(def-ge-1oper ATANH (x))
#+FIXTHIS
(defsimplify atanh (domain whole exp)
  (cond ((cl:floatp exp) (cl:atanh exp))
	  (t `(atanh ,exp))))

#+FIXTHIS
(defderiv-funct atanh
  (make-app-funciton '(x) (expt (- 1 (expt 'x 2)) -1)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Direct Sums
;;; ===========================================================================
;;; (c) Copyright 1991,1993 Cornell University

;;; direct-sums.lisp,v 1.4 1995/05/24 17:41:59 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.4")

(defmethod dimension-of ((domain direct-sum))
  (length (tuple-value domain)))

(defmethod initialize-instance :after ((domain direct-sum) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) domain
    (setf print-function 'direct-sum-print-object)))

(defun direct-sum-print-object (domain stream)
  (%apply #'format stream "~S~@{ (+) ~S~}"
	  (loop with v = (tuple-value domain)
		for i below (array-dimension v 0)
		collect (aref v i))))

(defgeneric %make-direct-sum (domain1 domain2)
  (:documentation
   "The purpose of this method is unknown."))

(defmacro define-direct-sum (domain-name classes
			     &optional other-domain-classes other-elt-classes)
  (let ((ds-domain (intern (format nil "DIRECT-SUM-~A" domain-name)))
	(ds-domain-elt (intern (format nil "DIRECT-SUM-~A-ELT" domain-name))))
    `(progn
      (defclass ,ds-domain 
          (,@(loop for name in classes 
                   collect (intern (format nil "DIRECT-SUM-~A" name)))
             ,domain-name ,@other-domain-classes direct-sum) ())
      (defclass ,ds-domain-elt 
          (,@(loop for name in classes 
                   collect (intern (format nil "DIRECT-SUM-~A-ELT" name))) 
             ,@other-elt-classes direct-sum-element) ())
      (define-domain-element-classes ,ds-domain ,ds-domain-elt)
      (defmethod %make-direct-sum ((a ,domain-name) (b ,domain-name))
        (%make-direct-sum-internal ',ds-domain a b))
      (defmethod make-element ((domain ,ds-domain) elt1 &rest elts)
        (%apply #'make-instance ',ds-domain-elt 
                :domain domain
                :values (cons elt1 elts)))
      (defmethod weyl::make-element ((domain ,ds-domain) elt1 &rest elts)
        (let* ((domains (tuple-value domain))
               (len (array-dimension domains 0)))
          (unless (cl:= (1- len) (length elts))
            (error "Incorrect number of elements to MAKE-ELMENT ~A" domain))
          (make-instance ',ds-domain-elt
                         :domain domain
                         :values (cons (coerce elt1 (aref domains 0))
                                       (loop for i upfrom 1 below len
                                             for elt in elts
                                             collect (coerce elt (aref domains i))))))))))

(defun make-direct-sum* (domain1 &rest domains)
  (when (null domains) 
    (error "Illegal number of arguments to MAKE-DIRECT-SUM"))
  (labels ((iterate (values)
	     (cond ((null (rest values))
		    (first values))
		   (t (%make-direct-sum (first values)
					(iterate (rest values)))))))
    (let ((domain (iterate (cons domain1 domains)))
	  (Z (get-rational-integers)))
      (make-homomorphism Z #'(lambda (x)			     
			       (map-with-domain
                                (first (domain-element-classes domain))
                                domain
                                #'(lambda (d) (coerce x d)) domain))
			 domain)
      domain)))

(defun make-direct-sum (domain1 &rest domains)
  (add-domain #'false 
    (%apply #'make-direct-sum* domain1 domains)))

(defun %make-direct-sum-internal (type a b)
  (flet ((domain-list (a)
	   (loop for i below (dimension-of a)
		 collect (ref a i))))
    (cond ((typep a 'direct-sum)
	   (if (typep b 'direct-sum)
	       (make-instance type :values (nconc (domain-list a) (domain-list b)))
	       (make-instance type :values (nconc (domain-list a) (list b)))))
	  ((typep b 'direct-sum)
	   (make-instance type :values (cons a (domain-list b))))
	  (t (make-instance type :values (list a b))))))

(defun get-direct-sum (domain1 &rest domains)
  (add-domain #'(lambda (d) 
		  (and (typep d 'direct-sum)
		       (eql domain1 (ref d 0))
		       (cl:= (1- (dimension-of d)) (length domains))
		       (loop for i below (dimension-of d)
			     for dom in domains
			     when (not (eql (ref d i) dom))
                             do (return nil)
			     finally (return t))))
    (%apply #'make-direct-sum* domain1 domains)))

(defmethod print-object ((domain direct-sum-element) stream)
  (%apply #'format stream "~S~@{ (+) ~S~}"
	  (loop with v = (tuple-value domain)
		for i below (array-dimension v 0)
		collect (aref v i))))

(define-direct-sum semigroup ())

(defmethod-sd times ((a direct-sum-semigroup-elt) (b direct-sum-semigroup-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'times a b))

(define-direct-sum monoid (semigroup))

(defmethod one ((domain direct-sum-monoid))
  (map 'direct-sum-monoid-elt #'one domain))

(defmethod 0? ((x direct-sum-monoid-elt))
  (let ((v (tuple-value x)))
    (loop for i below (array-dimension v 0)
	  when (not (0? (aref v i)))
          do (return nil)
	  finally (return t))))

(define-direct-sum group (monoid))

(defmethod-sd quotient ((a direct-sum-group-elt) (b direct-sum-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'quotient a b))

(defmethod recip ((a direct-sum-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt (domain-of a) #'recip a))

(define-direct-sum abelian-semigroup ())

(defmethod-sd plus ((a direct-sum-semigroup-elt) (b direct-sum-semigroup-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'plus a b))

(define-direct-sum abelian-monoid (abelian-semigroup))

(defmethod zero ((domain direct-sum-monoid))
  (map 'direct-sum-monoid-elt #'zero domain))

(defmethod 1? ((x direct-sum-monoid-elt))
  (let ((v (tuple-value x)))
    (loop for i below (array-dimension v 0)
	  when (not (1? (aref v i)))
          do (return nil)
	  finally (return t))))

(define-direct-sum abelian-group (abelian-monoid))

(defmethod-sd difference ((a direct-sum-abelian-group-elt) (b direct-sum-abelian-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt domain #'difference a b))

(defmethod minus ((a direct-sum-abelian-group-elt))
  (map-with-domain 'direct-sum-semigroup-elt (domain-of a) #'minus a))

(define-direct-sum module (abelian-group) (has-coefficient-domain))

(define-direct-sum algebra (module semigroup))

(define-direct-sum ring (algebra monoid))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

;; bigfloat.lisp,v 1.3 1994/10/21 18:16:34 rz Exp

;; Arbitrary Precision Real Arithmetic System
;;  by Tateaki  Sasaki
;;  The UNIVERSITY OF UTAH,  MARCH 1979

;; For design philosophy and characteristics of this system, see T. Sasaki, 
;; ``An Arbitrary Precision Real Arithmetic Package In Reduce,''
;;  Proceedings of Eurosam '79, Marseille (FRANCE), June 1979.

;; For Implementation notes and using this system, see T. Sasaki, 
;; ``Manual For Arbitrary Precision Real Arithmetic System in Reduce,' 
;; Operating Report of Utah Symbolic Computation Group

;;  In order to speed up this system, you have only to rewrite four
;; routines (DECPREC!, INCPREC!, PRECI!, AND ROUND!LAST)
;; machine-dependently.

;;  This function constructs an internal representation of a number
;; 'n' composed of the mantissa MT and the exponent EP with the base
;; 10.  The magnitude of the number thus constructed is hence
;; MT*10^EP.

;; **** CAUTION!  MT and EP are integers.  So, EP denotes the order of
;;                the last figure in 'N', WHERE order(N)=K if 10**K <=
;;                ABS(N) < 10**(K+1), with the exception order(0)=0. 
;;

;; The number 'n' is said to be of precision 'k' if its mantissa is a
;; k-figure number.  MT and EP are any integers (positive or
;; negative).  So, you can handle any big or small numbers.  in this
;; sense, 'BF' denotes a big-floating-point number.  Hereafter, an
;; internal representation of a number constructed by MAKE-BIGFLOAT is
;; referred to as a big-float representation.
 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.3")

;;; FIXME : The precision should be specified in a class allocated
;;; slot.
(eval-when (:compile-toplevel :load-toplevel)
  (proclaim '(special *real-precision*)))

(defsubst make-bigfloat (domain mantissa exponent)
  (make-instance 'bigfloat :domain domain
		 :mantissa mantissa :exponent exponent))

;; This function returns t if x is a big-float representation, else it
;; returns NIL.
(defun bigfloatp (x)
  (eql (class-name (class-of x)) 'bigfloat))

;;; Routines For Converting A Big-Float Number
 
;; This function converts a number N to an equivalent number the
;; precision of which is decreased by K.
(defun decprec! (number k)
  (with-slots (exponent mantissa) number
    (make-bigfloat *domain*
		   (cl:truncate mantissa (cl:expt 10 k))
		   (cl:+ exponent k))))
 
;;  This function converts a number N to an equivalent 
;;       number the precision of which is increased by K.
(defun incprec! (number k)
  (with-slots (exponent mantissa) number
    (make-bigfloat *domain*
		   (cl:* mantissa (cl:expt 10 k))
		   (cl:- exponent k))))
 
;; This function converts a number to an equivalent number of precision
;; k by rounding or adding '0's.
(defun conv!mt (number k)
  (unless (bigfloatp number)
    (error "Expected ~S to be a bigfloat" number))
  (unless (and (integerp k) (cl:> k 0))
    (error "Expected ~S to be a positive integer" k))
  (cond ((cl:zerop (setq k (cl:- (preci! number) k)))
	 number)
	((cl:< k 0) (incprec! number (cl:- k)))
	(t (round!last (decprec! number (1- k))))))
 
;; This function converts a number 'n' to an equivalent number having
;; the exponent k by rounding 'n' or adding '0's to 'n'.
(defun conv!ep (nmbr k)
  (unless (bigfloatp nmbr) 
    (error "Invalid argument to conv!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid second argument to conv!ep: ~S" k))
  (cond ((cl:zerop (setq k (cl:- k (bigfloat-exponent nmbr))))
	 nmbr)
	((cl:< k 0) (incprec! nmbr (cl:- k)))
	(t (round!last (decprec! nmbr (cl:- k 1))))))
 
;; This function returns a given number N unchanged if its precision
;; is not greater than K, else it cuts off its mantissa at the (K+1)th
;; place and returns an equivalent number of precision K.
;;  **** CAUTION!  NO ROUNDING IS MADE.                  
(defun cut!mt (nmbr k)
  (declare (fixnum k))
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!mt: ~S" nmbr))
  (unless (and (integerp k) (cl:> k 0))
    (error "Invalid precision to cut!mt: ~D" k))  
  (if (cl:plusp (setq k (cl:- (preci! nmbr) k)))
      (decprec! nmbr k)
      nmbr)) 

;; This function returns a given number N unchanged if its exponent is
;; not less than K, else it cuts off its mantissa and returns an
;; equivalent number of exponent K.
;;  **** CAUTION!  NO ROUNDING IS MADE.                  
(defun cut!ep (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid precision to cut!ep: ~D" k))  
  (if (not (cl:> (setq k (cl:- k (bigfloat-exponent nmbr))) 0))
      nmbr
      (decprec! nmbr k)))
 
;; This function counts the precision of a bigfloat 'n'.
;;
;; FIXTHIS:  The 1+ below is total kludge.  This whole package needs
;; to be rewritten sometime soon.  
(defun preci! (nmbr)
  (let* ((mt (1+ (cl:abs (bigfloat-mantissa nmbr))))
	 (len (integer-length mt)))
    (values
     (if (cl:< len 10)
	 (cl:ceiling (cl:log mt 10))
	 (cl:ceiling
	  (cl:+ (cl:/ (cl:+ len -10) #.(cl:log 10 2))
                (cl:log (cl:ash mt (cl:- 10 len)) 10)))))))
 
;; This function counts the order of a bigfloat 'n'.
;; **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1) 
;; ****     WHEN N IS NOT 0, AND ORDER(0)=0.      
(defun order! (nmbr)
  (if (cl:zerop (bigfloat-mantissa nmbr)) 0
      (cl:+ (preci! nmbr) (bigfloat-exponent nmbr) -1)))

(defun convert-number->characters (number)
  (if (cl:zerop number) (list #\0)
      (let ((chars ()))
	(loop with n = number
	      and digit
	      while (not (cl:zerop n))
	      do (multiple-value-setq (n digit) (cl:truncate n 10))
              (push (digit-char digit) chars))
	chars)))

;; This function rounds a number N at its last place.
(defun round!last (nmbr)
  (let ((abs-nmbr (cl:abs (bigfloat-mantissa nmbr)))
	n)
    (setq n (if (cl:< (rem abs-nmbr 10) 5)
		(cl:truncate abs-nmbr 10)
		(1+ (cl:truncate abs-nmbr 10))))
    (if (cl:minusp (bigfloat-mantissa nmbr)) (setq n (cl:- n)))
    (make-bigfloat *domain*  n (cl:1+ (bigfloat-exponent nmbr)))))

;; This function rounds a number N at the (K+1)th place and returns an
;; equivalent number of precision K if the precision of N is greater
;; than K, else it returns the given number unchanged.
(defun round!mt (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to round!mt: ~S" nmbr))
  (unless (and (integerp k) (not (cl:minusp k)))
    (error "Invalid precision to round!mt: ~D" k))  
  (cond ((cl:minusp (setq k (cl:- (preci! nmbr) k 1)))
	 nmbr)
	((equal k 0) (round!last nmbr))
	(t (round!last (decprec! nmbr k)))))

;; This function rounds a number N and returns an equivalent number
;; having the exponent K if the exponent of N is less than K, else it
;; returns the given number unchanged.
(defun round!ep (nmbr k)
  (unless (bigfloatp nmbr)
    (error "Invalid argument to cut!ep: ~S" nmbr))
  (unless (integerp k)
    (error "Invalid precision to cut!ep: ~D" k))  
  (cond ((cl:< (setq k (cl:- (cl:1- k) (bigfloat-exponent nmbr))) 0)
	 nmbr)
	((equal k 0) (round!last nmbr))
	(t (round!last (decprec! nmbr k)))))

(defmethod print-object ((number bigfloat) stream)
  (setq number (round!mt number (cl:- *REAL-PRECISION* 2)))
  (with-slots (mantissa exponent) number 
    (let ((u (convert-number->characters (cl:abs mantissa)))
	  k)
      (flet ((bfprin1 (u)
	       (when (cl:minusp mantissa)
		 (push #\- u))
	       ;; Suppress trailing zeroes
	       (loop for v on (reverse u)
		     while (and (char= (first v) #\0)
				(if (rest v) (char/= (second v) #\.)
				    t))
		     finally (setq u (nreverse v)))
	       ;; Now print the number
	       (loop for char in u
		     do (write-char char stream))))
	(or (rest u) (push #\0 (rest u)))
	(push #\. (rest u))
	(bfprin1 u)
	(princ "B" stream)
	(setq u (convert-number->characters (cl:abs (setq k (order! number)))))
	(setq u (cons (if (cl:< k 0) #\- #\+)
		      u))
	(loop for char in u
	      do (write-char char stream))
	number))))

;;; Routines for reading/printing numbers
 
;; This function reads a long number N represented by a list in a way
;; described below, and constructs a big-float representation of N.
;; Using this function, you can input any long floating-point numbers
;; without difficulty.  L is a list of integers, the first element of
;; which gives the order of N and all the next elements when
;; concatenated give the mantissa of N.
;;    **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1).

(defun read!lnum (l)
  (loop for q in l
	unless (integerp q)
        do (error "Invalid argument to read!lnum: ~S" q))
  (loop for term in (rest l)
	for k = (cl:ceiling (integer-length term) #.(cl:log 10 2))
	with mt = 0
	and ep = (1+ (first l))
	do (setq mt (cl:+ (cl:* mt (cl:expt 10 k)) (cl:abs term)))
        (setq ep (cl:- ep k))
	finally (return 
		  (make-bigfloat *domain*
                                 (if (cl:plusp (second l)) mt (cl:- mt))
                                 ep))))

;; This function reads a long number N represented by a list in a way
;; described below, and constructs a big-float representation of N.
;; Using this function, you can input any long floating-point numbers
;; without difficulty.  L is a list of integers, the first element of
;; which gives the order of N and all the next elements when
;; concatenated give the mantissa of N.
;;  **** ORDER(N)=K IF 10**K <= ABS(N) < 10**(K+1).       

(defun read!num (n)
  (let ((exponent 0))
    (multiple-value-bind (integer j)
	(parse-integer n :junk-allowed t)
      (unless integer
	(setq integer 0))
      (cond ((char= (aref n j) #\.)
	     (multiple-value-bind (fraction i)
		 (parse-integer n :start (1+ j) :junk-allowed t)
	       (setq integer (cl:+ (cl:* integer
                                         (cl:expt 10 (cl:- i j 1)))
                                   fraction))
	       (decf exponent (cl:- i j 1)))))
      (make-bigfloat *domain* integer exponent))))

(defgeneric convert-to-bigfloat (number)
  (:documentation
   "Return the bigfloat representation of the number."))

(defmethod convert-to-bigfloat ((x rational-integer))
  (make-bigfloat (domain-of x) (integer-value x) 0))

(defmethod convert-to-bigfloat ((x rational-number))
  (let ((domain (domain-of x)))
    (/ (make-bigfloat domain (qo-numerator x) 0)
       (make-bigfloat domain (qo-denominator x) 0))))

(defmethod convert-to-bigfloat ((x floating-point-number))
  (let ((domain (domain-of x))
	(float (fp-value x)))
    (multiple-value-bind (mantissa expt sign) (integer-decode-float float)
      (if (cl:minusp expt)
	  (/ (make-bigfloat domain (cl:* mantissa sign) 0)
	     (make-bigfloat domain (cl:expt (float-radix float) (cl:- expt)) 0))
	  (* (make-bigfloat domain (cl:* mantissa sign) 0)
	     (make-bigfloat domain (cl:expt (float-radix float) expt) 0))))))

;;;  Arithmetic manipulation routines

(defun bf-abs (nmbr)
  (if (cl:> (bigfloat-mantissa nmbr) 0) nmbr
      (make-bigfloat *domain*
                     (cl:- (bigfloat-mantissa nmbr))
                     (bigfloat-exponent nmbr))))

(defun bf-minus (nmbr)
  (make-bigfloat *domain*
                 (cl:- (bigfloat-mantissa nmbr))
                 (bigfloat-exponent nmbr)))
 
(defun bf-plus (n1 n2)
  (let ((e1 (bigfloat-exponent n1)) (e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:+ (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          e1))
	  ((cl:> e1 e2)
	   (make-bigfloat *domain*
                          (cl:+ (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                                (bigfloat-mantissa n2))
                          e2))
	  (t (make-bigfloat *domain*
                            (cl:+ (bigfloat-mantissa n1)
                                  (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
                            e1))))) 

(defun bf-difference (n1 n2)
  (let ((e1 (bigfloat-exponent n1)) (e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          e1))
	  ((cl:> e1 e2)
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                                (bigfloat-mantissa n2))
                          e2))
	  (t 
	   (make-bigfloat *domain*
                          (cl:- (bigfloat-mantissa n1)
                                (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
                          e1)))))

(defun bf-times (n1 n2)
  (make-bigfloat *domain*
                 (cl:* (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                 (cl:+ (bigfloat-exponent n1) (bigfloat-exponent n2)))) 
 
(defun bf-quotient (n1 n2 k)
  (round!mt
   (with-slots (mantissa exponent) (conv!mt n1 (cl:+ k (preci! n2) 1))
     (make-bigfloat *domain*
                    (cl:truncate mantissa (bigfloat-mantissa n2))
                    (cl:- exponent (bigfloat-exponent n2))))
   k)) 
 
;; This function calculates the kth power of 'n'. The result will
;; become a long number if abs(k) >> 1.                             
(defun bf-expt (number k precision)
  (if (cl:< k 0)
      (/ (make-bigfloat *domain* 1 0)
	 (bf-expt number (cl:- k) precision))
      (%funcall (repeated-squaring
                 #'(lambda (a b) (round!mt (bf-times a b) precision))
                 (make-bigfloat *domain* 1 0))
                number k)))

;; This function calculates the integer quotient of 'n1' and 'n2',
;; just as the quotient" for integers does.
(defun bf-floor (n1 n2)
  (let ((e1 (bigfloat-exponent n1))
	(e2 (bigfloat-exponent n2)))
    (cond ((cl:= e1 e2)
	   (make-bigfloat *domain*
                          (cl:truncate (bigfloat-mantissa n1) (bigfloat-mantissa n2))
                          0))
	  ((cl:> e1 e2)
	   (bf-floor (incprec! n1 (cl:- e1 e2)) n2))
	  (t  (bf-floor n1 (incprec! n2 (cl:- e2 e1)))))))

(defun bf-integer-part (num)
  (with-slots (exponent mantissa) num
    (if (cl:zerop exponent) mantissa
	(cl:* mantissa (cl:expt 10 exponent)))))

;; This returns a lisp integer as its first return value  (perhaps
;; this should be a floating point integer)?

(defgeneric floor1 (number)
  (:documentation
   "Return the quotient truncated towards negative infinity."))

(defmethod floor1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (cut!ep number 0))
      (values (bf-integer-part quo)
	      (bf-difference number quo)))))

(defmethod floor2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus (domain-of number)))
      (setq quo (bf-floor number modulus))
      (values (bf-integer-part quo)
	      (bf-difference number (bf-times quo modulus))))))

(defgeneric ceiling1 (number)
  (:documentation
   "Return the quotient truncated towards positive infinity."))

(defmethod ceiling1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (cut!ep number 0))
      (unless (eql quo number)
	(setq quo (+ 1 quo)))
      (values (bf-integer-part quo)
	      (bf-difference number quo)))))

(defmethod ceiling2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus (domain-of number)))
      (setq quo (bf-floor number modulus))
      (unless (eql quo (cut!ep quo 0))
	(setq quo (+ 1 quo)))
      (values (bf-integer-part quo)
	      (bf-difference number (bf-times quo modulus))))))

(defgeneric round1 (number)
  (:documentation
   "Return the quotient rounded to the nearest integer."))

(defmethod round1 ((number bigfloat))
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq quo (floor (+ (coerce 1/2 domain) number))) 
      (values quo (bf-difference number (coerce quo domain))))))

(defmethod round2 ((number bigfloat) modulus)
  (let ((domain (domain-of number))
	quo)
    (bind-domain-context domain
      (setq modulus (coerce modulus domain))
      (setq quo (bf-floor (+ number (* (coerce 1/2 domain) modulus))
			  modulus))
      (values quo
	      (bf-difference
	       number
	       (bf-times (coerce quo domain) modulus))))))

(defgeneric truncate1 (number)
  (:documentation
   "Return a quotient that has been truncated towards zero."))

(defmethod truncate1 ((num bigfloat))
  (if (plus? num)
      (floor1 num)
      (ceiling1 num)))

(defmethod truncate2 ((num bigfloat) modulus)
  (if (plus? num)
      (floor2 num modulus)      
      (ceiling2 num modulus)))

;;;  Arithmetic predicates
 
(defun bf-binary= (n1 n2)  
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (and (cl:=  e1 e2)
	   (cl:= (bigfloat-mantissa n1) (bigfloat-mantissa n2))))))
 
(defun bf-binary>= (n1 n2)
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (cond ((cl:=  e1 e2)
	     (cl:>= (bigfloat-mantissa n1) (bigfloat-mantissa n2)))
	    ((cl:> e1 e2)
	     (cl:> (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                   (bigfloat-mantissa n2)))
	    ((cl:>= (bigfloat-mantissa n1)
                    (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
	     t)
	    (t nil)))))

(defun bf-binary> (n1 n2)
  (with-slots ((e1 exponent)) n1
    (with-slots ((e2 exponent)) n2
      (cond ((cl:=  e1 e2)
	     (cl:> (bigfloat-mantissa n1) (bigfloat-mantissa n2)))
	    ((cl:> e1 e2)
	     (cl:> (bigfloat-mantissa (incprec! n1 (cl:- e1 e2)))
                   (bigfloat-mantissa n2)))
	    ((cl:> (bigfloat-mantissa n1)
                   (bigfloat-mantissa (incprec! n2 (cl:- e2 e1))))
	     t)
	    (t nil)))))
 
(defun bf-integerp (x)
  (and (bigfloatp x)
       (not (cl:minusp (bigfloat-exponent x)))))

;; Elementary Constants
 
;; This function returns the value of constant CNST of the precision
;; K, if it was calculated previously with, at least, the precision K,
;; else it returns :NOT-FOUND.
(defun get!const (cnst k)
  (unless (atom cnst)
    (error "Invalid argument to get!const: ~S" cnst))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to get!const: ~D" k))  
  (let ((u (get cnst 'save!c)))
    (cond ((or (null u) (cl:< (car u) k)) nil)
	  ((equal (car u) k) (cdr u))
	  (t (round!mt (cdr u) k))))) 
 
;; This function saves the value of constant CNST for the later use.
(defun save!const (cnst nmbr)
  (unless (atom cnst)
    (error "Invalid constant for save!const: ~S" cnst))
  (unless (bigfloatp nmbr)
    (error "Invalid argument to  save!const: ~S" nmbr))
  (setf (get cnst 'save!c) (cons (preci! nmbr) nmbr)))

;; This function sets the value of constant CNST.  CNST is the name of
;; the constant.  L is a list of integers, which represents the value
;; of the constant in the way described in the function READ!LNUM.
(defmacro set!const (constant digits)
  `(progn (save!const ',constant (read!lnum ',digits))
    ',constant))
 
(set!const !pi
           (0 31415926535897932384626433832795028841971693993751058209749))
 
(set!const !e
           (0 27182818284590452353602874713526624977572470936999595749669))

(defmacro define-bfloat-constant (name &body form)
  `(defun ,name (prec)
    (let ((u (get!const ',name prec)))
      (when (eq u :not-found)
        (setq u ,@form)
        (save!const ',name u))
      u)))

;;  This function calculates the value of  `pi' with the precision K by       
;; using Machin's identity:           
;;          PI = 16*ATAN(1/5) - 4*ATAN(1/239).         
;; The calculation is performed mainly on integers.  
(defun bf-pi-machin (k)
  (let* ((k+3 (+ k 3))
	 s
	 (ss (cl:truncate (expt 10 k+3) 5))
	 (n ss)
	 (m 1)
	 (x -25)
	 u)
    (loop while (not (cl:zerop n)) do
      (setq n (cl:truncate n x))
      (setq ss (+ ss (cl:truncate n (setq m (+ m 2))))))
    (setq s (setq n (cl:truncate (expt 10 k+3) 239)))
    (setq x (- (expt 239 2)))
    (setq m 1)
    (loop while (not (cl:zerop n)) do
      (setq n (cl:truncate n x))
      (setq s (+ s (cl:truncate n (setq m (+ m 2))))))
    (setq u (round!mt (make-bigfloat *domain* (- (* 16 ss) (* 4 s)) (- k+3))
		      k))
    (save!const '!pi u)
    u))

;; This function calculates the square root of X with the precision K,
;; by Newton's iteration method.
(defun bf-sqrt (x k)
  (if (0? x) (coerce 0 *domain*)
      (let* ((k2 (+ k 2))
	     (ncut (- k2 (cl:truncate (1+ (order! x)) 2)))
	     (half (coerce 1/2 *domain*))
	     (dcut (make-bigfloat *domain* 10 (- ncut)))
	     (dy (make-bigfloat *domain* 20 (- ncut)))
	     (nfig 1)
	     (y0 (conv!mt x 2))
	     y u)
	(setq y0 (if (cl:zerop (rem (bigfloat-exponent y0) 2))
		     (make-bigfloat *domain*
		       (+ 3 (* 2 (cl:truncate (bigfloat-mantissa y0) 25)))
		       (cl:truncate (bigfloat-exponent y0) 2))
		     (make-bigfloat *domain*
		       (+ 10 (* 2 (cl:truncate (bigfloat-mantissa y0) 9)))
		       (cl:truncate (- (bigfloat-exponent y0) 1) 2))))
	(loop while (or (< nfig k2)
			(> (bf-abs dy) dcut))
	      do (if (> (setq nfig (* 2 nfig)) k2)
		     (setq nfig k2))
		 (setq u (bf-quotient x y0 nfig))
		 (setq y (bf-times (bf-plus y0 u) half))
		 (setq dy (bf-difference y y0))
		 (setq y0 y))
	(round!mt y k)))) 
 
;; This function calculates the value of 'PI', with the precision K, by
;; the arithmetic-geometric mean method.  (R. Brent, JACM vol.23, #2,
;; pp.242-251(1976).)
(defun bf-pi-agm (k)
  (let* ((n 1)
	 (k2 (+ k 2))
	 (u (coerce 1/4 *domain*))
	 (half (coerce 1/2 *domain*))
	 (dcut (make-bigfloat *domain* 10 (- k2)))
	 (x (coerce 1 *domain*))
	 (y (bf-quotient x (bf-sqrt (coerce 2 *domain*) k2) k2))
	 v)
    (loop while (> (bf-abs (bf-difference x y)) dcut) do
          (setq v x)
          (setq x (bf-times (bf-plus x y) half))
          (setq y (bf-sqrt (cut!ep (bf-times y v) (- k2)) k2))
          (setq v (bf-difference x v))
          (setq v (bf-times (bf-times v v) (coerce n *domain*)))
          (setq u (bf-difference u (cut!ep v (- k2))))
          (setq n (* 2 n)))
    (setq v (cut!mt (bf-expt (bf-plus x y) 2 k2) k2))
    (setq u (bf-quotient v (bf-times (coerce 4 *domain*) u) k))
    (save!const '!pi u)
    u))

(defun bf-pi (precision)
  (cond ((cl:< precision 20)
	 (round!mt (make-bigfloat *domain* 314159265358979323846 -20)
		   precision))
	((get!const '!pi precision))
	((cl:< precision 1000) (bf-pi-machin precision))
	(t (bf-pi-agm precision))))

(defgeneric pi-value (domain)
  (:documentation
   "Return the value of PI with the proper precision."))

(defmethod pi-value ((domain real-numbers))
  (bind-domain-context domain
    (bf-pi *REAL-PRECISION*)))

(defun bf-e (precision)
  (cond ((not (> precision 20))
	 (round!mt (make-bigfloat *domain* 271828182845904523536 -20)
		   precision))
	(t (let* ((u (get!const '!e precision))
		  (k2 (+ precision 2))
		  (m 1)
		  (n (expt 10 k2))
		  (ans 0))
	     (when (null u)
	       (loop while (not (cl:zerop n))
		     do (incf ans (setq n (cl:truncate n (incf m)))))
	       (setq ans (+ ans (* 2 (expt 10 k2))))
	       (setq u (round!mt (make-bigfloat *domain* ans (- k2))
				 precision))
	       (save!const '!e u))
	     u))))

(defgeneric e-value (domain)
  (:documentation
   "Return the value of e with the proper precision."))

;; This function calculates the value of 'E', the base of the natural
;; logarithm, with precision K, by summing the Taylor series for
;; EXP(X=1).
(defmethod e-value ((domain real-numbers))
  (bind-domain-context domain
    (bf-e *REAL-PRECISION*)))

;;;  Elementary Functions.

(defun bf-exp (x k)
  (cond ((0? x) (coerce 1 *domain*))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (y (bf-abs x))
		  (m (floor y))
		  (q (coerce m *domain*))
		  (r (bf-difference y q))
		  yq yr)
	     (setq yq (if (cl:zerop m) one
			  (bf-expt (bf-e k2) m k2)))
	     (cond ((0? r) (setq yr one))
		   (t (let ((j 0) (n 0)
			    (dcut (make-bigfloat *domain* 10 (- k2)))
			    (ri one) (tm one)
			    fctrial)
			(setq yr one)
			(setq m 1)
			(loop while (> tm dcut) do
                              (setq fctrial
                                    (coerce
                                     (setq m (* m (setq j (1+ j)))) *domain*))
                              (setq ri (cut!ep (bf-times ri r) (- k2)))
                              (setq n (max 1 (+ (- k2 (order! fctrial))
                                                (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq yr (bf-plus yr tm))
                              (cond ((cl:zerop (rem j 10))
                                     (setq yr (cut!ep yr (- k2)))))))))
	     (setq y (cut!mt (bf-times yq yr) (1+ k)))
	     (if (minus? x) (bf-quotient one y k)
		 (round!last y)))))) 

;; This function calculates the value of the exponential function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for exp(z), 0 < z < 1.
(defmethod exp ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-exp number *REAL-PRECISION*)))

(defun bf-log (x k)
  (when (not (plus? x))
    (error "Invalid argument to log: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to log: ~D" k))
  (cond ((= x 1)
	 (coerce 0 *domain*))
	(t (let* ((m 0)
		  (k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (ee (bf-e k2))
		  (es (bf-exp (coerce 0.1 *domain*) k2))
		  sign l y z)
	     (cond ((> x one) (setq sign one) (setq y x))
		   (t (setq sign (bf-minus one))
		      (setq y (bf-quotient one x k2))))
	     (cond ((< y ee)
		    (setq z y))
		   (t 
		    (cond ((cl:zerop
			    (setq m (cl:truncate (* (order! y) 23) 10)))
			   (setq z y))
			  (t (setq z (bf-quotient y (bf-expt ee m k2) k2))))
		    (loop while (> z ee) do
                          (setq m (1+ m))
                          (setq z (bf-quotient z ee k2)))))
	     (setq l (coerce m *domain*))
	     (setq y (coerce 0.1 *domain*))
	     (loop while (> z es) do
                   (setq l (bf-plus l y))
                   (setq z (bf-quotient z es k2)))
	     (setq z (bf-difference z one))
	     (prog (n dcut tm zi)
                (setq n 0)
                (setq y (setq tm (setq zi z)))
                (setq z (bf-minus z))
                (setq dcut (make-bigfloat *domain* 10 (- k2)))
                (setq m 1)
                (loop while (> (bf-abs tm) dcut) do
                      (setq zi (cut!ep (bf-times zi z) (- k2)))
                      (setq n (max 1 (+ k2 (order! zi))))
                      (setq tm
                            (bf-quotient zi (coerce (setq m (1+ m)) *domain*)
                                         n))
                      (setq y (bf-plus y tm))
                      (cond ((cl:zerop (rem m 10))
                             (setq y (cut!ep y (- k2)))))))
	     (setq y (bf-plus y l))
	     (round!mt (bf-times sign y) k)))))
 
;; This function calculates log(x), the value of the logarithmic
;; function at the point 'x', with the precision k, by solving x =
;; exp(y) by Newton's method.
;;  x > 0, k is a positive integer                        
#+Ignore
(defun bf-log-newton (x k)
  (when (not (plus? x))
    (error "Invalid argument to log: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to log: ~D" k))
  (cond ((bf-equal x (coerce 1 *domain*))
	 (coerce 0 *domain*))
	(t (let* ((k2 (+ k 2))
		  m (one (coerce 1 *domain*))
		  (ee (bf-e (+ k2 2)))
		  sign y z)
	     (cond ((> x one)
		    (setq sign one)
		    (setq y x))
		   (t (setq sign (bf-minus one))
		      (setq y (bf-quotient one x k2))))
	     (if (< y ee)
		 (setq m 0 z y)
		 (cond ((cl:zerop
			 (setq m (cl:truncate (cl:* (order! y) 23) 10)))
			(setq z y))
		       (t (setq z (bf-quotient y (bf-expt ee m k2) k2))
			  (loop while (> z ee) do
                                (setq m (1+ m))
                                (setq z (bf-quotient z ee k2))))))
	     (let ((nfig 0) (n 0)
		   (dcut (make-bigfloat *domain* 10 (- k2)))
		   dx
		   (dy (make-bigfloat *domain* 20 (- k2)))
		   x0)
	       (setq y (bf-quotient (bf-difference z one)
				    (coerce 1.72 *domain*) 2))
	       (setq nfig 1)
	       (loop while (or (< nfig k2) (> (bf-abs dy) dcut)) do
                     (cond
                       ((> (setq nfig (* 2 nfig)) k2)
                        (setq nfig k2)))
                     (setq x0 (exp* y nfig))
                     (setq dx (bf-difference z x0))
                     (setq n (max 1 (+ nfig (order! dx))))
                     (setq dy (bf-quotient dx x0 n))
                     (setq y (bf-plus y dy))))
	     (setq y (bf-plus (coerce m *domain*) y))
	     (round!mt (bf-times sign y) k)))))

(defmethod-sd log2 ((x bigfloat) (base bigfloat))
  (let ((k2 (+ 2 *REAL-PRECISION*)))
    (bind-domain-context domain
      (bf-quotient (bf-log x k2) (bf-log base k2) (- k2 2)))))

;; This function calculates log(x) by summing terms of the     
;; Taylor series for LOG(1+Z), 0 < Z < 0.10518. 
(defmethod log ((x bigfloat))
  (bind-domain-context (domain-of x)
    (bf-log x *REAL-PRECISION*)))

(defun bf-cos (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to cos: ~D" k))
  (cond ((0? x) (coerce 1 *domain*))
	(t (when (minus? x)
	     (setq x  (- x)))
	   (let* ((k2 (+ k 2))
		  (m (preci! x))
		  (pi4 (/ (bf-pi (+ k2 m)) 4))
		  sign q r y)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (floor x pi4))))
		      (setq r (- x (* q pi4)))))
	     (setq sign (coerce 1 *domain*))
	     (cond ((not (< m 8)) (setq m (rem m 8))))
	     (cond ((not (< m 4))
		    (setq sign (- sign))
		    (setq m (- m 4))))
	     (cond ((not (< m 2)) (setq sign (- sign))))
	     (cond ((equal m 1)
		    (setq r (cut!mt (- pi4 r) k2))
		    (bf-times sign (bf-sin r k)))
		   ((equal m 2)
		    (setq r (cut!mt r k2))
		    (bf-times sign (bf-sin r k)))
		   (t (when (= m 3)
			(setq r (cut!mt (- pi4 r) k2)))
		      (let ((j 0) (n 0)
			    (dcut (make-bigfloat *domain* 10 (- k2)))
			    fctrial ri tm)
			(setq y (setq ri (setq tm (coerce 1 *domain*))))
			(setq r (- (cut!ep (* r r) (- k2))))
			(setq m 1)
			(loop while (> (bf-abs tm) dcut) do
                              (setq j (+ j 2))
                              (setq fctrial
                                    (coerce
                                     (setq m (* m j (- j 1))) *domain*))
                              (setq ri (cut!ep (* ri r) (- k2)))
                              (setq n (max 1 (+ (- k2 (order! fctrial))
                                                (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq y (+ y tm))
                              (cond ((equal (rem j 20) 0)
                                     (setq y (cut!ep y (- k2)))))))
		      (round!mt (* sign y) k)))))))

(defun bf-sin (x k)
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-sin (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (m (preci! x))
		  (pi4 (bf-times (bf-pi (+ k2 m)) (coerce 1/4 *domain*)))
		  sign q r y)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (bf-floor x pi4))))
		      (setq r (bf-difference x (bf-times q pi4)))))
	     (setq sign (coerce 1 *domain*))
	     (cond ((not (< m 8)) (setq m (rem m 8))))
	     (cond ((not (< m 4))
		    (setq sign (bf-minus sign))
		    (setq m (- m 4))))
	     (cond ((equal m 1)
		    (setq r (cut!mt (bf-difference pi4 r) k2))
		    (bf-times sign (bf-cos r k)))
		   ((equal m 2)
		    (setq r (cut!mt r k2))
		    (bf-times sign (bf-cos r k)))
		   (t (unless (equal m 0)
			(setq r (cut!mt (bf-difference pi4 r) k2)))
		      (let* ((ncut (- k2 (min 0 (1+ (order! r)))))
			     (dcut (make-bigfloat *domain* 10 (- ncut)))
			     (tm r) (ri r) (j 1) n fctrial)
			(setq y r)
			(setq r (bf-minus (cut!ep (bf-times r r) (- ncut))))
			(setq m 1)
			(loop while (> (bf-abs tm) dcut) do
                              (setq j (+ j 2))
                              (setq fctrial
                                    (coerce
                                     (setq m (* m j (1- j))) *domain*))
                              (setq ri (cut!ep (bf-times ri r) (- ncut)))
                              (setq n (max 1 (+ (- k2 (order! fctrial)) (order! ri))))
                              (setq tm (bf-quotient ri fctrial n))
                              (setq y (bf-plus y tm))
                              (cond ((cl:zerop (rem j 20))
                                     (setq y (cut!ep y (- ncut)))))))
		      (round!mt (bf-times sign y) k)))))))

;; This function calculates sin(x), the value of the sine function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for:   sin(z), 0 < Z < PI/4.    
(defmethod sin ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-sin number *REAL-PRECISION*)))

;; This function calculates cos(x), the value of the cosine function at
;; the point 'x', with the precision k, by summing terms of the Taylor
;; series for:   cos(z), 0 < Z < PI/4.    
(defmethod cos ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-cos number *REAL-PRECISION*)))

(defun bf-tan (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to tan: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-tan (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (m (preci! x))
		  (pi4 (bf-times (bf-pi (+ k2 m)) (coerce 1/4 *domain*)))
		  sign q r)
	     (cond ((< x pi4)
		    (setq m 0)
		    (setq r x))
		   (t (setq m (floor (setq q (bf-floor x pi4))))
		      (setq r (bf-difference x (bf-times q pi4)))))
	     (cond ((not (< m 4)) (setq m (rem m 4))))
	     (setq sign (if (< m 2) one (bf-minus one)))
	     (cond ((or (= m 1) (= m 3))
		    (setq r (bf-difference pi4 r))))
	     (setq r (cut!mt r k2))
	     (cond ((or (equal m 0) (equal m 3))
		    (setq r (bf-sin r k2))
		    (setq q (bf-difference one (bf-times r r)))
		    (setq q (bf-sqrt (cut!mt q k2) k2))
		    (bf-times sign (bf-quotient r q k)))
		   (t (setq r (bf-sin r k2))
		      (setq q (bf-difference one (bf-times r r)))
		      (setq q (bf-sqrt (cut!mt q k2) k2))
		      (bf-times sign (bf-quotient q r k))))))))

;; This function calculates tan(x), the value of the tangent function
;; at the point 'x', with the precision k, by calculating       
;;          sin(x)  or  cos(x) = sin(pi/2-x).       
(defmethod tan ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-tan number *REAL-PRECISION*)))

(defun bf-atan (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to atan: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-atan (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (one (coerce 1 *domain*))
		  (pi4 (bf-times (bf-pi k2) (coerce 1/4 *domain*)))
		  y z)
	     (cond ((= x 1)
		    (round!mt pi4 k))
		   ((> x one)
		    (round!mt
		     (bf-difference (bf-plus pi4 pi4)
				    (bf-atan (bf-quotient one x k2) (+ k 1)))
		     k))
		   ((< x (coerce 0.42 *domain*))
		    (let* ((m 1) (n 0)
			   (ncut (- k2 (min 0 (+ (order! x) 1))))
			   (dcut (make-bigfloat *domain* 10 (- ncut)))
			   (zi x)
			   (tm x))
		      (setq y tm)
		      (setq z (bf-minus (cut!ep (bf-times x x) (- ncut))))
		      (loop while (> (bf-abs tm) dcut) do
                            (setq zi (cut!ep (bf-times zi z) (- ncut)))
                            (setq n (max 1 (+ k2 (order! zi))))
                            (setq tm (bf-quotient
                                      zi (coerce (setq m (+ m 2)) *domain*)
                                      n))
                            (setq y (bf-plus y tm))
                            (cond ((cl:zerop (rem m 20))
                                   (setq y (cut!ep y (- ncut)))))))
		    (round!mt y k))
		   (t (setq y (bf-plus one (cut!mt (bf-times x x) k2)))
		      (setq y (bf-plus one (bf-sqrt y k2)))
		      (setq y (bf-atan (bf-quotient x y k2) (+ k 1)))
		      (round!mt (bf-times y (coerce 2 *domain*)) k)))))))

;; this function calculates atan(x), the value of the arctangent
;; function at the point 'x', with the precision k, by summing terms
;; of the Taylor series for atan(z)  if  0 < z < 0.42.  
;;   otherwise the following identities are used!  
;;       atan(x) = pi/2 - atan(1/x)  if  1 < x  and 
;;       atan(x) = 2*atan(x/(1+sqrt(1+x**2)))       
;;             if  0.42 <= x <= 1.                     
;; the answer is in the range [-pi/2, pi/2).    
(defmethod atan ((number bigfloat) &optional base)
  (when base
    (error "Two argument atan not implemented yet"))
  (bind-domain-context (domain-of number)
    (bf-atan number *REAL-PRECISION*)))

(defun bf-asin (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to asin: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to asin: ~D" k))
  (cond ((minus? x) (bf-minus (bf-asin (bf-minus x) k)))
	(t (let ((k2 (+ k 2))
		 (one (coerce 1 *domain*)))
	     (cond ((< (bf-difference one x)
		       (make-bigfloat *domain* 10 (- k2)))
		    (round!mt (bf-times (bf-pi (1+ k)) (coerce 1/2 *domain*))
			      k))
		   (t (bf-atan
		       (bf-quotient x (bf-sqrt (cut!mt (- 1 (* x x)) k2) k2)
				    k2)
		       k))))))) 

;; This function calculates asin(x), the value of the arcsine function
;; at the point 'x', with the precision k, by calculating        
;;          atan(x/sqrt(1-x**2))  
;; The answer is in the range <-pi/2 , pi/2>.  
(defmethod asin ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-asin number *REAL-PRECISION*)))

(defun bf-acos (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to acos: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to acos: ~D" k))
  (let ((k2 (+ k 2))
	y)
    (cond ((< (bf-abs x) (make-bigfloat *domain* 50 (- k2)))
	   (round!mt (bf-times (bf-pi (+ k 1)) (coerce 1/2 *domain*))
		     k))
	  (t (setq y (bf-quotient
		      (bf-sqrt (cut!mt
				(bf-difference (coerce 1 *domain*)
					       (bf-times x x))
				k2) k2)
		      (bf-abs x)
		      k2))
	     (if (minus? x)
		 (round!mt (bf-difference (bf-pi (+ k 1)) (bf-atan y k))
			   k)
		 (bf-atan y k))))))

;; This function calculates acos(x), the value of the arccosine
;; function at the point 'x', with the precision k, by calculating        
;;          atan(sqrt(1-x**2)/x)  if  x > 0  or      
;;          atan(sqrt(1-x**2)/x) + pi  if  x < 0.    
;; the answer is in the range [0 , pi).        
(defmethod acos ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-acos number *REAL-PRECISION*)))

;; this function calculates arcsin(x), the value of the arcsine
;; function at the point 'x', with the precision k, by solving
;;    x = sin(y)  if  0 < x <= 0.72,  or       
;;    sqrt(1-x**2) = sin(y)  if  0.72 < x,     
;; by Newton's iteration method.               
;; the answer is in the range [-pi/2, pi/2).
#+Ignore
(defun bf-asin-newton (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to asin: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to asin: ~D" k))
  (cond ((0? x) (coerce 0 *domain*))
	((minus? x) (bf-minus (bf-asin-newton (bf-minus x) k)))
	(t (let* ((k2 (+ k 2))
		  (dcut (make-bigfloat *domain* 10 (+ (- k2) (order! x) 1)))
		  (one (coerce 1 *domain*))
		  (pi2 (bf-times (bf-pi (+ k2 2)) (coerce 1/2 *domain*)))
		  y)	       
	     (cond ((< (- 1 x) dcut)
		    (round!mt pi2 k))
		   ((> x (coerce 0.72 *domain*))
		    (setq y (cut!mt (bf-difference one (bf-times x x)) k2))
		    (setq y (bf-asin-newton (bf-sqrt y k2) k))
		    (round!mt (bf-difference pi2 y) k))
		   (t (let ((nfig 1)
			    (n 0)
			    (dy one)
			    cx dx x0)
			(setq y x)
			(loop while (or (< nfig k2)
					(> (bf-abs dy) dcut))
			      do (cond ((> (setq nfig (* 2 nfig)) k2)
					(setq nfig k2)))
				 (setq x0 (bf-sin y nfig))
				 (setq cx (bf-sqrt (cut!mt (- 1 (* x0 x0))
							   nfig)
					     nfig))
				 (setq dx (- x x0))
				 (setq n (max 1 (+ nfig (order! dx))))
				 (setq dy (bf-quotient dx cx n))
				 (setq y (bf-plus y dy)))
			(round!mt y k))))))))
 
;; This function calculates arccos(x), the value of the arccosine
;; function at the point 'x', with the precision k, by calculating
;;    arcsin(sqrt(1-x**2))  if  x > 0.72  and    
;;    pi/2 - arcsin(x)  otherwise.
;; The answer is in the range [0, pi).          
#+ignore
(defun bf-acos-newton (x k)
  (when (or (> (bf-abs x) (coerce 1 *domain*)))
    (error "Invalid argument to acos: ~S" x))
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to acos: ~D" k))
  (cond ((bf-<= x (coerce 0.72 *domain*))
         (round!mt
	  (bf-difference
	   (bf-times (bf-pi (+ k 1)) (coerce 1/2 *domain*))
	   (bf-asin-newton x k))
	  k))
	(t (bf-asin-newton
	    (bf-sqrt
	     (cut!mt (bf-difference (coerce 1 *domain*) (* x x))
		     (+ k 2))
	     (+ k 2))
	    k))))
 
;; This function calculates arctan(x), the value of the arctangent
;; function at the point 'x', with the precision k, by calculating
;;     arcsin(x/sqrt(1+x**2))
;; The answer is in the range [-pi/2, pi/2).  
#+Ignore
(defun bf-atan-newton (x k)
  (unless (and (integerp k) (> k 0))
    (error "Invalid precision to atan: ~D" k))
  (cond ((minus? x) (bf-minus (bf-atan-newton (bf-minus x) k)))
	(t (bf-asin-newton
	    (bf-quotient x (bf-sqrt (cut!mt (+ 1 (* x x)) (+ k 2))
				    (+ k 2))
			 (+ k 2))
	    k))))

(defmethod-sd expt ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (cond ((bf-integerp y) (expt x (floor y)))
	  ((minus? y)
	   (/ 1 (expt x (bf-minus y))))
	  (t (let ((n *REAL-PRECISION*)
		   (xp (cl:abs x))
		   yp) 
	       (cond ((bf-integerp (bf-times y (coerce 2 domain)))
		      (setq xp (incprec! xp 1)) 
		      (setq yp (round!mt
				(bf-times (expt xp (floor y))
					  (bf-sqrt xp (+ n 1)))
				n)))
		     (t (setq yp (bf-exp (* y (bf-log xp (1+ n))) n))))
	       (cond ((minus? x) (bf-minus yp)) (t yp)))))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			         Numbers
;;; ===========================================================================
;;; (c) Copyright 1991,1993 Cornell University

;;; numbers.lisp,v 1.14 1995/05/24 17:42:07 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.14")

;; The Rational Integers  (Z)

(define-domain-element-classes rational-integers
    rational-integer)

(defun rational-integers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bZ~")
  #-Genera
  (princ "Z"  stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator rational-integers ()
    (let ((d (make-instance 'rational-integers
                            :print-function 'rational-integers-print-object)))
      (assert-ordered-domain d)
      (assert-gcd-domain d)
      (assert-unique-factorization-domain d)
      (with-slots (zero one) d
        (setq zero (make-element d 0))
        (setq one (make-element d 1)))
      d)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'rational-integers))))

(defmethod print-object ((n rational-integer) stream)
  (princ (integer-value n) stream))

(defmethod make-element ((domain rational-integers) (x integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod weyl:make-element ((domain rational-integers) (x integer)
			      &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-integer :domain domain :value x))


(defmethod coerce ((x integer) (domain rational-integers))
  (make-element domain x))

;; The Rational numbers  (Q)

(define-domain-element-classes rational-numbers
  rational-number rational-integer)

(defun rational-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bQ~")
  #-Genera
  (princ "Q"  stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator rational-numbers ()
    (let ((domain (make-instance 'rational-numbers
                                 :print-function 'rational-numbers-print-object))
          (Z (get-rational-integers)))
      (make-homomorphism Z
                         #'(lambda (x)
                             (make-element domain (integer-value x)))
                         domain)
      domain)
    :predicate #'(lambda (d)
                   (eql (class-name (class-of d)) 'rational-numbers))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator quotient-field ((ring rational-integers))
    (make-rational-numbers)))

(defmethod get-quotient-field ((ring rational-integers))
  (get-rational-numbers))

(defmethod print-object ((ratfun rational-number) stream)
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((1? denominator)
	   (prin1 numerator stream))
	  (t (prin1 numerator stream)
	     (princ "/" stream)
	     (prin1 denominator stream)))))

(defmethod make-element ((qf rational-numbers) (num integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain qf :value num))

(defmethod weyl:make-element ((qf rational-numbers) (num integer) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   qf (cons num args)))
  (make-instance 'rational-integer :domain qf :value num))

(defmethod coerce ((elt integer) (domain rational-numbers))
  (make-element domain elt))

(defmethod make-element ((qf rational-numbers) (num ratio) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-number :domain qf
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod weyl:make-element ((qf rational-numbers) (num ratio) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   qf (cons num args)))
  (make-instance 'rational-number :domain qf
		 :numerator (cl:numerator num)
		 :denominator (cl:denominator num)))

(defmethod coerce ((elt ratio) (domain rational-numbers))
  (make-element domain elt))

(defmethod make-quotient-element
    ((qf rational-numbers) (numerator integer) (denominator integer))
  (cond ((cl:= denominator 1)
	 (make-instance 'rational-integer :domain qf :value numerator))
	(t (make-instance 'rational-number :domain qf
			  :numerator numerator
			  :denominator denominator))))

;; The Real numbers (R)

(define-domain-element-classes real-numbers
  floating-point-number bigfloat rational-integer rational-number)

(defun real-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bR~")
  #-Genera
  (princ "R"  stream))

;; For these two there is only one domain.  (We could also implement
;; several integer domains if we wanted later.)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator real-numbers ()
    (let ((domain (make-instance 'real-numbers
                                 :print-function 'real-numbers-print-object))
          (Q (get-rational-numbers)))
      (make-homomorphism Q
                         #'(lambda (x)
                             (make-element domain
                                           (if (typep x 'rational-integer)
                                               (integer-value x)
                                               (cl:/ (qo-numerator x)
                                                     (qo-denominator x)))))
                         domain)
      domain)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'real-numbers))))

(defvar *floating-point-precision* 16.
  "Precision below which to use the machine floating point representation")

(defvar *real-precision* *floating-point-precision*
  "The default precision when creating a Real number")

(defmethod print-object ((z floating-point-number) stream)
  (format stream "~D" (fp-value z)))

(defmethod make-element ((domain real-numbers) (x integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod weyl:make-element ((domain real-numbers) (x integer) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-integer :domain domain :value x))

(defmethod coerce ((elt integer) (domain real-numbers))
  (make-element domain elt))

(defmethod make-element ((domain real-numbers) (x ratio) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'rational-number :domain domain
		 :numerator (cl:numerator x)
		 :denominator (cl:denominator x)))

(defmethod weyl:make-element ((domain real-numbers) (x ratio) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'rational-number :domain domain
		 :numerator (cl:numerator x)
		 :denominator (cl:denominator x)))

(defmethod coerce ((elt ratio) (domain real-numbers))
  (make-element domain elt))

;; The following method is needed so that routines that return ratios
;; will work.
(defmethod make-quotient-element
    ((domain real-numbers) (numerator integer) (denominator integer))
  (cond ((cl:= denominator 1)
	 (make-instance 'rational-integer :domain domain :value numerator))
	(t (make-instance 'rational-number :domain domain
			  :numerator numerator
			  :denominator denominator))))

(defmethod make-element ((domain real-numbers) (x float) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'floating-point-number :domain domain
		 :value x))

(defmethod weyl:make-element ((domain real-numbers) (x float) &rest args)
  (when (not (null args))
    (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
	   domain (cons x args)))
  (make-instance 'floating-point-number :domain domain
		 :value x))

(defmethod coerce ((elt float) (domain real-numbers))
  (make-element domain elt))

(defmethod zero ((domain real-numbers))
  (weyl:make-element domain 0))

(defmethod one ((domain real-numbers))
  (weyl:make-element domain 1))

;; The Complex numbers (C)

(define-domain-element-classes complex-numbers
  complex-number)

(defun complex-numbers-print-object (d stream)
  (declare (ignore d))
  #+Genera
  (format stream "~'bC~")
  #-Genera
  (princ "C"  stream))

;; For these two there is only one domain.  (We could also implement
;; several integer domains if we wanted later.)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator complex-numbers ()
    (let ((domain (make-instance 'complex-numbers
                                 :print-function 'complex-numbers-print-object))
          (R (get-real-numbers)))
      (make-homomorphism R
                         #'(lambda (x)
                             (make-element domain
                                           (cond ((typep x 'rational-integer)
                                                  (integer-value x))
                                                 ((typep x 'rational-number)
                                                  (cl:/ (qo-numerator x)
                                                        (qo-denominator x)))
                                                 ((typep x 'floating-point-number)
                                                  (fp-value x))
                                                 (t x))))
                         domain)
      domain)
    :predicate #'(lambda (d) (eql (class-name (class-of d)) 'complex-numbers))))

(defmethod print-object ((z complex-number) stream)
  (with-slots ((x real) (y imag)) z
    (cond ((0? y)
	   (princ x stream))
	  ((0? x)
	   (if (1? y)
	       (format stream "i")
	       (format stream "~S i" y)))
	  (t (if (1? y)
		 (format stream "~S + i" x)
		 (format stream "~S + ~S i" x y))))))

(defmethod make-element ((domain complex-numbers) (x integer) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'rational-integer :domain domain :value x)
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x integer) &rest args)
  (cond ((or (null args)
	     (and (0? (first args)) (null (rest args))))
	 (make-instance 'rational-integer :domain domain :value x))
	((null (rest args))
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element ((domain complex-numbers) (x ratio) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'rational-number :domain domain
		     :numerator (cl:numerator x)
		     :denominator (cl:denominator x))
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x ratio) &rest args)
  (cond ((or (null args)
	     (and (0? (first args))
		  (null (rest args))))
	 (make-instance 'rational-number :domain domain
			:numerator (cl:numerator x)
			:denominator (cl:denominator x)))
	((null (rest args))
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element ((domain complex-numbers) (x float) &rest args)
  (if (or (null args) (0? (first args)))
      (make-instance 'floating-point-number :domain domain
		     :value x)
      (make-instance 'complex-number :domain domain
		     :realpart x
		     :imagpart (first args))))

(defmethod weyl:make-element ((domain complex-numbers) (x float) &rest args)
  (cond ((or (null args)
	     (and (0? (first args))
		  (null (rest args))))
	 (make-instance 'floating-point-number :domain domain
			:value x))
	((null (rest args))	 
	 (if (and (cl:numberp (first args))
		  (not (cl:complexp (first args))))
	     (make-instance 'complex-number :domain domain
			    :realpart x
			    :imagpart (first args))
	     (error "Wrong type of arguments to MAKE-ELEMENT of ~S: ~S"
		    domain (cons x args))))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod make-element
    ((domain complex-numbers) (x cl:complex) &rest ignore)
  (declare (ignore ignore))
  (let ((real (cl:realpart x))
	(imag (cl:imagpart x)))
    (if (0? imag)
	(make-element domain real)
	(make-instance 'complex-number :domain domain
		       :realpart real :imagpart imag))))

(defmethod weyl:make-element
    ((domain complex-numbers) (x cl:complex) &rest args)
  (cond ((null args)
	 (make-element domain x))
	(t (error "Too many arguments to MAKE-ELEMENT of ~S: ~S"
		  domain (cons x args)))))

(defmethod realpart ((x number))
  (cl:realpart x))

(defmethod imagpart ((x number))
  (cl:imagpart x))

(defmacro def-realimag-part ((x type) real-body imag-body)
  `(progn
    (defmethod realpart ((,x ,type))
      (let ((domain (domain-of ,x)))
        (if (or (typep domain 'complex-numbers)
                (typep domain 'non-strict-domain))
            ,real-body
            (error "Don't know what \"realpart\" means in ~S"
                   domain))))
    (defmethod imagpart ((,x ,type))
      (let ((domain (domain-of ,x)))
        (if (or (typep domain 'complex-numbers)
                (typep domain 'non-strict-domain))
            ,imag-body
            (error "Don't know what \"imagpart\" means in ~S"
                   domain))))))

(def-realimag-part (x rational-integer)
    (make-element domain (integer-value x))
  (zero domain))

(def-realimag-part (x rational-number)
    (make-element domain (cl:/ (qo-numerator x) (qo-denominator x)))
  (zero domain))

(def-realimag-part (x floating-point-number)
    (make-element domain (fp-value x))
  (zero domain))

(def-realimag-part (x bigfloat)
    (make-bigfloat domain (bigfloat-mantissa x) (bigfloat-exponent x))
  (zero domain))

(defmethod realpart ((x complex-number))
  (let ((real (cn-realpart x))
	(domain (domain-of x)))
    (cond ((typep real 'number)
	   (make-element domain real))
	  (t (make-bigfloat domain
			    (bigfloat-mantissa real)
			    (bigfloat-exponent real))))))

(defmethod imagpart ((x complex-number))
  (let ((imag (cn-imagpart x))
	(domain (domain-of x)))
    (cond ((typep imag 'number)
	   (make-element domain imag))
	  (t (make-bigfloat domain
			    (bigfloat-mantissa imag)
			    (bigfloat-exponent imag))))))

(defgeneric conjugate (number)
  (:documentation
   "Return the conjugate value of the number."))

(defmethod conjugate  ((x number))
  (cl:conjugate x))

(defmethod conjugate ((x rational-integer))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x rational-number))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x floating-point-number))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x bigfloat))
  (cond ((or (typep (domain-of x) 'complex-numbers)
	     (typep (domain-of x) 'non-strict-domain))
	 x)
	(t (error "Don't know what \"conjugate\" means in ~S"
		  (domain-of x)))))

(defmethod conjugate ((x complex-number))
  (make-instance 'complex-number :domain (domain-of x)
		 :realpart (cn-realpart x)
		 :imagpart (- (cn-imagpart x))))

(defmethod abs ((x number))
  (cl:abs x))

(defmethod abs ((x rational-integer))
  (make-element (domain-of x) (cl:abs (integer-value x))))

(defmethod abs ((x rational-number))
  (make-instance 'rational-number
		 :domain (domain-of x)
		 :numerator (abs (qo-numerator x))     
		 :denominator (qo-denominator x)))

(defmethod abs ((z floating-point-number))
  (make-element (domain-of z) (cl:abs (fp-value z))))

(defmethod abs ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-abs number)))

(defmethod abs ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (domain-of z) (cl:sqrt (+ (* x x) (* y y))))))

(defgeneric phase (number)
  (:documentation
   "Return the phase of the number."))

(defmethod phase ((x number))
  (cl:phase x))

(defmethod phase ((x rational-integer))
  (zero (domain-of x)))

(defmethod phase ((x rational-number))
  (zero (domain-of x)))

(defmethod phase ((z floating-point-number))
  (zero (domain-of z)))

(defmethod phase ((number bigfloat))
  (zero (domain-of number)))

(defmethod phase ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (domain-of z) (atan y x))))

(defgeneric random-constant (domain &optional height)
  (:documentation
   "Return a random constant."))

(defmethod random-constant ((domain numeric-domain) &optional height)
  (random domain height))

(defvar *default-random-height* most-positive-fixnum)

(defmethod random ((domain rational-integers) 
                   &optional (height *default-random-height*))
  (make-element domain (cl:random height)))

(defmethod random ((domain rational-numbers) 
                   &optional (height *default-random-height*))
  (make-element domain (/ (if (cl:zerop (cl:random 2))
                              (cl:random height)
                              (cl:- (cl:random height)
                          (cl:random height))))))

(defun random-floating-number (height)
  (let ((num (cl:+ (float (cl:random height))
                   (cl:/ (float (cl:random height))
                         (float (cl:random height))))))
    (if (cl:zerop (cl:random 2)) num (cl:- num))))

(defmethod random ((domain real-numbers) 
                   &optional (height *default-random-height*))
  (make-element domain (random-floating-number height)))

(defmethod random ((domain complex-numbers)
                   &optional (height *default-random-height*))
  (make-instance 'complex-number :domain domain
                 :realpart (random-floating-number height)
                 :imagpart (random-floating-number height)))

(defmethod height ((x number))
  (cl:abs x))

(defmethod height ((x rational-integer))
  (make-element (get-real-numbers) (cl:abs (integer-value x))))

(defmethod height ((x rational-number))
  (make-element (get-real-numbers) (cl:max (cl:abs (qo-numerator x))     
                                           (qo-denominator x))))

(defmethod height ((z floating-point-number))
  (make-element (get-real-numbers) (cl:abs (fp-value z))))

;; FIXTHIS I think this is buggy!
(defmethod height ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-abs number)))

(defmethod height ((z complex-number))
  (let ((x (cn-realpart z))
	(y (cn-imagpart z)))
    (make-element (get-real-numbers) (cl:max (cl:abs x) (cl:abs y)))))

(defgeneric convert-to-lisp-number (number)
  (:documentation
   "Return a lisp representation of the number."))

(defmethod convert-to-lisp-number ((x number))
  x)

(defmethod convert-to-lisp-number ((x numeric))
  x)

(defmethod convert-to-lisp-number ((x rational-integer))
  (integer-value x))

(defmethod convert-to-lisp-number ((x rational-number))
  (cl:/ (qo-numerator x) (qo-denominator x)))

(defmethod convert-to-lisp-number ((x floating-point-number))
  (fp-value x))

(defmethod convert-to-lisp-number ((x bigfloat))
  x)

(defun parse-numeric-obj (num)
  ;;(declare (values num type domain))
  (cond ((typep num 'number)
	 (values num
		 (if (typep num 'integer) 'integer (cl:type-of num))
		 nil))
	((typep num 'rational-integer)
	 (values (integer-value num) 'rational-integer (domain-of num)))
	((typep num 'rational-number)
	 (values (cl:/ (qo-numerator num)
                       (qo-denominaTor num))
		 'rational-number
		 (domain-of num)))
	((typep num 'numeric)
	 (values num (class-name (class-of num)) (domain-of num)))
	(t (error "~S is not a numeric object" num))))

(defmethod numerator ((n rational-integer))
  (cond ((or (field? (domain-of n))
	     (typep (domain-of n) 'non-strict-domain))
	 n)
	(t (error "Don't know what \"numerator\" means in ~S"
		  (domain-of n)))))

(defmethod denominator ((n rational-integer))
  (cond ((or (field? (domain-of n))
	     (typep (domain-of n) 'non-strict-domain))
	 (one (domain-of n)))
	(t (error "Don't know what \"denominator\" means in ~S"
		  (domain-of n)))))

(defmethod 0? (x)
  (declare (ignore x))
  nil)

(defmethod 0? ((x number))
  (cl:zerop x))

(defmethod 0? ((x rational-integer))
  (cl:zerop (integer-value x)))

(defmethod 0? ((x rational-number))
  nil)

(defmethod 0? ((x floating-point-number))
  (cl:zerop (fp-value x)))

(defmethod 0? ((number bigfloat))
  (equal (bigfloat-mantissa number) 0))

(defmethod 0? ((x complex-number))
  (and (0? (realpart x)) (0? (imagpart x))))

(defmethod 1? (x)
  (declare (ignore x))
  nil)

(defmethod 1? ((x number))  
  (= x 1))

(defmethod 1? ((x rational-integer))  
  (eql (integer-value x) 1))

(defmethod 1? ((x rational-number))
  nil)

(defmethod 1? ((x floating-point-number))
  (cl:= 1.0 (fp-value x)))

(defmethod 1? ((number bigfloat))
  (and (equal (bigfloat-mantissa number) 1)
       (eql (bigfloat-exponent number) 0)))

(defmethod 1? ((x complex-number))
  (and (1? (realpart x)) (0? (imagpart x))))

(defmethod minus ((x number))
  (cl:- x))

(defmethod minus ((x rational-integer))
  (make-element (domain-of x) (cl:- (integer-value x))))

(defmethod minus ((x rational-number))
  (make-quotient-element (domain-of x)
			 (cl:- (qo-numerator x))
			 (qo-denominator x)))

(defmethod minus ((x floating-point-number))
  (make-element (domain-of x) (cl:- (fp-value x))))

(defmethod minus ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-minus number)))

(defmethod minus ((x complex-number))
  (make-element (domain-of x) (- (cn-realpart x)) (- (cn-imagpart x))))

(defmethod minus? ((x number))
  (cl:minusp x))

(defmethod minus? ((x cl:complex))
  nil)

(defmethod minus? ((x rational-integer))
  (cl:minusp (integer-value x)))

(defmethod minus? ((x rational-number))
  (cl:minusp (qo-numerator x)))

(defmethod minus? ((x floating-point-number))
  (cl:minusp (fp-value x)))

(defmethod minus? ((x bigfloat))
  (cl:minusp (bigfloat-mantissa x)))

(defgeneric plus? (number)
  (:documentation
   "Return true if the number is positive."))

(defmethod plus? ((x number))
  (cl:plusp x))

(defmethod plus? ((x cl:complex))
  (not (cl:zerop x)))

(defmethod plus? ((x rational-integer))
  (cl:plusp (integer-value x)))

(defmethod plus? ((x rational-number))
  (cl:plusp (qo-numerator x)))

(defmethod plus? ((x floating-point-number))
  (cl:plusp (fp-value x)))

(defmethod plus? ((x bigfloat))
  (cl:plusp (bigfloat-mantissa x)))

(defgeneric integer? (number)
  (:documentation
   "Return true if the number is an integer."))

(defmethod integer? ((x number))
  (cl:integerp x))

(defmethod integer? ((x numeric))
  nil)

(defmethod integer? ((x rational-integer))
  t)

(defmethod recip ((x number))
  (cl:/ x))

(defmethod recip ((x rational-integer))
  (let ((x-val (integer-value x))
	(domain (domain-of x)))
    (cond ((or (eql x-val 1) (eql x-val -1))
	   x)
	  ((or (field? domain)
	       (typep domain 'non-strict-domain))
	   (make-element domain (cl:/ 1 x-val)))
	  (t 
	   (error "Trying to take the reciprocal of the rational integer ~S"
		  x)))))

;; recip of a rational integer is covered by QUOTIENT-ELEMENT

(defmethod recip ((x floating-point-number))
  (when (0? x)
    (error "Error: Attempt take reciprocal of zero: ~S" x))
  (make-element (domain-of x) (cl:/ (fp-value x))))

(defmethod recip ((z complex-number))
  (when (0? z)
    (error "Error: Attempt take reciprocal of zero: ~S" z))
  (let ((x (realpart z))
	(y (imagpart z))
	denom)
    (setq denom (+ (* x x) (* y y)))
    (make-element (domain-of z)
                  (convert-to-lisp-number (/ x denom))
                  (convert-to-lisp-number (/ (- y) denom)))))

(defgeneric sqrt (number)
  (:documentation
   "Return the square root of the number."))

(defmethod sqrt ((x number))
  (cl:sqrt  x))

(defmethod sqrt ((x integer))
  (let* ((n (cl:abs x))
	 (root (faster-isqrt n)))
    (unless (cl:= n (cl:* root root))
      (setq root (cl:sqrt n)))
    (if (minus? x) (cl:complex 0 root)
	root)))

(defmethod sqrt ((x rational-integer))
  (let ((domain (domain-of x)))
    (cond ((complete-set? domain)
	   (make-element domain (cl:sqrt (integer-value x))))
	  ((minus? (integer-value x))
	   (error "Can't take the sqrt of a negative number: ~S" x))
	  (t (let* ((n (integer-value x))
		    (root (faster-isqrt n)))
	       (cond ((cl:= n (cl:* root root))
		      (make-element domain root))
		     (t (error "~S does not have a sqrt in ~S"
			       x domain))))))))

(defmethod sqrt ((x rational-number))
  (let ((domain (domain-of x)))
    (cond ((complete-set? domain)
	   (make-element domain (cl:sqrt (cl:/ (qo-numerator x)
                                               (qo-denominator x)))))
	  (t (let* ((n (qo-numerator x))
		    (d (qo-denominator x))
		    (n-root (faster-isqrt n))
		    (d-root (faster-isqrt d)))
	       (cond ((and (cl:= n (cl:* n-root n-root))
			   (cl:= d (cl:* d-root d-root)))
		      (make-quotient-element domain n-root d-root))
		     (t (error "~S does not have a sqrt in ~S"
			       x domain))))))))

(defmethod sqrt ((x floating-point-number)) 
  (make-element (domain-of x) (cl:sqrt (fp-value x))))

(defmethod sqrt ((number bigfloat))
  (bind-domain-context (domain-of number)
    (bf-sqrt number *REAL-PRECISION*)))

(defmethod sqrt ((number complex-number))
  (let* ((x (cn-realpart number))
	 (y (cn-imagpart number))
	 (mag (/ (sqrt (+ (* x x) (* y y))) 2)))
    (make-element (domain-of number)
                  (sqrt (+ mag (/ x 2)))
                  (if (plus? y)
                      (sqrt (- mag (/ x 2)))
                      (- (sqrt (- mag (/ x 2))))))))

;; The following idea was stolen from the Mindy implementation of Dylan.
(defmacro with-contagion ((x y) &body body)
  (let (x-val y-val)
    (if (atom x) (setq x-val x)
	(setq x-val (second x)
	      x (first x)))
    (if (atom y) (setq y-val y)
	(setq y-val (second y)
	      y (first y)))
    `(multiple-value-bind (,x ,y) (contagion ,x-val ,y-val)
      ,@body)))

;; If this changes, you should also change the numeric-numeric part of
;; def-binary-coercions in morphisms.lisp
(defmacro define-binary-contagions (binary-op &key (numeric-numeric? t)
                                    (number-numeric? t))
  `(progn
    ,@(when number-numeric?
            `((defmethod ,binary-op ((.x. number) (.y. numeric))
                (with-contagion (.x. .y.) (,binary-op .x. .y.)))
              (defmethod ,binary-op ((.x. numeric) (.y. number))
                (with-contagion (.x. .y.) (,binary-op .x. .y.)))))
    ,(when numeric-numeric?
           `(defmethod ,binary-op ((.x. numeric) (.y. numeric))
             (let ((x-domain (domain-of .x.))
                   (y-domain (domain-of .y.)))
               (cond ((eql x-domain y-domain)
                      (when (eql (class-of .x.) (class-of .y.))
                        (error "No applicable contagion method for ~S and ~S" .x. .y.))
                      (with-contagion (.x. .y.) (,binary-op .x. .y.)))
                     ((typep (domain-of .x.) 'non-strict-domain)
                      (,binary-op .x. (coerce .y. (domain-of .x.))))
                     ((typep (domain-of .y.) 'non-strict-domain)
                      (,binary-op (coerce .x. (domain-of .y.)) .y.))
                     (t (call-next-method))))))))

(defgeneric contagion (number1 number2)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod contagion ((x number) (y number))
  (values x y))

(defmethod contagion ((x number) (y numeric))
  (values (coerce x (domain-of y)) y))

(defmethod contagion ((x numeric) (y number))
  (values x (coerce y (domain-of x))))

(defmethod-sd contagion ((x numeric) (y numeric))
  (values x y))

(defmethod-sd contagion ((x rational-integer) (y rational-number))
  (values (make-instance 'rational-number :domain domain
			 :numerator (integer-value x)
			 :denominator 1)
	  y))

(defmethod-sd contagion ((x rational-integer) (y floating-point-number))
  (values (make-element domain (float (integer-value x))) y))

(defmethod-sd contagion ((x rational-integer) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x rational-integer) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart x
			 :imagpart 0)
	  y))

(defmethod-sd contagion ((x rational-number) (y rational-integer))
  (values x
	  (make-instance 'rational-number :domain domain
			 :numerator (integer-value y)
			 :denominator 1)))

(defmethod-sd contagion ((x rational-number) (y floating-point-number))
  (values (make-element domain (cl:/ (float (qo-numerator x))
				     (qo-denominator x)))
	  y))

(defmethod-sd contagion ((x rational-number) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x rational-number) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (cl:/ (qo-denominator x)
					 (qo-numerator x))
			 :imagpart 0)
	  y))

(defmethod-sd contagion ((x floating-point-number) (y rational-integer))
  (values x (make-element domain (float (integer-value y)))))

(defmethod-sd contagion ((x floating-point-number) (y rational-number))
  (values x
	  (make-element domain (cl:/ (float (qo-numerator y))
				     (qo-denominator y)))))

(defmethod-sd contagion ((x floating-point-number) (y bigfloat))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x floating-point-number) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (fp-value x)
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x bigfloat) (y rational-integer))
  (values x (make-element domain (float (integer-value y)))))

(defmethod-sd contagion ((x bigfloat) (y rational-number))
  (values x
	  (make-element domain (cl:/ (float (qo-numerator y))
				     (qo-denominator y)))))

(defmethod-sd contagion ((x bigfloat) (y floating-point-number))
  (values (convert-to-bigfloat x) y))

(defmethod-sd contagion ((x bigfloat) (y complex-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart x
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x complex-number) (y rational-integer))
  (values x (make-instance 'complex-number :domain domain
			   :realpart (integer-value y)
			   :imagpart 0)))

(defmethod-sd contagion ((x complex-number) (y rational-number))
  (values x
	  (make-instance 'complex-number :domain domain
                         :realpart (cl:/ (qo-numerator y)
                                         (qo-denominator y))
                         :imagpart 0)))

(defmethod-sd contagion ((x complex-number) (y floating-point-number))
  (values (make-instance 'complex-number :domain domain
			 :realpart (fp-value x)
			 :imagpart 0.0)
	  y))

(defmethod-sd contagion ((x complex-number) (y bigfloat))
  (values (make-instance 'complex-number :domain domain
			 :realpart y
			 :imagpart 0.0)
	  y))

;; These routines don't really need the contagion tools. 

(defmethod binary= ((x number) (y number))
  (cl:= x y))

(defmethod binary= ((x number) (y numeric))
  (cl:= x (convert-to-lisp-number y)))

(defmethod binary= ((x numeric) (y number))
  (cl:= (convert-to-lisp-number x) y))

;; Unless otherwise, checked two number are never equal!
(defmethod binary= ((x numeric) (y numeric))
  nil)

(defmethod-sd binary= ((x rational-integer) (y rational-integer))
  (eql (integer-value x) (integer-value y)))

(defmethod-sd binary= ((x floating-point-number) (y floating-point-number) )
  (cl:= (fp-value x) (fp-value y)))

(defmethod-sd binary= ((x bigfloat) (y bigfloat))
  (bf-binary= x y))

(defmethod-sd binary= ((x complex-number) (y complex-number))
  (and (= (cn-realpart x) (cn-realpart y))
       (= (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions binary>
    :number-numeric? nil)

(defmethod binary> ((x number) (y number))
  (cl:> x y))

(defmethod binary> ((x number) (y numeric))
  (cl:> x (convert-to-lisp-number y)))

(defmethod binary> ((x numeric) (y number))
  (cl:> (convert-to-lisp-number x) y))

(defmethod-sd binary> ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (cl:> (integer-value x) (integer-value y))
      (call-next-method)))

(defmethod-sd binary> ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (cl:> (cl:* (qo-numerator x) (qo-denominator y))
	      (cl:* (qo-numerator y) (qo-denominator x)))
      (call-next-method)))

(defmethod-sd binary> ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (cl:> (fp-value x) (fp-value y))
      (call-next-method)))

(define-binary-contagions binary>=
    :number-numeric? nil)

(defmethod binary>= ((x number) (y number))
  (cl:>= x y))

(defmethod binary>= ((x number) (y numeric))
  (cl:>= x (convert-to-lisp-number y)))

(defmethod binary>= ((x numeric) (y number))
  (cl:>= (convert-to-lisp-number x) y))

(defmethod-sd binary>= ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (cl:>= (integer-value x) (integer-value y))
      (call-next-method)))

(defmethod-sd binary>= ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (cl:>= (cl:* (qo-numerator x) (qo-denominator y))
             (cl:* (qo-numerator y) (qo-denominator x)))
      (call-next-method)))

(defmethod-sd binary>= ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (cl:>= (fp-value x) (fp-value y))
      (call-next-method)))

(define-binary-contagions max-pair)

(defmethod max-pair ((x number) (y number))
  (if (cl:> x y) x y))

;; Added the following two methods. Look at the related note for
;; min-pair.                             -- Sekhar 8/3/94

(defmethod max-pair :around ((x number) (y t))
  (cond ((eql x *positive-infinity*) x)
	((eql x *negative-infinity*) y)
	(t (call-next-method))))

(defmethod max-pair :around ((x t) (y number))
  (cond ((eql y *positive-infinity*) y)
	((eql y *negative-infinity*) x)
	(t (call-next-method))))

(defmethod-sd max-pair ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (if (cl:>= (integer-value x) (integer-value y))
	  x y)
      (call-next-method)))

(defmethod-sd max-pair ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (if (cl:>= (cl:* (qo-numerator x) (qo-denominator y))
		 (cl:* (qo-numerator y) (qo-denominator x)))
	  x y)
      (call-next-method)))

(defmethod-sd max-pair ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (if (cl:>= (fp-value x) (fp-value y))
	  x y)
      (call-next-method)))

(define-binary-contagions min-pair)

(defmethod min-pair ((x number) (y number))
  (if (cl:> x y) y x))

;; Added the following two methods to fix a problem occurred while
;; while exponentiating a tpower-series by an integer. -- Sekhar 8/3/94

(defmethod min-pair :around ((x number) (y t))
  (cond ((eql x *positive-infinity*) y)
	((eql x *negative-infinity*) x)
	(t (call-next-method))))

(defmethod min-pair :around ((x t) (y number))
  (cond ((eql y *positive-infinity*) x)
	((eql y *negative-infinity*) y)
	(t (call-next-method))))

(defmethod-sd min-pair ((x rational-integer) (y rational-integer))
  (if (ordered-domain? domain)
      (if (cl:<= (integer-value x) (integer-value y))
	  x y)
      (call-next-method)))

(defmethod-sd min-pair ((x rational-number) (y rational-number))
  (if (ordered-domain? domain)
      (if (cl:<= (cl:* (qo-numerator x) (qo-denominator y))
                 (cl:* (qo-numerator y) (qo-denominator x)))
	  x y)
      (call-next-method)))

(defmethod-sd min-pair ((x floating-point-number) (y floating-point-number))
  (if (ordered-domain? domain)
      (if (cl:<= (fp-value x) (fp-value y))
	  x y)
      (call-next-method)))

(define-binary-contagions plus)

(defmethod plus ((x number) (y number))
  (cl:+ x y))

(defmethod-sd plus ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:+ (integer-value x) (integer-value y))))

(defmethod-sd plus ((x rational-number) (y rational-number))
  (make-element domain
		(cl:+ (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd plus ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:+ (fp-value x) (fp-value y))))

(defmethod-sd plus ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-plus x y) *REAL-PRECISION*)))

(defmethod-sd plus ((x complex-number) (y complex-number))
  (make-element domain
		(+ (cn-realpart x) (cn-realpart y))
		(+ (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions difference)

(defmethod difference ((x number) (y number))
  (cl:- x y))

(defmethod-sd difference ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:- (integer-value x) (integer-value y))))

(defmethod-sd difference ((x rational-number) (y rational-number))
  (make-element domain
		(cl:- (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd difference ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:- (fp-value x) (fp-value y))))

(defmethod-sd difference ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-difference x y) *REAL-PRECISION*)))

(defmethod-sd difference ((x complex-number) (y complex-number))
  (make-element domain
		(- (cn-realpart x) (cn-realpart y))
		(- (cn-imagpart x) (cn-imagpart y))))

(define-binary-contagions times)

(defmethod times ((x number) (y number))
  (cl:* x y))

(defmethod-sd times ((x rational-integer) (y rational-integer))
  (make-instance 'rational-integer :domain domain
		 :value (cl:* (integer-value x) (integer-value y))))

(defmethod-sd times ((x rational-number) (y rational-number))
  (make-element domain
		(cl:* (cl:/ (qo-numerator x) (qo-denominator x))
		      (cl:/ (qo-numerator y) (qo-denominator y)))))

(defmethod-sd times ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:* (fp-value x) (fp-value y))))

(defmethod-sd times ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-times x y) *REAL-PRECISION*)))

(defmethod-sd times ((x complex-number) (y complex-number))
  (let ((x-real (cn-realpart x))
	(x-imag (cn-imagpart x))
	(y-real (cn-realpart y))
	(y-imag (cn-imagpart y)))
    (make-element domain
                  (- (* x-real y-real) (* x-imag y-imag))
                  (+ (* x-real y-imag) (* x-imag y-real)))))

(define-binary-contagions quotient)

(defmethod quotient ((x number) (y number))
  (cl:/ x y))

;; Changed this method so that if x|y then we get an integer. Made this
;; change to fix a problem occurred while exponentiating a tpower-series
;; by an integer.                               -- Sekhar 8/3/94
(defmethod-sd quotient ((x rational-integer) (y rational-integer))
  (if (1? y) x
      (let ((quo (cl:/ (integer-value x) (integer-value y))))
        (cond ((typep quo 'integer)
               (make-element domain quo))
              ((or (field? domain)
                   (typep domain 'non-strict-domain))
               (make-element domain quo))
              (t (call-next-method))))))

(defmethod-sd quotient ((x rational-number) (y rational-number))
  (make-element domain
		(cl:/ (cl:* (qo-numerator x) (qo-denominator y))
		      (cl:* (qo-numerator y) (qo-denominator x)))))

(defmethod-sd quotient ((x floating-point-number) (y floating-point-number))
  (make-element domain (cl:/ (fp-value x) (fp-value y))))

(defmethod-sd quotient ((x bigfloat) (y bigfloat))
  (bind-domain-context domain
    (round!mt (bf-quotient x y *REAL-PRECISION*)
	      *REAL-PRECISION*)))

(defmethod-sd quotient ((x complex-number) (y complex-number))
  (let* ((x-real (cn-realpart x))
	 (x-imag (cn-imagpart x))
	 (y-real (cn-realpart y))
	 (y-imag (cn-imagpart y))
	 (norm (+ (* y-real y-real) (* y-imag y-imag))))
    (make-element (domain-of x)
		  (/ (+ (* x-real y-real) (* x-imag y-imag)) norm)
		  (/ (- (* x-imag y-real) (* x-real y-imag)) norm))))

(defmethod expt ((n number) (e number))
  (cl:expt n e))

(defmethod expt ((n integer) (e ratio))
  (let* ((num (cl:numerator e))
	 (den (cl:denominator e))
	 (nn (abs n))
	 (root (integer-nth-root nn den)))
    (setq root
	  (if (and root (cl:= nn (cl:expt root den)))
	      (cl:expt root num)
	      (cl:expt nn e)))
    (cond ((cl:minusp n)
	   (if (cl:evenp den)
	       (cl:complex 0 root)
	       (cl:- root)))
	  (t root))))

(defmethod expt ((n ratio) (e ratio))
  (cl:/ (expt (cl:numerator n) e)
        (expt (cl:denominator n) e)))

(defmethod expt ((n rational-integer) (e integer))
  (let ((domain (domain-of n)))
    (cond ((1? n) (one domain))
	  ((cl:minusp e)
	   (if (or (field? domain)
		   (typep domain 'non-strict-domain))
	       (make-quotient-element domain 1
				      (cl:expt (integer-value n) (cl:- e)))
	       (error "Raising ~D to a negative power ~D" n e)))
	  (t (if (eql (integer-value n) -1)
		 (if (oddp e) (- (one domain)) (one domain))
		 (make-element (domain-of n)
			       (cl:expt (integer-value n) e)))))))

(defmethod-sd expt ((n rational-integer) (e rational-integer))
  (cond ((1? n) (one domain))
	((cl:minusp (integer-value e))
	 (if (or (field? domain)
		 (typep domain 'non-strict-domain))
	     (make-quotient-element domain 1 (integer-value n))
	     (error "Raising ~D to a negative power ~D" n e)))
	(t (if (eql (integer-value n) -1)
	       (if (oddp (integer-value e)) (- (one domain)) (one domain))
	       (make-element (domain-of n)
                             (cl:expt (integer-value n) (integer-value e)))))))

(defmethod expt ((n rational-integer) (e ratio))
  (let* ((domain (domain-of n))
	 (nn (integer-value n))
	 (abs-nn (cl:abs nn))
	 (num (cl:numerator e))
	 (den (cl:denominator e))
	 (root (integer-nth-root abs-nn den)))
    (setq root 
	  (cond ((cl:= abs-nn (cl:expt root den))
		 (cl:expt root num))
		((complete-set? domain)
		 (cl:expt nn e))
		(t (error "Can't compute ~S to the ~S power in ~S"
			  n e domain))))
    (cond ((cl:minusp nn)
	   (cond ((cl:oddp den)
		  (make-element domain (cl:- root)))
		 ((complete-set? domain)
		  (make-element domain (cl:complex 0 root)))
		 (t (error "Can't compute ~S to the ~S power in ~S"
			   n e domain))))
	  (t (make-element domain root)))))

(defmethod expt ((n rational-integer) (e rational-number))
  (let* ((domain (domain-of n))
	 (nn (integer-value n))
	 (abs-nn (cl:abs nn))
	 (num (qo-numerator e))
	 (den (qo-denominator e))
	 (root (integer-nth-root abs-nn den)))
    (setq root 
	  (cond ((cl:= abs-nn (cl:expt root den))
		 (cl:expt root num))
		((complete-set? domain)
		 (cl:expt nn e))
		(t (error "Can't compute ~S to the ~S power in ~S"
			  n e domain))))
    (cond ((cl:minusp nn)
	   (cond ((cl:oddp den)
		  (make-element domain (cl:- root)))
		 ((complete-set? domain)
		  (make-element domain (cl:complex 0 root)))
		 (t (error "Can't compute ~S to the ~S power in ~S"
			   n e domain))))
	  (t (make-element domain root)))))

(defmethod expt ((x rational-number) (y ratio))
  (/ (expt (numerator x) y)
     (expt (denominator x) y)))

(defmethod expt ((x rational-number) (y rational-number))
  (/ (expt (numerator x) y)
     (expt (denominator x) y)))

(defmethod expt ((x floating-point-number) (y number))
  (make-element (domain-of x) (cl:expt (fp-value x) y)))

(defmethod expt ((x floating-point-number) (y rational-integer))
  (make-element (domain-of x) (cl:expt (fp-value x) (integer-value y))))

(defmethod expt ((x floating-point-number) (y rational-number))
  (make-element (domain-of x)
		(cl:expt (fp-value x)
                         (cl:/ (numerator y) (denominator y)))))

(defmethod expt ((x floating-point-number) (y floating-point-number))
  (make-element (domain-of x) (cl:expt (fp-value x) (fp-value y))))

(defmethod expt ((number bigfloat) (k integer))
  (cond ((eql k 0) (make-bigfloat (domain-of number) 1 0))
	((eql k 1) number)
	(t (let ((domain (domain-of number)))
	     (bind-domain-context domain
	       (bf-expt number k *REAL-PRECISION*))))))

;; FIXTHIS: exponentiation of complex numbers needs to be improved 
(defmethod expt ((x complex-number) (y integer))
  (cond ((minusp y) (recip (expt x (- y))))
	((zerop y) (one (domain-of x)))
	((1? y) x)
	(t (let ((half (expt x (ash y -1))))
	     (if (oddp y) (* half half x)
		 (* half half))))))

;; The first value returned by TRUNCATE is an integer for numbers, and
;; is returned in the domain of the first argument.  The second value
;; is returned in the domain of the second argument.

(defmethod truncate1 ((a number))
  (cl:truncate a))

(defmethod truncate1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod truncate1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:truncate (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod truncate1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:truncate (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod truncate1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod truncate2 ((a number) (b number))
  (cl:truncate a b))

(defmethod truncate2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:truncate (convert-to-lisp-number a)
					  (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod truncate2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod truncate2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod floor1 ((a number))
  (cl:floor a))

(defmethod floor1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod floor1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:floor (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod floor1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:floor (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod floor1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod floor2 ((a number) (b number))
  (cl:floor a b))

(defmethod floor2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:floor (convert-to-lisp-number a)
                                       (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod floor2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod floor2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod ceiling1 ((a number))
  (cl:ceiling a))

(defmethod ceiling1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod ceiling1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:ceiling (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod ceiling1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:ceiling (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod ceiling1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod ceiling2 ((a number) (b number))
  (cl:ceiling a b))

(defmethod ceiling2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:ceiling (convert-to-lisp-number a)
                                         (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod ceiling2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod ceiling2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod round1 ((a number))
  (cl:round a))

(defmethod round1 ((a rational-integer))
  (values a (zero (domain-of a))))

(defmethod round1 ((a rational-number))
  (multiple-value-bind (q r)
      (cl:round (qo-numerator a) (qo-denominator a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod round1 ((a floating-point-number))
  (multiple-value-bind (q r) (cl:round (fp-value a))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of a) r))))

(defmethod round1 ((a complex-number))
  (error "Improper numeric argument"))

(defmethod round2 ((a number) (b number))
  (cl:round a b))

(defmethod round2 ((a numeric) (b numeric))
  (multiple-value-bind (q r) (cl:round (convert-to-lisp-number a)
                                       (convert-to-lisp-number b))
    (values (make-element (domain-of a) q)
	    (make-element (domain-of b) r))))

(defmethod round2 ((a numeric) (b complex-number))
  (error "Improper numeric argument"))

(defmethod round2 ((a complex-number) (b numeric))
  (error "Improper numeric argument"))

(defmethod remainder ((a number) (b number))
  (cl:rem a b))

(defmethod-sd remainder ((a rational-integer) (b rational-integer))
  (make-element domain (cl:rem (integer-value a) (integer-value b))))

(defgeneric even? (number)
  (:documentation
   "Return true if the number is even."))

(defmethod even? ((a number))
  (cl:evenp a))

(defmethod even? ((a rational-integer))
  (cl:evenp (integer-value a)))

(defgeneric oddp? (number)
  (:documentation
   "Return true if the number is odd."))

(defmethod oddp? ((a number))
  (cl:evenp a))

(defmethod oddp? ((a rational-integer))
  (cl:evenp (integer-value a)))

(defmethod binary-gcd ((a integer) (b integer))
  (cl:gcd a b))

;; Do we really need this???
(defmethod binary-gcd ((a float) (b float))
  a)

;; All this extra stuff is because this method and lcm below override
;; the definitions in morphisms.lisp

(defmethod binary-gcd ((a numeric) (b numeric))
  (let ((a-domain (domain-of a)) (b-domain (domain-of b)))
    (cond ((eql a-domain b-domain) (one a-domain))
          ((typep (domain-of a) 'non-strict-domain)
           (gcd a (coerce b (domain-of a))))
          ((typep (domain-of b) 'non-strict-domain)
           (gcd (coerce a (domain-of b)) b))
          (t (call-next-method)))))

(defmethod-sd binary-gcd ((a rational-integer) (b rational-integer))
  (make-element domain (cl:gcd (integer-value a) (integer-value b))))

(defmethod binary-lcm ((a integer) (b integer))
  (cl:* (cl:/ a (cl:gcd a b)) b))

(defmethod binary-lcm ((a numeric) (b numeric))
  (let ((a-domain (domain-of a)) (b-domain (domain-of b)))
    (cond ((eql a-domain b-domain) (* a b))
          ((typep (domain-of a) 'non-strict-domain)
           (gcd a (coerce b (domain-of a))))
          ((typep (domain-of b) 'non-strict-domain)
           (gcd (coerce a (domain-of b)) b))
          (t (call-next-method)))))

(defmethod-sd binary-lcm ((a rational-integer) (b rational-integer))
  (let ((a (integer-value a))
	(b (integer-value b)))
    (make-element domain (cl:* (cl:/ a (cl:gcd a b)) b))))

(defun extended-gcd* (a b)
  (if (= b 0)
      (values a 1 0)
      (multiple-value-bind (d x y) (extended-gcd* b (mod a b))
	(values d y (cl:- x (cl:* (cl:floor a b) y))))))

(defgeneric extended-gcd (numerator denominator)
  (:documentation
   "Return the greatest common denominator."))

(defmethod extended-gcd ((a integer) (b integer))
  (multiple-value-bind (d x y) (extended-gcd* (abs a) (abs b))
    (values (cl:* (signum a) x) (cl:* (signum b) y) d)))

;; Some single argument functions

(defmethod sin ((x floating-point-number))
  (make-element (domain-of x) (cl:sin (fp-value x))))

(defmethod cos ((x floating-point-number))
  (make-element (domain-of x) (cl:cos (fp-value x))))

(defmethod tan ((x floating-point-number))
  (make-element (domain-of x) (cl:tan (fp-value x))))

(defmethod asin ((x floating-point-number))
  (make-element (domain-of x) (cl:asin (fp-value x))))

(defmethod acos ((x floating-point-number))
  (make-element (domain-of x) (cl:acos (fp-value x))))

(defmethod sinh ((x floating-point-number))
  (make-element (domain-of x) (cl:sinh (fp-value x))))

(defmethod cosh ((x floating-point-number))
  (make-element (domain-of x) (cl:cosh (fp-value x))))

(defmethod tanh ((x floating-point-number))
  (make-element (domain-of x) (tanh (fp-value x))))

(defmethod asinh ((x floating-point-number))
  (make-element (domain-of x) (cl:asinh (fp-value x))))

(defmethod acosh ((x floating-point-number))
  (make-element (domain-of x) (cl:acosh (fp-value x))))

(defmethod atanh ((x floating-point-number))
  (make-element (domain-of x) (cl:atanh (fp-value x))))

(defmethod exp ((x floating-point-number))
  (make-element (domain-of x) (cl:exp (fp-value x))))

(defmethod log ((x floating-point-number))
  (make-element (domain-of x) (cl:log (fp-value x))))
;;; -*- Mode:Lisp; Package:Weyli; Syntax:Common-Lisp; Base:10; Lowercase:T -*-
;;; ===========================================================================
;;;				  GF(p)
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; gfp.lisp,v 1.8 1995/05/24 17:42:01 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.8")

(define-domain-element-classes GFp GFp-element)

(defgeneric number-of-elements (domain)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod number-of-elements ((domain GFp))
  (characteristic domain))

(defmethod number-of-elements ((domain GFq))
  (expt (characteristic domain) (field-degree domain)))

(defgeneric make-GFp-domain (number number~)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-GFp-domain ((characteristic integer) (degree integer))
  (cond ((= degree 1)
	 (let ((domain (make-instance 'gfp :characteristic characteristic
				      :print-function 'GFp-print-object))
	       (Z (get-rational-integers)))
	   (make-homomorphism
            Z
            (lambda (x)
              (make-element domain
                            (if (cl:integerp x) x
                                (integer-value x))))
            domain)
	   domain))
 	(t (error "Can't do GF(~D^~D) yet" characteristic degree)
	   ;; This is where GFq domains are to be defined.
	   )))

(defun GFp-print-object (d stream)
  #+Genera
  (format stream "~'bGF~(~D)" (characteristic d))
  #-Genera
  (format stream "GF(~D)" (characteristic d)))

(defmethod make-element ((domain GFp) (value integer) &rest ignore)
  (declare (ignore ignore))
  (let ((modulus (characteristic domain)))
    (make-instance 'GFp-element
		   :domain domain
		   :value (reduce-modulo-integer value modulus))))

;; Could have more error checking
(defmethod weyl::make-element ((domain GFp) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-element domain value))

(defvar *print-modulus* t)

(defmethod print-object ((x GFp-element) stream)
  (with-slots (value domain) x
    (if *print-modulus*
	(format stream "~D(~D)" value (characteristic domain))	
	(format stream "~D" value))))

(defun compute-inverse (value modulus)
  (let ((a1 modulus)
	(a2 (if (cl:< value 0) (cl:+ value modulus) value))
	(y1 0)
	(y2 1)
	q)
    (loop
     (if (eql a2 1) (return (values y2 y1)))
     (if (cl:zerop a2)
         (error "Inverse of zero divisor -- ~d modulo ~d"
                value modulus))
     (setq q (truncate a1 a2))
     (psetq a1 a2 a2 (cl:- a1 (cl:* a2 q)))
     (psetq y1 y2 y2 (cl:- y1 (cl:* y2 q))))))

(defmethod coerce ((value ratio) (domain GFp))
  (make-element
   domain
   (cl:* (cl:numerator value)
         (compute-inverse (cl:denominator value)
                          (characteristic domain)))))

(defmethod coerce ((value rational-integer) (domain GFp))
  (make-element domain (integer-value value)))

(defmethod coerce ((value integer) (domain GFp))
  (make-element domain value))

(defmethod coerce ((element gfp-element) (domain general-expressions))
  (coerce (gfp-value element) domain))

(defmethod-sd binary= ((x GFp-element) (y GFp-element))
  (with-slots ((v1 value) (d1 domain)) x
    (with-slots ((v2 value) (d2 domain)) y
      (and (eq d1 d2) (eql v1 v2)))))

(defmethod 0? ((x GFp-element))
  (with-slots (value) x
    (cl:zerop value)))

(defmethod 1? ((x GFp-element))
  (with-slots (value) x
    (eql value 1)))

;; The following three methods make finite fields behave like quotient fields

(defmethod make-quotient-element ((domain GFp) (a GFp-element) (b GFp-element))
  (unless (eql domain (domain-of a))
    (error "~S should be an element of ~S" a domain))
  (unless (eql domain (domain-of b))
    (error "~S should be an element of ~S" b domain))
  (with-slots ((v1 value)) a
    (with-slots ((v2 value)) b
      (with-slots (characteristic) domain
	(make-element domain
                      (cl:* v1 (compute-inverse v2 characteristic)))))))

(defmethod numerator ((a GFp-element))
  a)

(defmethod denominator ((a GFp-element))
  (make-element (domain-of a) 1))

(defmethod minus ((x GFp-element))
  (with-slots (value domain) x
    (with-slots (characteristic) domain
      (if (eql 2 characteristic) x
	  (make-element domain (cl:- characteristic value))))))

;;; There is no such thing as a negative number in finite fields.
(defmethod minus? ((x GFp-element))
  nil)

(defmethod plus? ((x GFp-element))
  (not (0? x)))

(defmethod-sd plus ((a GFp-element) (b GFp-element))
  (make-element domain (cl:+ (gfp-value a) (gfp-value b))))

(defmethod-sd difference ((a GFp-element) (b GFp-element))
  (make-element domain (cl:- (gfp-value a) (gfp-value b))))

(defmethod-sd times ((a GFp-element) (b GFp-element))
  (make-element domain (cl:* (gfp-value a) (gfp-value b))))

;; Takes the inverse of an integer N mod P.  Solve N*X + P*Y = 1.  N
;; is guaranteed to be less than P, since in the case where P is a
;; fixnum, N is also assumed to be one.

(defmethod recip ((x GFp-element))
  (with-slots (value domain) x
    (with-slots (characteristic) domain
      (make-element domain (reduce-modulo-integer
			    (compute-inverse value characteristic)
			    characteristic)))))

(defmethod expt ((x GFp-element) (e integer))
  (with-slots (value domain) x
    (cond ((eql 1 value) x)
	  ((cl:minusp e)
	   (error "Raising ~D to a negative power ~D" x e))
	  (t (make-element domain
	       (expt-modulo-integer value e (characteristic domain)))))))

(defmethod quotient ((a GFp-element) (b GFp-element)) 
  (with-slots ((v1 value) (d1 domain)) a
    (with-slots ((v2 value) (d2 domain)) b
      (cond ((eq d1 d2)
	     (with-slots (characteristic) d1
	       (make-element d1
		 (cl:* v1 (compute-inverse v2 characteristic)))))
	    (t (error "Taking the quotient of elements of ~
		       different fields: ~S, ~S"
		      a b))))))

(defmethod remainder ((a GFp-element) (b GFp-element))
  (error "Computing the remainder of ~D by ~D"
	 a b))

(defmethod binary-gcd ((a GFp-element) (b GFp-element))
  (with-slots ((d1 domain)) a
    (with-slots ((d2 domain)) b
      (cond ((eq d1 d2) (make-element d1 1))
	    (t (error "Taking the GCD of elements of different fields: ~S, ~S"
		      a b))))))

(defmethod binary-lcm ((a GFp-element) (b GFp-element))
  (with-slots ((d1 domain)) a
    (with-slots ((d2 domain)) b
      (cond ((eq d1 d2) (make-element d1 1))
	    (t (error "Taking the LCM of elements of different fields: ~S, ~S"
		      a b))))))

(defmethod random ((domain GFp) &optional height)
  (declare (ignore height))
  (make-element domain (cl:random (characteristic domain))))

(defmethod height ((x GFp-element))
  (make-element (get-real-numbers) (characteristic (domain-of x))))

(defgeneric multiplicative-order (element)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod multiplicative-order ((a GFp-element))
  (with-slots (value domain) a
    (with-slots ((p  characteristic)) domain
      (cond ((not (eql 1 (cl:gcd value p)))
	     *positive-infinity*)
	    ((let ((group-order (totient p)))
	       (do ((factors (factor group-order)
			     (rest factors))
		    (order group-order))
		   ((null factors)
		    order)
		 (do ((i 0 (cl:1+ i)))
		     ((cl:= i (cdar factors)))
		   (setq order (cl:/ order (caar factors)))
		   (when (not (eql 1 (expt-modulo-integer value order p)))
		     (setq order (cl:* order (caar factors)))
		     (return t))))))))))

;; GF(2^n)
(defvar *GF2-irreducible-polynomials*
  '(#O7 #O13 #O23 #O45 #O103 #O211 #O435 #O1021 #O2011 #O4005 #O10123
    #O20033 #O42103 #O100003 #O210013))

(defmethod make-GFp-domain ((characteristic (eql 2)) (degree integer))
  (cond ((= degree 1)
	 (make-instance 'gfp :characteristic characteristic))
	((< degree (+ (length *GF2-irreducible-polynomials*) 2))
	 (let* ((mask (ash 1 degree))
		(field (1- mask))
		(min-poly (logand (nth (- degree 2) *GF2-irreducible-polynomials*)
				  field))
                domain Z)
           (setq domain
                 (make-instance 'GF2^n 
                                :degree degree
                                :reduction-table
                                (loop for i below degree
                                      for x^n = min-poly then (ash x^n 1)
                                      collect
                                      (if (cl:zerop (logand mask x^n)) x^n
                                          (setq x^n (logxor (logand field x^n) min-poly))))
                                :characteristic characteristic
				:print-function 'GF2^n-print-object))
           (setq Z (get-rational-integers))
           (make-homomorphism Z #'(lambda (x)
                                    (coerce (integer-value x) domain))
                              domain)
           domain))
	(t (error "Table doesn't go far enough: 2^~D" degree))))

(defun GF2^n-print-object (domain stream)
  #+Genera
  (format stream "~'bGF~(2^~D)" (field-degree domain))
  #-Genera
  (format stream "GF(2^~D)" (field-degree domain)))

(defclass GF2^n-element (GFp-element) 
  ())

(defmethod print-object ((elt GF2^n-element) stream)
  (format stream "~V,'0B(2^~D)"
	  (field-degree (domain-of elt)) (GFp-value elt)
          (field-degree (domain-of elt))))

(defmethod make-element ((domain GF2^n) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'GF2^N-element
		 :domain domain
		 :value (logand (1- (ash 1 (field-degree domain))) value)))

;; Could have more error checking
(defmethod weyl::make-element ((domain GF2^n) (value integer) &rest ignore)
  (declare (ignore ignore))
  (make-element domain value))

(defmethod coerce ((value integer) (domain GF2^n))
  (make-element domain (if (zerop value) 0 1)))

(defmethod coerce ((value ratio) (domain GF2^n))
  (make-element domain (if (zerop value) 0 1)))

(defmethod multiplicative-order ((a GF2^n-element))
  (let ((group-size (1- (number-of-elements (domain-of a)))))
    (loop for order in (all-divisors group-size)
	  do (when (1? (expt a order))
	       (return order)))))

(defmethod-sd plus ((a GF2^n-element) (b GF2^n-element))
  (make-element domain (logxor (gfp-value a) (gfp-value b))))

(defmethod-sd times ((a GF2^n-element) (b GF2^n-element))
  (let ((x (Gfp-value a))
	(y (GFp-value b))
	(degree (field-degree domain))
	(acc 0) answer)
    (loop while (not (cl:zerop y)) do
          (when (not (cl:zerop (cl:logand 1 y)))
            (setq acc (cl:logxor acc x)))
          (setq x (cl:ash x 1))
          (setq y (cl:ash y -1)))
    (setq answer (cl:logand (cl:1- (cl:ash 1 degree)) acc))
    (loop for hi-bits = (cl:ash acc (cl:- degree))
          then (cl:ash hi-bits -1)
	  for poly in (GFp-reduction-table domain)
	  while (not (cl:zerop hi-bits))
	  do (unless (cl:zerop (cl:logand 1 hi-bits))
	       (setq answer (cl:logxor answer poly))))
    (make-instance 'GF2^N-element :domain domain :value answer)))    

(defmethod expt ((base GF2^n-element) (expt integer))
  (%funcall (repeated-squaring #'times (make-element (domain-of base) 1))
            base expt))

(defmethod recip ((x GF2^n-element))
  (let ((domain (domain-of x)))
    (expt x (cl:- (cl:expt 2 (field-degree domain)) 2))))

(defmethod-sd quotient ((x GF2^n-element) (y GF2^n-element))
  (* x (recip y)))

;; GF(m)

;; This domain is the union of all Z/mZ for all m.

(define-domain-element-classes GFm GFm-element)

(defun make-gfm-domain ()
  (let ((domain (make-instance 'gfm))
	(Z (get-rational-integers)))
    (make-homomorphism Z #'(lambda (x)
			     (make-element domain (integer-value x) 0))
		       domain)
    domain))

(defmethod make-element ((domain GFm) value &rest rest)
  (let ((modulus (first rest)))
    (make-instance 'GFm-element
		   :domain domain
		   :value (reduce-modulo-integer value modulus)
		   :modulus modulus)))

;; Could have more error checking
(defmethod weyl::make-element ((domain GFm) value &rest rest)
  (%apply #'make-element domain value rest))

(defmethod print-object ((x GFm-element) stream)
  (with-slots (value modulus) x
    (format stream "~D(~D)" value modulus)))

(defmethod coerce ((value integer) (domain GFm))
  (make-element domain value 0))

(defmethod coerce ((elt GFp-element) (domain GFm))
  (with-slots ((v1 value) (d1 domain)) elt
    (make-element domain v1 (characteristic d1))))

(defmethod-sd binary= ((x GFm-element) (y GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) x
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) y
      (and (eq d1 d2) (eql v1 v2) (eql m1 m2)))))

(defmethod 0? ((x GFm-element))
  (with-slots (value) x
    (cl:zerop value)))

(defmethod 1? ((x GFm-element))
  (with-slots (value) x
    (eql value 1)))

(defmethod minus ((x GFm-element))
  (with-slots (value modulus domain) x
    (if (eql 2 modulus) x
	(make-element domain (cl:- modulus value) modulus))))

;;; There is no such thing as a negative number in finite fields.
(defmethod minus? ((x GFm-element))
  nil)

(defmethod plus? ((x GFm-element))
  (not (0? x)))

(defmethod-sd plus ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:+ v1 v2) m1))
	    (t (make-element d1 (cl:+ v1 v2) (cl:lcm m1 m2)))))))

(defmethod-sd difference ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:- v1 v2) m1))
	    (t (make-element d1 (cl:- v1 v2) (cl:lcm m1 m2)))))))

(defmethod-sd times ((a GFm-element) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (with-slots ((v2 value) (m2 modulus) (d2 domain)) b
      (cond ((not (eql d1 d2))
	     (error "~S and ~S are not from the same domain" a b))
	    ((eql m1 m2)
	     (make-element d1 (cl:* v1 v2) m1))
	    (t (make-element d1 (cl:* v1 v2) (cl:lcm m1 m2)))))))

(defmethod plus ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:+ v1 (reduce-modulo-integer b m1)) m1)))

(defmethod plus ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:+ (reduce-modulo-integer a m1) v1) m1)))

(defmethod difference ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:- v1 (reduce-modulo-integer b m1)) m1)))

(defmethod difference ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:- (reduce-modulo-integer a m1) v1) m1)))

(defmethod times ((a GFm-element) (b integer))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) a
    (make-element d1 (cl:* v1 (reduce-modulo-integer b m1)) m1)))

(defmethod times ((a integer) (b GFm-element))
  (with-slots ((v1 value) (m1 modulus) (d1 domain)) b
    (make-element d1 (cl:* (reduce-modulo-integer a m1) v1) m1)))

;;; Takes the inverse of an integer N mod P.  Solve N*X + P*Y = 1.  N
;;; is guaranteed to be less than P, since in the case where P is a
;;; fixnum, N is also assumed to be one.

(defmethod recip ((x GFm-element))
  (with-slots (value modulus domain) x
    (make-element domain (reduce-modulo-integer (compute-inverse value modulus)
						modulus)
		  modulus)))

(defmethod expt ((x GFm-element) (e integer))
  (with-slots (value modulus domain) x
    (cond ((eql 1 value) x)
	  ((cl:minusp e)
	   (error "Raising ~D to a negative power ~D" x e))
	  (t (make-element domain (expt-modulo-integer value e modulus)
			   modulus)))))

(defmethod-sd  quotient ((a GFm-element) (b GFm-element)) 
  (with-slots ((v1 value) (m1 modulus)) a
    (with-slots ((v2 value) (m2 modulus)) b
      (make-element domain (cl:* v1 (compute-inverse v2 m2)) m1))))
  
(defmethod remainder ((a GFm-element) (b GFm-element))
  (error "Computing the remainder of ~D by ~D" a b))

(defmethod binary-gcd ((a GFm-element) (b GFm-element))
  (declare (ignore b))
  (with-slots ((m1 modulus) (d1 domain)) a
    (make-element d1 1 m1)))

(defmethod-sd binary-lcm ((a GFm-element) (b GFm-element))
  (with-slots ((m1 modulus)) a
    (make-element domain 1 m1)))

(defmethod multiplicative-order ((a GFm-element))
  (with-slots (value modulus) a
    (cond ((not (eql 1 (cl:gcd value modulus)))
	   *positive-infinity*)
	  ((let ((group-order (totient modulus)))
	     (do ((factors (factor group-order)
			   (rest factors))
		  (order group-order))
		 ((null factors)
		  order)
	       (do ((i 0 (cl:1+ i)))
		   ((cl:= i (cdar factors)))
		 (setq order (cl:/ order (caar factors)))
		 (when (not (eql 1 (expt-modulo-integer value order modulus)))
		   (setq order (cl:* order (caar factors)))
		   (return t)))))))))

;; These are the guys that actually create the finite fields.
(defun make-finite-field* (size)
  (cond ((null size)
	 (make-gfm-domain))
	((prime? size)
	 (make-GFp-domain size 1))
	(t (let* ((s (factor size))
		  (char (first (first s)))
		  (degree (rest (first s))))
	     (if (null (rest s))
		 (make-Gfp-domain char degree)
		 (error "Finite fields of size ~S=~S don't exist" size s))))))

(defun make-finite-field (&optional size)
  (add-domain #'false (make-finite-field* size)))

;; This is slightly inefficient, but who cares...  I want to localize
;; the knowledge of how to create domains in the MAKE-...* functions.
(defun get-finite-field (&optional size)
  (cond ((null size)
	 (add-domain #'(lambda (d) (eql (class-name (class-of d)) 'GFm))
	   (make-finite-field* size)))
	((prime? size)
	 (add-domain #'(lambda (d)
		         (and (eql (class-name (class-of d)) 'GFp)
			      (eql (characteristic d) size)))
	   (make-finite-field* size)))
        ((null (rest (factor size)))
         (add-domain #'(lambda (d)
                         (and (eql (class-name (class-of d)) 'GF2^n)
                              (eql (cl:expt (characteristic d)
                                            (field-degree d))
                                   size)))
	   (make-finite-field* size)))
	(t (error "Can't do algebraic extensions yet"))))

(defgeneric get-factor-ring (ring ideal)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-factor-ring ((ring rational-integers) (ideal ideal))
  (cond ((eql (ring-of ideal) ring)
	 (get-finite-field (first (generators-of ideal))))
	(t (error "Don't know how to compute ~S/~S" ring ideal))))

(defgeneric use-chinese-remainder (remainders)
  (:documentation
   "The purpose of this method is unknown."))

;; Use Chinese remainder theorem to compute the result given a list
;; of remainders which are GFp elements.
(defmethod use-chinese-remainder ((remainders list))
  (let* ((gfm (get-finite-field '()))
	 (x (car remainders))
	 (remainders (cdr remainders))
	 (p1 (if (typep (domain-of x) 'GFp)
		 (coerce (characteristic (domain-of x)) gfm)
		 (coerce (modulus x) gfm)))
	 p1inv p2)
    (setq x (coerce x gfm))
    (loop for k2 in remainders do
          (setq p1inv (recip (with-slots ((v value)) p1
                               (coerce (coerce v (domain-of k2)) gfm))))
          (setq p2 (if (typep (domain-of k2) 'GFp)
                       (coerce (characteristic (domain-of k2)) gfm)
                       (coerce (modulus k2) gfm)))
          (setq k2 (coerce k2 gfm))
          (setq x (make-element gfm
                                (value (+ x (* (* p1inv (- k2 x)) p1)))
                                (value (* p1 p2))))
          (setq p1 (* p1 p2)))
    x))

(defgeneric compute-result (result)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod compute-result ((result GFm-element))
  (with-slots ((v value) (modulus modulus)) result
    (if (> v (floor modulus 2)) (- v modulus) v)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Polynomial Domain Tools
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; poly-tools.lisp,v 1.4 1995/05/24 17:42:08 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.4")

;; Things conditionalized by GEHASH would require hash tables that
;; work with GE-EQUAL.

(defmethod initialize-instance :after
    ((domain variable-hash-table) &rest plist)
  (declare (ignore plist))
  (with-slots (variable-hash-table variable-table variables) domain
    #+GEHASH
    (setq variable-hash-table (make-hash-table :test #'equal))
    (setq variable-table (make-array (list (max (length variables) 1) 2)))
    (setq variable-hash-table 
	  (loop for var in variables
		for i upfrom 0
		collect (list var i)
		do (setf (aref variable-table i 0) var)))))

(defgeneric variable-index (domain variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod variable-index ((domain variable-hash-table) (variable symbol))
  (setq variable (coerce variable *general*))
  (loop for (var index) in (variable-hash-table domain)
	do (when (ge-equal variable var)
	     (return index)))
  #+GEHASH
  (gethash variable (variable-hash-table domain)))

(defmethod variable-index
    ((domain variable-hash-table) (variable general-expression))  
  (loop for (var index) in (variable-hash-table domain)
	do (when (ge-equal variable var)
	     (return index)))
  #+GEHASH
  (gethash variable (variable-hash-table domain)))

(defgeneric variable-symbol (domain order)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod variable-symbol ((domain variable-hash-table) (order-number number))
  (aref (variable-index-table domain) order-number 0))

;;(defmethod variable-symbol ((domain variable-hash-table) (poly polynomial))
;;  (aref (variable-index-table domain) (poly-order-number (poly-form poly)) 0))

(defgeneric get-variable-number-property (domain order property)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-variable-number-property
    ((domain variable-hash-table) order-number property)
  (%getf (aref (variable-index-table domain) order-number 1) property))

(defgeneric set-variable-number-property (domain order property value)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod set-variable-number-property
    ((domain variable-hash-table) order-number property value)
  (setf (%getf (aref (variable-index-table domain) order-number 1) property)
	value))

(defsetf get-variable-number-property set-variable-number-property)

(defmethod get-variable-property
    ((domain variable-hash-table) variable property)
  (setq variable (coerce variable *general*))
  (get-variable-number-property domain (variable-index domain variable)
				property))

(defmethod set-variable-property
    ((domain variable-hash-table) variable property value)  
  (setq variable (coerce variable *general*))
  (set-variable-number-property domain (variable-index domain variable)
				property value))

;; Defined in general, which is loaded first.
;;(defsetf get-variable-property set-variable-property)

(defgeneric add-new-variable (ring variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod add-new-variable ((ring variable-hash-table) variable)
  (with-slots (variables variable-hash-table variable-table) ring
    (let ((vars (different-kernels (coerce variable *general*)
				   variables)))
      (setq vars
	    (loop for var in vars
		  unless (member var variables :test #'ge-equal)
                  collect var))
      (unless (null vars)
	(let* ((count (length variables))
	       (array (make-array (list (cl:+ count (length vars)) 2))))
	  (setq variables (append variables vars))
	  (copy-array-contents variable-table array)
	  (setq variable-table array)
	  #-GEHASH
	  (setq variable-hash-table
		(nconc variable-hash-table
		       (loop for var in vars
			     for cnt upfrom count
			     do (setf (aref variable-table cnt 0) var)
			     collect (list var cnt))))
	  #+GEHASH ;; If we had General expression hash tables
	  (loop for var in vars
		for cnt upfrom count
		do (setf (gethash var variable-table) cnt)))))
    ring))

(defmethod zero ((domain caching-zero-and-one))
  (with-slots (zero) domain
    zero))

(defmethod one ((domain caching-zero-and-one))
  (with-slots (one) domain
    one))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      General Polynomial Domain
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; mpolynomial.lisp,v 1.10 1995/05/24 17:42:05 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.10")

(define-domain-element-classes multivariate-polynomial-ring
  mpolynomial epolynomial)

(defmethod initialize-instance :after ((d multivariate-polynomial-ring)
				       &rest plist)
  (declare (ignore plist))
  (with-slots (zero one coefficient-domain) d
    (setq zero (make-polynomial d (zero coefficient-domain)))
    (setq one (make-polynomial d (one coefficient-domain)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator polynomial-ring
      ((coefficient-domain domain) (variables list))
    (let ((domain (make-instance 'multivariate-polynomial-ring
                                 :variables (loop for var in variables
                                                  collect (coerce var *general*))
                                 :coefficient-domain coefficient-domain
                                 :print-function 'polynomial-ring-print-object)))
      (make-homomorphism coefficient-domain
                         #'(lambda (c) (make-polynomial domain c))
                         domain)
      domain)))

(defun polynomial-ring-print-object (d stream)
  (format stream "~A[" (coefficient-domain-of d))
  (display-list (ring-variables d) stream)
  (princ "]" stream))

(defgeneric get-polynomial-ring (domain variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod get-polynomial-ring
    ((coefficient-domain domain) (variables list))
  (let ((kernels ()))
    (loop for var in variables
	  do (loop for v in (different-kernels (coerce var *general*)
                                               kernels)
		   do (pushnew v kernels :test #'ge-equal)))
    (setq kernels (nreverse kernels))
    (add-domain #'(lambda (d)
		    (and (typep d 'polynomial-ring)
			 (eql (coefficient-domain-of d) coefficient-domain)
			 (ge-lequal (ring-variables d) kernels)))
      (make-polynomial-ring* coefficient-domain kernels))))

;;; ===========================================================================
;;;			       Polynomial Elements
;;; ===========================================================================

;;; Polynomials are a structure consisting of three parts: an order
;;; number, the variable at this level, and a list of the terms of the
;;; polynomial.  Term-list are exponent coefficient pairs.

;; Polynomials  := <coef> | (<var-number> . <term-list>)
;; term-list := nil | ((<exponent> . <coefficient>) . <term-list>)

(defmacro poly-order-number (poly)
  `(first ,poly))

(defmacro poly-terms (poly)
  `(rest ,poly))

(defmacro poly-coef? (x)
  `(not (listp ,x)))

(defgeneric scalar? (object)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod scalar? ((x mpolynomial))
  (poly-coef? (poly-form x)))

(defsubst poly-0? (x)
  (and (poly-coef? x) (0? x))) 

(defmethod 0? ((x mpolynomial))
  (poly-0? (poly-form x)))

(defsubst poly-1? (x)
  (and (poly-coef? x) (1? x)))

(defmethod 1? ((x mpolynomial))
  (poly-1? (poly-form x)))

(defun make-poly-form (poly terms)
  (cons (poly-order-number poly) terms))

(defgeneric make-polynomial (domain form)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod make-polynomial ((domain multivariate-polynomial-ring) form)
  (make-instance 'mpolynomial :domain domain :form form))

(defmethod variable-symbol ((domain variable-hash-table) (poly mpolynomial))
  (aref (variable-index-table domain) (poly-order-number (poly-form poly)) 0))

(defsubst more-main? (x y)
  (cl:< (poly-order-number x) (poly-order-number y)))

(defun more-main-order-number? (x y) (cl:< x y))

(defsubst same-variable? (x y)
  (cl:= (poly-order-number x) (poly-order-number y)))

;; The following macros are for dealing with term-lists.

(defmacro le (terms) `(first  (first ,terms)))

(defmacro lc (terms) `(rest (first ,terms)))

(defmacro lt (terms) `(first ,terms))

(defmacro red (terms) `(rest ,terms))

(defmacro make-terms (e c &optional (terms ()))
  (if (null terms)
      `(list (cons ,e ,c))	 
      `(cons (cons ,e ,c) ,terms)))

(defmacro make-many-terms (&rest e-c-pairs)
  (if (null e-c-pairs) `(terms0)
      `(make-terms ,(first e-c-pairs) ,(second e-c-pairs)
        (make-many-terms ,@(rest (rest e-c-pairs))))))

(defmacro terms0? (x) `(null ,x))

(defmacro terms0 () '())

(defgeneric make-poly (domain variable &optional terms)
  (:documentation
   "This is not part of the interface. It is mostly used for
testing."))

(defmethod make-poly
    ((domain multivariate-polynomial-ring) variable &optional terms)  
  (unless terms
    (setq terms (make-terms 1 (coerce 1 (coefficient-domain-of domain)))))
  (make-polynomial domain (cons (variable-index domain variable) terms)))

;; Some useful control abstractions for term lists

;; This is the version we should use when the walker is re-installed properly.
#+ignore 
(defmacro map-over-each-term (terms (e c) &body body)
  (let ((collects? nil) (updates? nil))
    (walker::walk-form `(progn ,@body) nil
		       #'(lambda (form context env)
			   (declare (ignore env))
			   (cond ((and (eql context :eval) (not (atom form)))
				  (cond ((eql (first form) 'collect-term)
					 (setq collects? t))
					((eql (first form) 'update-term)
					 (setq updates? t)))))
                           form))
    `(let ((.ans-terms. (list nil))
	   ,@(when collects? 
                   `((.terms. nil))))
      (macrolet (,@(when collects?
                         '((collect-term (.e. .c.)
                            `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                              (setf .terms. (rest .terms.))))))
                 ,@(when updates?
                         '((update-term (.e. .c.)
                            `(progn (setf (le .t.) , .e.)
                              (setf (lc .t.) , .c.))))))
        ,@(when collects?
                '((setq .terms. .ans-terms.)))
        (loop for .t. on ,terms
              for ((,(and (not (eql e 'ignore)) e)
                     . ,(and (not (eql c 'ignore)) c))) = .t.
              do ,@body))
      (rest .ans-terms.))))

(defmacro map-over-each-term (terms (e c) &body body)
  `(let ((.ans-terms. (list nil))
	 (.terms. nil))
    (macrolet ((collect-term (.e. .c.)
                 `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                   (setf .terms. (rest .terms.))))
               (update-term (.e. .c.)
                 `(progn (setf (le .t.) , .e.)
                   (setf (lc .t.) , .c.))))
      (setq .terms. .ans-terms.)
      (loop for .t. on ,terms
            for ((,(and (not (eql e 'ignore)) e)
                   . ,(and (not (eql c 'ignore)) c))) = .t.
            do ,@body))
    (rest .ans-terms.)))

(defmacro pair-up-terms (terms1 (e1 c1) terms2 (e2 c2) order-predicate &body body)
  (unless (atom order-predicate)
    (error "Invalid order predicate for PAIR-UP-TERMS: ~S" order-predicate))
  `(let ((.ans-terms. (list nil))
	 (.terms. nil)
	 (.t1. ,terms1)
	 (.t2. ,terms2)
	 ,e1 ,c1 ,e2 ,c2)
    (macrolet ((collect-term (.e. .c.)
                 `(progn (setf (rest .terms.) (make-terms , .e. , .c.))
                   (setf .terms. (rest .terms.))))		
               (update-term1 (.e. .c.)
                 `(progn (setf (le .t1.) .e.)
                   (setf (lc .t1.) .c.)))
               (update-term2 (.e. .c.)
                 `(progn (setf (le .t2.) .e.)
                   (setf (lc .t2.) .c.))))
      (setq .terms. .ans-terms.)
      (loop
       (cond ((terms0? .t1.)
              (cond ((terms0? .t2.)
                     (return (rest .ans-terms.)))
                    (t (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))
                       (setq ,e1 nil))))
             ((or (terms0? .t2.) (,order-predicate (le .t1.) (le .t2.)))
              (setq ,e1 (le .t1.) ,c1 (lc .t1.) .t1. (red .t1.))
              (setq ,e2 nil))
             ((,order-predicate (le .t2.) (le .t1.))
              (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))
              (setq ,e1 nil))
             (t
              (setq ,e1 (le .t1.) ,c1 (lc .t1.) .t1. (red .t1.))
              (setq ,e2 (le .t2.) ,c2 (lc .t2.) .t2. (red .t2.))))
       ,@body))))

;; Simple version for book
#+ignore
(defmacro accummulate-terms (terms accumulator element-fun &optional (identity (terms0)))
  `(let ((answer ,identity))
    (map-over-each-term terms (e c)
      (setq answer (,accumulator answer (,element-fun e c))))))

(defvar *empty-accumulation-slot* (list nil))

(defmacro accumulate-terms (terms (accumulator &optional (identity (terms0)))
			    (e c)
			    &body element-forms)
  `(let ((.accum-list. (list *empty-accumulation-slot*))
	 (.accum-fun. ,accumulator))
    (map-over-each-term ,terms (,e ,c)
      (insert-into-accumulation-list .accum-list. (progn ,@element-forms) .accum-fun.))
    (accumulate-accumulation-list .accum-list. .accum-fun. ,identity)))

(defun insert-into-accumulation-list (l element accumulator)
  (cond ((eq (car l) *empty-accumulation-slot*)
	 (setf (car l) element))
	(t (setq element (%funcall accumulator (car l) element))
	   (setf (car l) *empty-accumulation-slot*)
	   (when (null (cdr l))
	     (setf (cdr l) (list *empty-accumulation-slot*)))
	   (insert-into-accumulation-list (cdr l) element accumulator)))
  l)

(defun accumulate-accumulation-list (accum-list accumulator identity)
  (cond ((null accum-list) identity)
	((eq (car accum-list) *empty-accumulation-slot*)
	 (accumulate-accumulation-list (cdr accum-list) accumulator identity))
	(t (do ((sum (car accum-list))
		(l (cdr accum-list) (cdr l)))
	       ((null l) sum)
	     (unless (eq (car l) *empty-accumulation-slot*)
	       (setq sum (%funcall accumulator sum (car l))))))))

;;; ===========================================================================
;;;				     EXPONENT ARITHMETIC
;;; ===========================================================================

(defmacro e= (x y) `(cl:= ,x ,y))

(defmacro e> (x y) `(cl:> ,x ,y))

(defmacro e< (x y) `(cl:< ,x ,y))

(defmacro e0 () 0)

(defmacro e0? (x)  `(cl:= (e0) ,x))

(defmacro e1 () 1)

(defmacro e1? (x)  `(cl:= (e1) ,x))

(defmacro e+ (x y) `(cl:+ ,x ,y))

(defmacro e1+ (x) `(cl:1+ ,x))

(defmacro e1- (x) `(cl:1- ,x))

(defmacro e- (x y) `(cl:- ,x ,y))

(defmacro e* (x y) `(cl:* ,x ,y))

(defmacro e/ (x y) `(cl:/ ,x ,y))

(defmacro eminus? (x) `(cl:minusp ,x))

(defmacro eoddp (x) `(cl:oddp ,x))

(defmacro eminus (x) `(cl:- ,x))

(defmacro emax (x y) `(cl:max ,x ,y))

(defgeneric make-polynomial-morphism (domain range &rest pairs)
  (:documentation
   "The purpose of this method is unknown."))

;; The pairs are variables in the domain and their values in the range
(defmethod make-polynomial-morphism
    ((domain polynomial-ring) (range polynomial-ring) &rest pairs)
  (let ((array (make-array (length (ring-variables domain))))
	(range-coefficient-domain (coefficient-domain-of range)))
    (loop for (v value) in pairs
	  do (setf (aref array (variable-index domain v))
		   (poly-form value)))
    (%funcall (if (eql domain range) #'make-automorphism #'make-homomorphism)
              domain
              #'(lambda (poly)
                  (bind-domain-context range ;; Because all computations are in the range
                    (labels ((transform (form)
                               (cond ((poly-coef? form)
                                      (coerce form range-coefficient-domain))
                                     (t (let* ((terms (poly-terms form))
                                               (old-e (le terms))
                                               (ans (transform (lc terms)))
                                               (value (aref array (poly-order-number form))))
                                          (map-over-each-term (red terms) (e c)
                                            (setq ans (poly-plus
                                                       (poly-times (poly-expt value (e- old-e e))
                                                                   ans)
                                                       (transform c)))
                                            (setq old-e e))
                                          (poly-times ans (poly-expt value old-e)))))))
                      (make-polynomial range (transform (poly-form poly))))))
              range)))

(defmethod make-polynomial-morphism
    ((domain free-module) (range free-module) &rest pairs)
  (unless (and (typep (coefficient-domain-of domain) 'polynomial-ring)
	       (typep (coefficient-domain-of range) 'polynomial-ring))
    (error "Don't know how to create a polynomial map from ~S to ~S"
	   domain range))
  (let ((morphism (%apply #'make-polynomial-morphism
                          (coefficient-domain-of domain)
                          (coefficient-domain-of range)
                          pairs)))
    (%funcall (if (eql domain range) #'make-automorphism #'make-homomorphism)
              domain
              #'(lambda (vector)
                  (%apply #'make-element
                          range
                          (loop with vect = (tuple-value vector)
                                for i below (array-dimension vect 0)
                                collect (apply-morphism morphism (aref vect i)))))
              range)))

(defun poly-monomial? (poly)
  (cond ((poly-coef? poly) t)
	((terms0? (red (poly-terms poly)))
	 (poly-monomial? (lc (poly-terms poly))))
	(t nil)))

(defgeneric print-mpolynomial-form (domain p stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod print-mpolynomial-form ((domain multivariate-polynomial-ring)
                                   p stream)
  (let* ((coef-domain (coefficient-domain-of domain))
	 (simple-coefs?
	  (and (typep coef-domain 'numeric-domain)
	       (not (typep coef-domain 'complex-numbers))))
	 (complex-num-coefs? (typep coef-domain 'complex-numbers)))
    (labels
      ((print-form (form)
         (if (poly-coef? form)
           (princ form stream)
           (let ((terms (poly-terms form))
                 (variable (variable-symbol
                            domain (poly-order-number form))))
             (let ((c (lc terms)))
               (when (and (poly-coef? c) 
                          (= c (- (one domain))))
                 (princ "- " stream)
                 (setq c (- c)))
               (print-term variable (le terms) c))
             (map-over-each-term (red terms) (e c)
               (if (and (poly-coef? c)
                        (number? c) (real? c) (minus? c))
                 (progn (princ " - " stream) (setq c (- c)))
                 (princ " + " stream))
               (print-term variable e c)))))
       (print-term (v e c)
         (unless (and (poly-1? c) (not (e0? e)))
           (cond ((or (and simple-coefs? (poly-monomial? c))
		      (and complex-num-coefs? (poly-coef? c)
			   (or (0? (realpart c)) (0? (imagpart c)))))
                  (print-form c))
                 (t (princ "(" stream)
                    (print-form c)
                    (princ ")" stream))))
         (unless (e0? e)
           (and (not (poly-1? c))
                (princ " " stream))
           (display v stream)
           (if (not (e1? e))
             #+Genera
             (format stream "~S" e)
             #-Genera
             (format stream "^~S" e)))))
      (print-form p))))

(defmethod print-object ((p mpolynomial) stream)
  (print-mpolynomial-form (domain-of p) (poly-form p) stream))

;;; ===========================================================================
;;;				    POLYNOMIAL ARITHMETIC
;;; ===========================================================================

;; Coercions 

(defmethod coerce (elt (domain multivariate-polynomial-ring))
  (let ((value (coercible? elt (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-polynomial domain value))
	  (t (call-next-method)))))

(defmethod coerce ((exp symbol) (domain multivariate-polynomial-ring))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((exp list) (domain multivariate-polynomial-ring))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((p mpolynomial) (d general-expressions))
  (let ((domain (domain-of p)))
    (labels ((transform (form)
               (if (poly-coef? form)
                   (coerce form d)
                   (let ((terms (poly-terms form))
                         (variable (variable-symbol
                                    domain (poly-order-number form)))
                         (sum ()))
                     (map-over-each-term terms (e c)
                       (push (cond ((e0? e) (transform c))
                                   ((poly-1? c)
                                    (if (e1? e) variable
                                        (make-ge-expt d variable e)))
                                   ((e1? e)
                                    (make-ge-times d
                                                   (list (transform c) variable)))
                                   (t (make-ge-times d
                                                     (list (transform c)
                                                           (make-ge-expt d variable e)))))
                             sum))
                     (if (null (rest sum))
                         (first sum)
                         (make-ge-plus d (nreverse sum)))))))
      (transform (poly-form p)))))

(defmethod coerce ((exp general-expression)
		   (domain multivariate-polynomial-ring)) 
  (with-slots (variables) domain 
    (cond ((member exp variables :test #'ge-equal)
	   (make-polynomial domain
                            (cons (variable-index domain exp)
                                  (make-terms 1 (one (coefficient-domain-of domain))))))
	  ((and (ge-atom? exp)
		(let ((var (coercible? exp (coefficient-domain-of domain))))
		  (and var (make-polynomial domain var)))))
	  ((ge-plus? exp)
	   (let ((sum (zero domain)))
	     (loop for x in (terms-of exp)
		   do (setq sum (+ sum (coerce x domain))))
	     sum))
	  ((ge-times? exp)
	   (let ((prod (one domain)))
	     (loop for x in (terms-of exp)
		   do (setq prod (* prod (coerce x domain))))
	     prod))
	  ((and (ge-expt? exp)
		(integer? (exponent-of exp))
		(plus? (exponent-of exp)))
	   (expt (coerce (base-of exp) domain) (exponent-of exp)))
	  (t (coerce exp (coefficient-domain-of domain))))))

(defun poly-simp (variable x)
  (cond ((terms0? x)
	 (zero *coefficient-domain*))
	((atom x)
	 (error "An atom, ~A, was passed to poly-simp, this is a bug" X))
	((and (e0? (le x))              ;just a coefficient
	      (terms0? (red x)))        ;and no additional terms
	 (lc x))
	(t (make-poly-form variable x))))

(defun terms-term (terms n)
  (do ((terms terms (red terms)))
      ((terms0? terms) (zero *coefficient-domain*))
    (cond ((e= (le terms) n) (return (lc terms)))
          ((e< (le terms) n)
           (return (zero *coefficient-domain*))))))

;;; The term lists are assumed to come from legitimate polynomials, so
;;; none of their coefficients can be zero.  Consequently, make-terms
;;; can be used in some places.

(defun terms-plus (x y)			;x and y are term lists
  (pair-up-terms x (e1 c1) y (e2 c2) e>
    (if e1 (if e2 (let ((c-sum (poly-plus c1 c2)))
		    (if (not (poly-0? c-sum))
			(collect-term e1 c-sum)))
	       (collect-term e1 c1))
	(collect-term e2 c2))))

(defun poly-plus (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (+ x y)
	     (if (poly-0? x) y
		 (poly-simp y (terms-plus (make-terms (e0) x)
					  (poly-terms y))))))
	((poly-coef? y)
	 (if (poly-0? y) x
	     (poly-simp x (terms-plus (make-terms (e0) y) (poly-terms x)))))
	((same-variable? x y)
	 (poly-simp x (terms-plus (poly-terms y) (poly-terms x))))
	((more-main? x y)
	 (poly-simp x (terms-plus (make-terms (e0) y) (poly-terms x))))
	(t (poly-simp y (terms-plus (make-terms (e0) x) (poly-terms y))))))

(defmethod-sd plus ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-plus (poly-form x) (poly-form y)))))

(defun poly-minus (p)
  (cond ((poly-coef? p)
	 (minus p))
	(t (make-poly-form p (terms-minus (poly-terms p))))))

(defun terms-minus (x)
  (map-over-each-term x (e c)
    (collect-term e (poly-minus c))))

(defmethod minus ((x mpolynomial))
  (let ((domain (domain-of x)))
    (bind-domain-context (domain-of x)
      (make-polynomial domain (poly-minus (poly-form x))))))

(defun poly-minus? (p)
  (if (poly-coef? p) (minus? p)
      (terms-minus? (poly-terms p))))

(defun terms-minus? (terms)
  (poly-minus? (lc terms)))

(defmethod minus? ((x mpolynomial))
  (bind-domain-context (domain-of x)
    (poly-minus? (poly-form x))))

(defun terms-difference (x y)			;x and y are term lists
  (pair-up-terms x (e1 c1) y (e2 c2) e>
    (if e1 (if e2 (let ((c-sum (poly-difference c1 c2)))
		    (if (not (poly-0? c-sum))
			(collect-term e1 c-sum)))
	       (collect-term e1 c1))
	(collect-term e2 (poly-minus c2)))))

(defun poly-difference (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (- x y)
	     (if (poly-0? x) (poly-minus y)
		 (poly-simp y (terms-difference (make-terms (e0) x)
						(poly-terms y))))))
	((poly-coef? y)
	 (if (poly-0? y) x
	     (poly-simp x (terms-difference (poly-terms x)
					    (make-terms (e0) y)))))
	((same-variable? x y)
	 (poly-simp x (terms-difference (poly-terms x) (poly-terms y))))
	((more-main? x y)
	 (poly-simp x (terms-difference (poly-terms x) (make-terms (e0) y))))
	(t (poly-simp y (terms-difference (make-terms (e0) x)
					  (poly-terms y))))))

(defmethod-sd difference ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-difference (poly-form x) (poly-form y)))))

(defun terms-mon-times (poly-terms e c)
  (if (poly-0? c) (terms0)
      (map-over-each-term poly-terms (te tc)
	(collect-term (e+ e te) (poly-times tc c)))))

#+AMBook
(defun terms-times (x y)
  (accumulate-terms y (#'terms-plus) (e c) (terms-mon-times x e c)))

(defun terms-times (x y)
  (let ( ;; Multiply x by the first term of y.  This is the initial
	;; term list we will modify.
	(answer (terms-mon-times x (le y) (lc y))) 
	e c)
    (setq answer (cons nil answer))
    (loop for (e-y . c-y) in (red y)
	  for ans = answer do
          (loop for (e-x . c-x) in x do
                (unless (poly-0? (setq c (poly-times c-x c-y)))
                  (setq e (e+ e-x e-y))
                  ;; Find place to insert this term.
                  (loop			
                   ;; Sure would be nice if the complier recognized and optimized
                   ;; the usages of (red ans)
                   (cond ((or (terms0? (red ans)) (e> e (le (red ans))))
                          (setf (red ans) (make-terms e c (red ans)))
                          (return t))	
                         ((e= e (le (red ans)))
                          (setf (lc (red ans)) (poly-plus (lc (red ans)) c))
                          (return t))
                         (t  (setq ans (red ans))))))))
    (loop for ans on answer
	  do (when (poly-0? (lc (red ans)))
	       (setf (red ans) (red (red ans)))))
    (red answer)))			     

(defun poly-times (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (* x y)
	     (poly-simp y (terms-mon-times (poly-terms y) (e0) x))))
	((poly-coef? y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	((same-variable? x y)
	 (poly-simp x (terms-times (poly-terms x) (poly-terms y))))
	((more-main? x y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	(t (poly-simp y (terms-mon-times (poly-terms y) (e0) x)))))

(defmethod-sd times ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-times (poly-form x) (poly-form y))))) 

;;; In both POLY-EXPTSQ and POLY-EXPT the second argument is a
;;; repetition count, and is thus is actually an integer.

(defun poly-exptsq (p n)
  (%funcall (repeated-squaring #'poly-times (one *coefficient-domain*))
            p n))

(defun poly-expt (p n)
  (cond ((e0? n) (one *coefficient-domain*))
	((e1? n) p)
	((poly-coef? p) (expt p n))
	((poly-monomial? p)	 
	 (let ((c (poly-expt (lc (poly-terms p)) n)))
	   (if (poly-0? c) c
	       (poly-simp p (make-terms (e* n (le (poly-terms p))) c)))))
	(t (let ((bl (do ((b (poly-simp p (red (poly-terms p))))
			  (bl (list (poly-simp p (red (poly-terms p))))
			      (cons (poly-times b (car bl)) bl))
			  (m 1 (1+ m)))
			 ((cl:= m n) bl)))
		 (monomial
                  (make-poly-form p (make-terms (le (poly-terms p))
                                                (lc (poly-terms p))))))
	     (do ((x^m monomial (poly-times x^m monomial))
		  (u (cdr bl) (cdr u))
		  (m 1 (1+ m))
		  (nom n (cl:/ (cl:* nom (cl:- n m)) (1+ m)))
		  (answer (car bl)
			  (poly-plus
			   (poly-times
			    (poly-times (coerce nom *coefficient-domain*) x^m)
			    (car u))
			   answer)))
		 ((null u)
		  (poly-plus x^m answer)))))))

(defmethod expt ((base mpolynomial) (expt integer))
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain (poly-expt (poly-form base) expt)))))

(defmethod expt ((base mpolynomial) (expt rational-integer))
  ;; Ignoring the domain of expt!!  FIXTHIS
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain (poly-expt (poly-form base)
					 (integer-value expt))))))

(defun terms-quotient (u v)
  (do ((coef)
       (exp)
       (quotient (terms0)))
      ((e< (le u) (le v))       
       (throw 'quotient-error
	 "~S is not exactly divisible by ~S"))
    (setq coef (poly-quotient* (lc u) (lc v)))
    (setq exp (e- (le u) (le v)))
    (setq u (terms-difference u (terms-mon-times v exp coef)))
    (setq quotient (terms-plus (make-terms exp coef) quotient))
    (if (terms0? u)
	(return quotient))))

(defun poly-quotient* (x y)
  (cond ((poly-0? y)
	 (throw 'quotient-error  "~S was divided by zero"))
	((poly-0? x) x)
	((poly-coef? x)
	 (if (poly-coef? y)
	     (cond ((typep *coefficient-domain* 'field)
		    (/ x y))
		   (t (multiple-value-bind (q r) (truncate x y)
			(if (0? r) q
			    (throw 'quotient-error
			      "Inexact division of ~S by ~S")))))
	     (throw 'quotient-error
	       "~S was divided by a polynomial of higher degree ~S")))
	((or (poly-coef? y)
	     (more-main? x y))
	 (make-poly-form x (terms-cquotient (poly-terms x) y)))
	((more-main? y x)
	 (throw 'quotient-error
	   "~S was divided by a polynomial of more main variable: ~S"))
	(t (poly-simp x (terms-quotient (poly-terms x) (poly-terms y))))))

(defun poly-quotient (x y)
  (let ((ans (catch 'quotient-error
	       (poly-quotient* x y))))
    (cond ((stringp ans)
	   (error ans x y))
	  (t ans))))

(defun terms-cquotient (terms c)
  (map-over-each-term terms (te tc)
    (collect-term te (poly-quotient tc c))))

(defmethod-sd quotient ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-quotient (poly-form x) (poly-form y)))))

(defun poly-test-quotient (x y)
  (let ((ans (catch 'quotient-error
	       (poly-quotient* x y))))
    (if (stringp ans) nil
	ans)))

(defun terms-pseudo-remainder (u v)
  (if (e< (le u) (le v)) u
      (do ((k (e- (le u) (le v)) m)
	   (m))
	  (nil)
	(setq u (terms-difference (terms-mon-times u (e0) (lc v))
				  (terms-mon-times v k (lc u))))
	(cond ((terms0? u) (return u))
	      ((eminus? (setq m (e- (le u) (le v))))
	       (return (if (e0? k) u
			   (terms-mon-times u (e0) (poly-expt (lc v) k))))))
	(if (e> (e- k 1) m)
	    (setq u (terms-mon-times u
				     (e0)
				     (poly-expt (lc v) (e- (e- k 1) m))))))))

(defun poly-pseudo-remainder (p q)
  (cond ((poly-coef? p)
	 (cond ((poly-coef? q) (remainder p q))
	       (t p)))
	((poly-coef? q)
	 (with-slots (coefficient-domain) p
	   (zero coefficient-domain)))
	((same-variable? p q)
	 (poly-simp p (terms-pseudo-remainder (poly-terms p)
					      (poly-terms q)))) 
	((more-main? p q)
	 (poly-simp p (terms-coef-remainder (poly-terms p) q)))
	(t p)))

(defun terms-coef-remainder (u q)
  (map-over-each-term u (e c) 
    (collect-term e (poly-pseudo-remainder c q))))

(defmethod-sd remainder ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain
      (poly-pseudo-remainder (poly-form x) (poly-form y)))))

(defun poly-truncate2 (u v)
  (cond ((poly-coef? v)
	 (cond ((typep *coefficient-domain* 'field)
		(values (poly-times u (/ v))
			(zero *coefficient-domain*)))
	       (t (error "Not implemented yet"))))
	(t (error "not implemented yet"))))

(defmethod-sd truncate2 ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (multiple-value-bind (quo rem) (poly-truncate2 (poly-form x) (poly-form y))
      (values (make-polynomial domain quo)
	      (make-polynomial domain rem)))))

(defun poly-height (x)
  (if (poly-coef? x) (height x)
      (let (h)
        (map-over-each-term (poly-terms x) (e  c)
          (setq h (if h (max h (poly-height c))
                      (poly-height c))))
        h)))

(defmethod height ((x mpolynomial))
  (poly-height (poly-form x)))

;; The following routine gives a bound on the absolute value of largest 
;; coefficient that can occur in a factor involving the main variable (using
;; Gelfond's bound).
(defun poly-factor-cbound (p)
  (if (poly-coef? p) (height p)
      (* (expt 2 (le (poly-terms p))) (sqrt (le (poly-terms p)))
         (poly-height p))))

(defvar poly-gcd-algorithm 'poly-subresultant-gcd
  "Algorithm to be used to compute the GCD of two polynomials with the
   same main variable")

(defun terms-content (p)
  (do ((gcd (lc p) (poly-gcd gcd (lc pp)))
       (pp (red p) (red pp)))
      ((terms0? pp)
       (if (poly-minus? (lc p))
	   (poly-minus gcd)
	   gcd))))

(defun poly-content (p)
  (cond ((poly-coef? p) p)
	(t (terms-content (poly-terms p)))))

(defun poly-gcd (p q)
  (cond ((poly-1? p) p)
	((poly-1? q) q)
	((poly-coef? p)
	 (cond ((poly-coef? q) (gcd p q))
	       (t (poly-gcd (poly-content q) p))))
	((poly-coef? q)
	 (poly-gcd (poly-content p) q))
	((more-main? p q)
	 (poly-gcd (poly-content p) q))
	((more-main? q p)
	 (poly-gcd (poly-content q) p))
	(t (if (e< (le (poly-terms p)) (le (poly-terms q)))
	       (rotatef p q))
	   (let ((pc (poly-content p))
		 (qc (poly-content q)))
	     (poly-times (poly-gcd pc qc)
			 (%funcall poly-gcd-algorithm
				       (poly-quotient p pc)
				       (poly-quotient q qc)))))))

(defmethod-sd binary-gcd ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (make-polynomial domain (poly-gcd (poly-form x) (poly-form y))))) 

(defun poly-lcm (p q)
  (poly-times (poly-quotient p (poly-gcd p q)) q))

(defun poly-split-on-vars 
    (poly vars &optional (pvars (reverse (poly-list-of-variables poly))))
  (let ((polys (list poly)))
    (unless (or (poly-coef? poly) (subsetp pvars vars)) 
      (loop for bad-v in (loop for v in pvars
                               when (not (member v vars))
                               collect v)
            for var = (list bad-v)
            for new-polys = nil
            do (loop for p in polys do 
                     (loop for deg below (1+ (poly-degree p var))
                           for coef = (poly-coefficient p var deg)
                           do (unless (0? coef)
                                (push coef new-polys))))
            (setq polys new-polys)))
    polys))

;; The following routine takes a list of polynomials and returns a list of
;; polynomials that all involve the same variables.  When computing the GCD
;; of the original polynomials, you only need to compute the GCD of the 
;; polynomials in returned list.
(defun poly-find-common-vars (polys)
  (let (pvars vars all-polys)
    (loop for same-vars = t 
          do 
          (setq pvars (loop for p in polys
                            collect (reverse (poly-list-of-variables p))))
          (setq vars (loop for vs in (rest pvars)
                           with ans = (first pvars)
                           do (unless (equal ans vs)
                                (setq same-vars nil)
                                (setq ans (intersection ans vs)))
                           finally (return ans)))
          (when same-vars 
            (return t))
          (setq all-polys nil)
          (loop for p in polys
                for vs in pvars
                do (setq all-polys (nconc all-polys 
                                          (poly-split-on-vars p vars vs))))
          (setq polys all-polys))
    polys))

(defun poly-mgcd (polys)
  (let ((d *coefficient-domain*)
        gcd odd-poly even-poly new-polys)
    (loop 
     (setq odd-poly (first polys))
     (setq even-poly (second polys))
     (loop for (op ep) on (poly-find-common-vars polys) by #'cddr
           do (setq odd-poly (poly-plus odd-poly (poly-times (random-constant d)
                                                             op)))
           (when ep
             (setq even-poly (poly-plus even-poly (poly-times (random-constant d)
                                                              ep)))))
     (print "Begin GCD")
     (setq gcd (spmod-gcd1 odd-poly even-poly))
     (print "Begin Test divides")
     (setq new-polys nil)
     (loop for p in polys
           do (unless (poly-test-quotient p gcd)
                (push p new-polys)))
     (print new-polys)
     (if (null new-polys)
         (return gcd)
         (push gcd new-polys)))))

;;; The following should ultimately be optimized to use SpGCD.
(defun poly-content-and-prim-part (p)
  (let ((content (poly-content p)))
    (values content (poly-quotient p content))))

(defun terms-prim-part (p)
  (if (terms0? p) p
      (terms-cquotient p (terms-content p))))

(defun poly-prim-part (p)
  (make-poly-form p (terms-prim-part (poly-terms p))))

(defun terms-monicize (terms)
  (let ((inv (recip (lc terms))))
    (map-over-each-term terms (e c)
      (collect-term e (* c inv)))))

(defun poly-monicize (p)
  (make-poly-form p (terms-monicize (poly-terms p))))

(defun terms-euclidean-gcd (u v)
  (do ((u u v)
       (v v (terms-pseudo-remainder u v)))
      ((terms0? v)
       (terms-prim-part u))
    ;; (print v)
    (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))))

(defun poly-euclidean-gcd (p q)
  (poly-simp p (terms-euclidean-gcd (poly-terms p) (poly-terms q))))

(defun terms-primitive-gcd (u v)
  (do ((u u v)
       (v v (terms-prim-part (terms-pseudo-remainder u v))))
      ((terms0? v)
       u)
    ;; (print v)
    (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))))

(defun poly-primitive-gcd (p q)
  (poly-simp p (terms-primitive-gcd (poly-terms p) (poly-terms q))))

(defun poly-reduced-gcd (p q)
  (poly-simp p (terms-reduced-gcd (poly-terms p) (poly-terms q))))

(defun terms-reduced-gcd (u v) 
  (let ((delta (e- (le u) (le v))))
    (do ((u u v)
	 (v v (terms-cquotient (terms-pseudo-remainder u v) beta))
	 (beta 1 (poly-expt (lc v) (e1+ delta))))
      ((terms0? v) (terms-prim-part u))
      ;;(print v)
      (if (e0? (le v)) (return (make-terms (e0) (one *coefficient-domain*))))
      (setq delta (e- (le u) (le v))))))

(defun poly-subresultant-gcd (p q)
  (let ((result (terms-subresultant-gcd (poly-terms p) (poly-terms q))))
    (if (e0? (le result))
	(one *coefficient-domain*)
	(poly-simp p result))))

(defgeneric resultant (polynomial polynomial~ variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod resultant ((x mpolynomial) (y mpolynomial) var)
  (let ((domain (domain-of x)) 
        pvar one into-varlist out-varlist)
    (unless (eql (domain-of y) domain)
      (error "Incompatible domains for resultant"))
    (setq one (one (coefficient-domain-of domain)))
    (setq pvar (poly-form (coerce var domain)))
    (setq into-varlist (list (list (poly-order-number pvar)
                                   (make-poly-form (list '-1) 
                                                   (make-terms (e1) one))))
          out-varlist (list (list -1 (make-poly-form pvar
                                                     (make-terms (e1) one)))))
    (bind-domain-context domain
      (make-polynomial domain 
                       (poly-subst 
                        (poly-resultant (poly-subst (poly-form x) into-varlist)
                                        (poly-subst (poly-form y) into-varlist))
                        out-varlist)))))

(defun poly-resultant (p q)
  (let ((result (terms-subresultant-gcd (poly-terms p) (poly-terms q))))
    (if (e0? (le result))
	(lc result)
	(zero *coefficient-domain*))))

(defun terms-subresultant-gcd (u v) 
  (let ((delta (e- (le u) (le v)))
	beta)
    (do ((u u v)
	 (v v (terms-cquotient (terms-pseudo-remainder u v) beta))
	 (first-time t nil)
	 (h ;; (poly-expt (lc v) (e- (le u) (le v)))
          ;; (if (e1? delta) (lc v)
          ;; 	(poly-quotient (poly-times h (poly-expt (lc v) delta))
          ;;		       (poly-expt h delta)))
          ))
	((terms0? v)
	 (terms-prim-part u))
      (if (e0? (le v))
	  (return v))
      (setq delta (e- (le u) (le v)))
      (setq beta (if first-time
		     (if (eoddp delta) 1 -1)
		     (if (eoddp delta) 
			 (poly-times (lc u) (poly-expt h delta))
			 (poly-times (poly-minus (lc u))
				     (poly-expt h delta)))))
      (if first-time
	  (setq h (poly-expt (lc v) (e- (le u) (le v))))
	  (setq h 
                (if (e1? delta) (lc v)
                    (poly-quotient (poly-times h (poly-expt (lc v) delta))
                                   (poly-expt h delta)))))
      ;;      (format t "~& = ~S,  = ~S, V = ~S, h = ~S" beta delta v h)
      )))

(defun poly-coerce (poly domain)
  (cond ((poly-coef? poly)
	 (coerce poly domain))
	(t (poly-simp poly (terms-coerce (poly-terms poly) domain)))))

(defun terms-coerce (terms domain)
  (map-over-each-term terms (e c)
    (collect-term e (poly-coerce c domain))))

(defun poly-subst (poly var-value)
  (let ((temp nil))
    (cond ((null var-value)
	   poly)
	  ((poly-coef? poly) (coerce poly *coefficient-domain*))
	  ((setq temp (second (assoc (poly-order-number poly) var-value
                                     :test #'eql)))	   
           (terms-horners-rule (poly-terms poly) temp var-value))
          (t (terms-horners-rule 
              (poly-terms poly) 
              (poly-simp poly (make-terms (e1) (one *coefficient-domain*)))
              var-value)
             ;; This assumed that more main variables were not substituted for
             #+ignore
             (poly-simp poly (terms-subst (poly-terms poly) var-value))
             ))))

(defun terms-subst (terms var-val-pairs)
  (let (temp)
    (map-over-each-term terms (e c)
      (unless (poly-0? (setq temp (poly-subst c var-val-pairs)))
	(collect-term e temp)))))

(defun terms-horners-rule (terms value &optional var-value)
  (let ((old-e (le terms))
	(ans (poly-subst (lc terms) var-value)))
    (map-over-each-term (red terms) (e c) 
      (setq ans (poly-plus (poly-times (poly-expt value (e- old-e e)) ans)
			   (poly-subst c var-value)))
      (setq old-e e))
    (poly-times ans (poly-expt value old-e))))

(defmethod substitute (value variable (p mpolynomial) &rest ignore)
  (declare (ignore ignore))
  (substitute (list (coerce value (domain-of p)))
	      (list (coerce variable (domain-of p)))
	      p))

(defmethod substitute 
    ((value mpolynomial) (variable mpolynomial) (p mpolynomial) &rest
     ignore)
  (declare (ignore ignore))
  (substitute (list value) (list variable) p))

(defmethod substitute ((values list) (variables list) (p mpolynomial)
		       &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of p))
	(new-domain (domain-of (first values))))
    (loop for var in variables
	  unless (eql (domain-of var) domain)
          do (error "Domain of ~S was expected to be ~S" var domain))
    (loop for val in values
	  unless (eql (domain-of val) new-domain)
          do (error "Domain of ~S was expected to be ~S" val new-domain))
    (loop for var in (ring-variables domain)
	  do (unless (find var variables 
			   :test #'(lambda (a b) 
				     (ge-equal a (variable-symbol domain b)))) 
	       (push (coerce var domain) variables)
	       (push (if (coercible? var new-domain)
			 (coerce var new-domain)
			 nil)
		     values)))
    (bind-domain-context new-domain
      (make-polynomial new-domain
                       (poly-subst (poly-form p)
                                   (loop for var in variables
                                         for val in values
                                         collect (list (variable-index domain var)
                                                       (and val (poly-form val)))))))))

(defun poly-variable-list (p &optional (varlist ()))
    (cond ((poly-coef? p) varlist)
	  (t (when (not (member (poly-order-number p) varlist))
	       (push (poly-order-number p) varlist))
	     (do ((terms (poly-terms p) (red terms)))
		 ((terms0? terms) varlist)
	       (setq varlist (poly-variable-list (lc terms) varlist))))))

(defgeneric partial-deriv (polynomial variable)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod partial-deriv ((p mpolynomial) x)
  (error "Don't know how to compute the partial deriv with respect to ~S"
	 x))

(defmethod partial-deriv ((p mpolynomial) (x symbol))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p mpolynomial) (x general-expression))
  (let ((domain (domain-of p)))
    (with-slots (variables) domain
      (if (member x variables :test #'ge-equal)
	  (partial-deriv p (coerce x domain))
	  (call-next-method)))))

(defmethod partial-deriv ((p mpolynomial) (x mpolynomial))
  (let ((domain (domain-of p))
	terms)
    (cond ((and (eql domain (domain-of x))
		(null (red (setq terms (poly-terms (poly-form x)))))
		(e1? (le terms))
		(poly-1? (lc terms)))
	   (bind-domain-context domain
	     (make-polynomial domain
			      (poly-derivative (poly-form p) (poly-form x)))))
	  (t (error "~S is not a variable in ~S" x domain)))))

;;; var in the following is expected to be a polynomial of degree one
;;; with coefficient 1.

(defun poly-derivative (p var)
  (cond ((poly-coef? p) (zero *coefficient-domain*))
	((same-variable? var p)
	 (poly-simp p (terms-derivative (poly-terms p))))
	((more-main? var p)
	 (zero *coefficient-domain*))
	(t (poly-simp p (let (dc)
			  (map-over-each-term (poly-terms p) (e c)
			    (if (not (poly-0?
				      (setq dc (poly-derivative c var))))
				(collect-term e dc))))))))

(defun terms-derivative (x)
  (map-over-each-term x (e c)
    (if (not (e0? e))
	(collect-term (e1- e)
		      (poly-times (coerce e *coefficient-domain*) c)))))

(defmethod deriv ((poly mpolynomial) &rest vars)
  (let* ((domain (domain-of poly))
	 deriv diff)
    (bind-domain-context domain
      (loop for var in vars do
            (setq var (coerce var *general*))
            (setq deriv (zero domain))
            (loop with variables = (list-of-variables poly)
                  for kernel in variables do
                  (when (depends-on? kernel var)
                    (setq diff (deriv kernel var))
                    (loop for new in (different-kernels diff variables) do
                          (add-new-variable domain new))
                    (setq deriv
                          (+ deriv (* (partial-deriv poly kernel)
                                      (coerce diff domain))))))
            (setq poly deriv)))
    poly))		   

(defun poly-max-coefficient (p)
  (unless (poly-coef? p)
    (terms-max-coefficient (poly-terms p))))

(defun terms-max-coefficient (terms &optional (max 0))
  (map-over-each-term terms (ignore c)
    (setq max (if (poly-coef? c) (max max (abs c))
		  (terms-max-coefficient (poly-terms c) max))))
  max)

(defgeneric degree (polynomial variable &rest rest)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod degree ((p mpolynomial) (var symbol) &rest ignore)
  (declare (ignore ignore))
  (degree p (coerce var (domain-of p))))

(defmethod degree ((p mpolynomial) (var ge-variable) &rest ignore)
  (declare (ignore ignore))
  (degree p (coerce var (domain-of p))))

(defmethod degree ((p mpolynomial) (x mpolynomial) &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of p))
	terms)
    (cond ((and (eql domain (domain-of x))
		(null (red (setq terms (poly-terms (poly-form x)))))
		(e1? (le terms))
		(poly-1? (lc terms)))
	   (bind-domain-context domain
	     (poly-degree (poly-form p) (poly-form x))))
	  (t (error "~S is not a variable in ~S" x domain)))))

(defun poly-degree (p var)
  (cond ((poly-coef? p) (e0))
	((same-variable? var p)
	 (le (poly-terms p)))
	((more-main? var p) (e0))
	(t (do ((l (poly-terms p) (red l))
		(e (e0)))
	       ((terms0? l) e)
	     (setq e (emax e (poly-degree (lc l) var)))))))

;;; The following routine returns a list of pairs (var . degree).
;;; There should probably be a structure defined called variable
;;; information which is actually returned.
(defun poly-degree-vector (p)
  (unless (poly-coef? p) 
    (let ((pdv (list (cons (poly-order-number p) (le (poly-terms p))))))
      (terms-degree-vector (poly-terms p) pdv)
      (sort pdv #'(lambda (x y)
		    (more-main-order-number? (first x) (first y)))))))

(defun add-variable-degree (pdv var deg)
  (loop for var-info in pdv
	when (eql (car var-info) var)
        return (if (cl:> deg (cdr var-info))
                   (setf (cdr var-info) deg))
	when (more-main-order-number? var (car var-info))
        return nil
	finally (setq pdv (nconc pdv (list (cons var deg)))))
  pdv)

(defun terms-degree-vector (terms pdv)
  (map-over-each-term terms (ignore c)
    (unless (poly-coef? c)
      (setq pdv (add-variable-degree pdv
				     (poly-order-number c)
				     (le (poly-terms c))))
      (terms-degree-vector (poly-terms c) pdv)))
  pdv)

(defgeneric list-of-variables (polynomial &optional variables)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod list-of-variables ((x mpolynomial) &optional list-of-variables)
  (let ((domain (domain-of x)))
    (loop for order-number in (poly-list-of-variables (poly-form x))
          do (pushnew (get-variable-name order-number domain)
                      list-of-variables :test #'ge-equal))
    list-of-variables))

(defun poly-list-of-variables (p &optional list-of-vars)
  (labels ((terms-list-of-vars (terms)
	     (map-over-each-term terms (ignore c)
	       (p-list-of-vars c)))
	   (p-list-of-vars (p)
	     (cond ((poly-coef? p))
		   (t (pushnew (poly-order-number p) list-of-vars)
		      (terms-list-of-vars (poly-terms p))))))
    (p-list-of-vars p)
    list-of-vars))

(defgeneric coefficient (polynomial variables &optional exponent)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod coefficient ((p mpolynomial) (var symbol) &optional (exponent 1))
  (coefficient p (coerce var (domain-of p)) exponent))  

(defmethod coefficient ((p mpolynomial) (var ge-variable) &optional (exponent 1))
  (coefficient p (coerce var (domain-of p)) exponent))  

(defmethod coefficient
    ((p mpolynomial) (var mpolynomial) &optional (exponent 1))
  (let ((domain (domain-of p)))
    (bind-domain-context domain
      (make-polynomial domain
                       (cond ((eql domain (domain-of var))
                              (if (not (poly-coef? (poly-form var)))
                                  (poly-coefficient (poly-form p)
                                                    (poly-form var)
                                                    exponent)
                                  (poly-c-coefficient (poly-form p)
                                                      (poly-form var)
                                                      exponent)))
                             (t (poly-c-coefficient (poly-form p) var
                                                    exponent)))))))

(defun poly-coefficient (poly var exp)
  (cond ((poly-coef? poly)
	 (if (e0? exp) poly (zero *coefficient-domain*)))
	((same-variable? poly var)
	 (do ((l (poly-terms poly) (red l)))
	     ((terms0? l) (zero *coefficient-domain*))
	   (if (e= (le l) exp)
	       (return (lc l)))))
	((more-main? var poly)
	 (if (e0? exp) poly (zero *coefficient-domain*)))
	(t (do ((l (poly-terms poly))
		(coef (zero *coefficient-domain*)))
	       ((terms0? l) coef)
	     (setq coef (poly-plus
                         (poly-times
                          (poly-simp poly
                                     (make-terms (le l)
                                                 (one *coefficient-domain*)))
                          (poly-coefficient (lc l) var exp))
                         coef))
	     (setq l (red l))))))

(defun poly-c-coefficient (poly var exp)
  (cond ((poly-coef? poly) (coefficient poly var exp))
	(t (do ((l (poly-terms poly))
		(coef (zero *coefficient-domain*)))
	       ((terms0? l) coef)
	     (setq coef (poly-plus
                         (poly-times
                          (poly-simp poly
                                     (make-terms (le l)
                                                 (one *coefficient-domain*)))
                          (poly-c-coefficient (lc l) var exp))
                         coef))
	     (setq l (red l))))))

(defun poly-leading-coefficient (poly)
  (if (poly-coef? poly) poly
      (poly-leading-coefficient (lc (poly-terms poly)))))

(defmethod-sd binary= ((x mpolynomial) (y mpolynomial))
  (bind-domain-context domain
    (poly-0? (poly-difference (poly-form x) (poly-form y)))))

(defmethod-sd binary> ((x mpolynomial) (y mpolynomial))
  (let ((pf-x (poly-form x))
	(pf-y (poly-form y)))
    (cond ((and (poly-coef? pf-x) (poly-coef? pf-y))
	   (> pf-x pf-y))
	  (t (call-next-method)))))

(defgeneric get-variable-name (order-number domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-variable-name (order-number (domain variable-hash-table))
  (aref (variable-index-table domain) order-number 0))

(defmethod variable-index ((domain domain) (p mpolynomial))
  (poly-order-number (poly-form p)))

(defmethod parse-linear-equation ((p mpolynomial) &rest variables)
  (let ((domain (domain-of p)))
    (bind-domain-context domain
      (loop for var in variables
	    with poly = (poly-form p)
	    and coefs
	    unless (eql (domain-of var) domain)
            do (error "~S is not a variable of ~S" var domain)
	    do (setq var (poly-form var))
            (push (poly-coefficient poly var 1) coefs)
            (setq poly (poly-difference poly
                                        (poly-times (first coefs) var)))
	    finally (return (values
                             (values
                              (mapcar #'(lambda (c)
                                          (make-polynomial domain c))
                                      (reverse coefs)))
                             (make-polynomial domain poly)))))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Univariate Polynomial Domain
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; upolynomial.lisp,v 1.6 1994/12/20 22:43:27 sekhar Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.6")

;;; ===========================================================================
;;;			       Univariate Polynomial Elements
;;; ===========================================================================

;;; Univariate polynomials are a vector of coefficients, with the degree
;;; of the term equal to the index in the vector.  (I.e. the leftmost
;;; coefficient is for the x^0 term, and the rightmost for the x^n term).
;;; We explicitly assume that the rightmost coefficient is non-zero,
;;; unless it is a zero-degree polynomial.
;;; Unlike multivariate polynomials, even a zero-degree polynomial will
;;; be represented as a (singleton) vector.  Right now I can't see any
;;; reason to allow a coefficient as a polynomial.
;;; The coef-list vector should never have length 0.

;;; In many places this code assumes that the coefficient domain is an
;;; integral domain.  In the remainder code it assumes the coefficient
;;; domain is a field.

(defmethod scalar? ((x upolynomial))
  (= 1 (length (poly-form x))))

(defmethod 0? ((x upolynomial))
  (coef-list0? (poly-form x)))
  
(defsubst coef-list0? (c)
  (and (= 1 (length c))
       (0? (svref c 0))))

(defmacro coef-list0 (coefficient-domain)
  `(vector (zero ,coefficient-domain)))

(defmethod 1? ((x upolynomial))
  (coef-list1? (poly-form x)))

(defsubst coef-list1? (c)
  (and (= 1 (length c))
       (1? (svref c 0))))

(defmacro coef-list1 (coefficient-domain)
  `(vector (one ,coefficient-domain)))    

;; This produces the polynomial 'x+0' where x is the variable of the ring
(defmacro clist-x (coefficient-domain)
  `(vector (zero ,coefficient-domain) (one ,coefficient-domain)))

(defmacro copy-clist (c)
  `(make-array (length ,c) :initial-contents ,c))

(defmacro clist-degree (c)
  `(cl:- (length ,c) 1))

;; Need the variable argument here because all degree methods need the
;; same number of required arguments.
(defmethod degree ((x upolynomial) variable &rest other-variables)
  (declare (ignore variable other-variables))
  (clist-degree (poly-form x)))

(defmacro clist-zero-deg? (c)
  `(cl:= (length ,c) 1))

(defmacro clist-length (c)     ; Always 1 more than clist-degree, of course.
  `(length ,c))

(defmacro make-clist (length &rest args)
  `(make-array ,length ,@args))

(defmacro clist-get (cl exp)
  `(svref ,cl ,exp))

(defmacro clist-subseq (cl beg end)
  `(subseq ,cl ,beg ,end))

(defgeneric weyl::make-upolynomial (domain coef-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod weyl::make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list array))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain)))
    (loop for i fixnum below (array-dimension coef-list 0)
	  do (setf (svref coef-list i)
		   (coerce (svref coef-list i) coef-domain)))
    (make-instance 'upolynomial :domain domain 
		   :form (clist-simplify coef-list))))

(defgeneric make-upolynomial (domain coef-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list array))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
		 :form (clist-simplify coef-list)))

(defmethod weyl::make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list list))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (let* ((len (length coef-list))
	 (array (make-array len))
	 (coef-domain (coefficient-domain-of domain)))
    (loop for i downfrom (1- len)
	  and #-ANSI-CL for c in coef-list
	  do (setf (svref array i) (coerce c coef-domain)))
    (make-instance 'upolynomial :domain domain 
		   :form array)))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (coef-list list))
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
                 :form (clist-simplify
			(make-array (length coef-list)
				    :initial-contents (reverse coef-list)))))

(defmethod make-upolynomial ((domain multivariate-polynomial-ring) coef-list)
  (unless (null (rest (ring-variables domain)))
    (error "Cannot create a univariate polynomial in ~S" domain))
  (make-instance 'upolynomial :domain domain 
                 :form (clist-simplify coef-list)))

(defmethod make-upolynomial
    ((domain multivariate-polynomial-ring) (poly mpolynomial))
  (let* ((var (car (ring-variables (domain-of poly))))
	 (form (poly-form poly))
	 (coef-list (make-array (1+ (degree poly var))
				:initial-element
				(zero (coefficient-domain-of domain)))))
	(if (poly-coef? form)
	    (setf (svref coef-list 0) form)
	    (map-over-each-term (poly-terms form) (e c)
	      (setf (svref coef-list e) c)))
       (make-upolynomial domain coef-list)))


(defmethod print-object ((p upolynomial) stream)
  (print-upolynomial 
     (first (ring-variables (domain-of p)))
     (poly-form p)
     stream))

;; This is still rough with spacing, parenthesis, etc.
(defun print-upolynomial (var clist stream)
  (labels ((print-term (e c)
             (cond ((cl:= e 0)
                    (print-object c stream))
                   (t
		    (if (not (1? c))
			(print-object c stream))
		    #+Genera
		    (format stream "~'i~A~" var)
		    #-Genera
		    (display var stream)
		    (if (cl:> e 1)
			#+Genera
			(format stream "~S" e)
			#-Genera
			(format stream "^~S" e))))))
    (let ((exp (clist-degree clist))
          coef)
      (print-term exp (clist-get clist exp))
      (loop until (cl:zerop exp) do
	(setq exp (- exp 1))
	(setq coef (clist-get clist exp))
	(cond ((0? coef) nil)
	      ((minus? coef)
	       (princ " - " stream)
	       (print-term exp (minus coef)))
	      (t (princ " + " stream)
		 (print-term exp coef)))))))

;;; ===========================================================================
;;;				    POLYNOMIAL ARITHMETIC
;;; ===========================================================================

(defmethod coerce ((poly upolynomial) (domain general-expressions))
  (let* ((var (car (ring-variables (domain-of poly))))
	 (form (poly-form poly)))
	(loop for i below (length form) with exp = 0
	      unless (0? (svref form i))
		do (setq exp (+ exp (* (svref form i)
				       (make-ge-expt domain var i))))
	      finally
	   (return exp))))

;; Make sure the highest degree coefficient is non-zero
(defun clist-simplify (c)
  (let ((end (clist-degree c)))
    (if (or (cl:= end 0)
            (not (0? (clist-get c end))))
        c
        (loop
          (setq end (- end 1))
          (if (or (not (0? (clist-get c end)))
                  (cl:= end 0))
              (return (clist-subseq c 0 (+ end 1)))
              nil)))))

(defmethod-sd plus ((x upolynomial) (y upolynomial))
  (make-upolynomial (domain-of x)
                    (clist-plus (poly-form x) (poly-form y))))

(defun clist-plus (x y)
  (let ((x-deg (clist-degree x))
	(y-deg (clist-degree y))
	array
	exp)
    (flet ((zipper-sum ()
	     (loop for i downfrom exp
		   while (not (cl:minusp i))
		   do (setf (svref array i) (+ (svref x i) (svref y i))))))

      (cond ((cl:> x-deg y-deg)
	     (setq array (make-array (cl:1+ x-deg)))
	     (setq exp y-deg)
	     (loop for i fixnum downfrom x-deg
		   while (cl:>= i y-deg)
		   do (setf (svref array i) (svref x i)))
	     (zipper-sum))
	    ((cl:> y-deg x-deg)
	     (setq array (make-array (cl:1+ y-deg)))
	     (setq exp x-deg)
	     (loop for i downfrom y-deg
		   while (cl:>= i x-deg)
		   do (setf (svref array i) (svref y i)))
	     (zipper-sum))
	    (t (loop for i fixnum downfrom x-deg
		     do 
		      (cond ((cl:minusp i)
			     (setq array (make-array 1))
			     (setf (svref array 0)
				   (zero *coefficient-domain*)))
			    ((not (0? (+ (svref x i) (svref y i))))
			     (setq exp i)
			     (setq array (make-array (cl:1+ exp)))
			     (zipper-sum)
			     (return t))))))
      array)))

(defmethod minus ((x upolynomial))
  (make-upolynomial (domain-of x) (clist-minus (poly-form x))))

(defun clist-minus (x)
  (do ((anslist (make-clist (clist-length x)))
       (exp 0 (+ exp 1)))
      ((cl:= exp (clist-length x)) 
       anslist)
    (setf (clist-get anslist exp) (minus (clist-get x exp)))))

(defmethod minus? ((x upolynomial))
    (clist-minus? (poly-form x)))

(defun clist-minus? (x)
  (minus? (clist-get x (clist-degree x))))

(defmethod-sd difference ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x) 
		      (clist-difference (poly-form x) (poly-form y)))))
                                                  
(defun clist-difference (x y)
  (let ((x-deg (clist-degree x))
	(y-deg (clist-degree y))
	array
	exp)
    (flet ((zipper-sum ()
	     (loop for i downfrom exp
		   while (not (cl:minusp i))
		   do (setf (svref array i) (- (svref x i) (svref y i))))))

      (cond ((cl:> x-deg y-deg)
	     (setq array (make-array (cl:1+ x-deg)))
	     (setq exp y-deg)
	     (loop for i fixnum downfrom x-deg
		   while (cl:>= i y-deg)
		   do (setf (svref array i) (svref x i)))
	     (zipper-sum))
	    ((cl:> y-deg x-deg)
	     (setq array (make-array (cl:1+ y-deg)))
	     (setq exp x-deg)
	     (loop for i downfrom y-deg
		   while (cl:>= i x-deg)
		   do (setf (svref array i) (- (svref y i))))
	     (zipper-sum))
	    (t (loop for i fixnum downfrom x-deg do 
	         (cond ((cl:minusp i)
			(setq array (make-array 1))
			(setf (svref array 0)
			      (zero *coefficient-domain*))
			(return t))
		       ((not (0? (- (svref x i) (svref y i))))
			(setq exp i)
			(setq array (make-array (cl:1+ exp)))
			(zipper-sum)
			(return t))))))
      array)))

(defmethod-sd times ((x upolynomial) (y upolynomial))
  (make-upolynomial (domain-of x) 
		    (clist-times (poly-form x) (poly-form y))))

(defun clist-times (x y)
  (declare (type simple-array x y)
	   (optimize (safety 0)))
  (let* ((xlen (clist-length x))
         (ylen (clist-length y))
         (anslist (make-array (cl:- (the fixnum (cl:+ xlen ylen)) 1)
			      :initial-element (zero *coefficient-domain*))))
    (declare (fixnum xlen ylen)
	     (type simple-array anslist))
    (do ((xexp 0 (cl:+ xexp 1)))
        ((cl:= xexp xlen)
         anslist)                        ; return this when done
      (declare (fixnum xexp))
      (do ((xelt (clist-get x xexp) )
           (yexp 0 (cl:+ yexp 1))
           (ansexp xexp (cl:+ ansexp 1)))
          ((cl:= yexp ylen))
	(declare (fixnum yexp ansexp))
        (setf (clist-get anslist ansexp)
              (+ (clist-get anslist ansexp)
		 (* xelt (clist-get y yexp))))))))

;;; In CLIST-EXPT the second argument is a repetition count, and thus
;;; is actually an integer.

(defmethod expt ((base upolynomial) (expt integer))
  (bind-domain-context (domain-of base)
    (make-upolynomial (domain-of base)
		      (clist-exptsq (poly-form base) expt))))

(defun clist-exptsq (c n)
  (%funcall (repeated-squaring #'clist-times (coef-list1 *coefficient-domain*))
           c n))

;; This assumes the coefficient domain is a field
(defmethod-sd quotient ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-quotient (poly-form x) (poly-form y)))))

(defun clist-quotient (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide ~S by zero univariate polynomial ~S" x y))
        ((cl:> (clist-degree y) (clist-degree x))
         (error "Attempt to divide ~S by uni. poly. of higher degree ~S" x y))
        (t
         (multiple-value-bind (q r) (clist-divide x y)
           (if (coef-list0? r) 
               q
               (error "Quotient of ~S, and ~S not exact" x y))))))

(defmethod-sd remainder ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-remainder (poly-form x) (poly-form y)))))

(defun clist-remainder (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide ~S by zero univariate polynomial ~S" x y))
        (t
         (multiple-value-bind (q r) (clist-divide x y)
	   (declare (ignore q))
           r))))
         
;; This could be improved.  clist-divide should be merged with
;; clist-remainder and clist-quotient.  That way we wouldn't be
;; generating a remainder whe we only want a quotient and vice versa.
;; --RZ (FIXTHIS)

(defun clist-divide (x y)
  (cond ((coef-list0? y)
         (error "Attempt to divide by zero univariate polynomial ~S" y))
        ((coef-list1? y)
         (values x (coef-list0 *coefficient-domain*)))
        ((cl:> (clist-degree y) (clist-degree x))
         (values (coef-list0 *coefficient-domain*) x))
        (t
         (let* ((xdeg (clist-degree x))
                (ydeg (clist-degree y))
                (yhigh (clist-get y ydeg))
                (qdeg (cl:- xdeg ydeg))
                (rem (copy-clist x))
                (quot (make-clist (cl:+ qdeg 1))))
           (do* ((qexp qdeg (cl:- qexp 1)))
                ((cl:= qexp -1)
                 (values quot (clist-simplify rem)))
             (let ((c (/ (clist-get rem (cl:+ ydeg qexp)) yhigh)))
               (setf (clist-get quot qexp) c)
               (do ((yexp ydeg (- yexp 1)))
                   ((cl:= yexp -1))
                 (setf (clist-get rem (+ yexp qexp))
                            (- (clist-get rem (+ yexp qexp))
			       (* c (clist-get y yexp)))))))))))

;;; This gcd uses Euclid's algorithm and the above remainder function.
(defmethod-sd binary-gcd ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-gcd (poly-form x) (poly-form y)))))

(defun clist-gcd (x y)
  (do ((a x b)
       (b y (clist-remainder a b)))
      ((coef-list0? b)
       a)
    ))

(defmethod-sd binary-lcm ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (make-upolynomial (domain-of x)
		      (clist-lcm (poly-form x) (poly-form y)))))

(defun clist-lcm (x y)
  (clist-times (clist-quotient x (clist-gcd x y)) y))

;; Compute the gcd d of a,b and upolynomials x,y such that ax + by = d.
;; This method works for univariate polynomial ring over a field.
(defmethod extended-gcd ((a upolynomial) (b upolynomial))
  (let ((domain (domain-of a)))
       (setq *coefficient-domain* (coefficient-domain-of domain))       
       (multiple-value-bind
	   (d x y) (clist-extended-gcd (poly-form a) (poly-form b))
	 (values (make-upolynomial domain d)
		 (make-upolynomial domain x)
		 (make-upolynomial domain y)))))

(defun clist-extended-gcd (a b)
  (if (coef-list0? b)
      (values a (make-array 1 :initial-element 1)
	      (make-array 1 :initial-element 0))
      (multiple-value-bind (q r) (clist-divide a b)
	(multiple-value-bind (d x y) (clist-extended-gcd b r)
	  (values d y (clist-difference x (clist-times q y)))))))

;; I don't know why this routines was ever here (but there's a similar
;; routine in mpolynomial).  --RZ (FIXTHIS)
#+ignore
(defmethod-sd binary> ((x upolynomial) (y upolynomial))
  (let ((cl-x (poly-form x))
	(cl-y (poly-form y)))
    (cond ((and (clist-zero-deg? cl-x) (clist-zero-deg? cl-y))
           (> (clist-get cl-x 0) (clist-get cl-y 0)))
          (t nil))))

(defmethod-sd binary= ((x upolynomial) (y upolynomial))
  (bind-domain-context (domain-of x)
    (coef-list0? (clist-difference (poly-form x) (poly-form y)))))

;; this should return a list of (polynomial . power) pairs
(defmethod factor ((poly upolynomial))
  (bind-domain-context (domain-of poly)
    (if (typep *coefficient-domain* 'gfp)
	(if (cl:< 2 (characteristic *coefficient-domain*))
	    (mapcar #'(lambda (pair)
		        (cons (make-upolynomial *domain* (car pair))
			      (cdr pair)))
		    (clist-factor (poly-form poly)))
	    (error "Can't factor univariate polynomials over GF(2) yet"))
	(error "Can't factor unless coefficients domain is GF(p)"))))

(defmethod cfactor ((poly upolynomial))
  (let* ((factlist (factor poly))
         (prod (one (domain-of poly))))
    (mapcar #'(lambda (fact) 
	        (setq prod (* prod (expt (car fact) (cdr fact))))
	      t)
            factlist)
    (if (= prod poly)
        factlist
        (error "Factor didn't work.  Prod ~S not = ~S" factlist poly))))


;; This should return a list of (clist . power) pairs
(defun clist-factor (poly)
  (if (coef-list0? (clist-deriv poly))
      (if (cl:= (clist-degree poly) 0)
          (list (cons poly 1))
          (let ((p (characteristic *coefficient-domain*)))
            (mapcar #'(lambda (fact)
                        (cons (car fact) (cl:* p (cdr fact))))
                    (clist-factor (clist-pth-root poly)))))
      (loop for (pp . degree) in (clist-sqfr-decompose poly)
	    append (loop for p in (clist-factor-squarefree pp)
			 collect (cons p degree)))))

;; The exclamation point indicatest that this routine modifies its
;; argument.
(defun clist-primitive-part! (poly)
  (let* ((deg (clist-degree poly))
	 (lc (clist-get poly deg)))
    (unless (1? lc)
      (loop for i fixnum below deg
	    do (setf (clist-get poly i) (/ (clist-get poly i) lc)))
      (setf (clist-get poly deg) (one *coefficient-domain*)))
    poly))
	     
	  
;; As currently constituted this routine first monicizes the
;; polynomial and then computes the primitive part.  It should just
;; multiply it into the square free terms.  --RZ (FIXTHIS)
(defmethod square-free ((poly upolynomial))
  (bind-domain-context (domain-of poly)
    (mapcar #'(lambda (pair)
	        (cons (make-upolynomial *domain* (car pair))
		      (cdr pair)))
	    (clist-sqfr-decompose (poly-form poly)))))

(defun clist-sqfr-decompose (poly)
  (loop for f1 = poly then f2
	for n upfrom 0
	for prod1 = nil then prod2
	with factor and factors and deriv and f2 and prod2 and
	p = (characteristic *coefficient-domain*)
	do (when (coef-list0? (setq deriv (clist-deriv f1)))
	     (push (cons (clist-primitive-part! prod1) n) factors)
	     (unless (cl:= (clist-degree f1) 0)
	       (push (cons (clist-primitive-part! (clist-pth-root f1)) p)
		     factors))
	     (return (reverse factors)))
	   (setq f2 (clist-gcd deriv f1))
	   (setq prod2 (if (cl:> (clist-degree f2) 0)
			    (clist-quotient f1 f2)
			    f1))
	   (when prod1
	     (setq factor (clist-quotient prod1 prod2)))
	   (when (cl:plusp (clist-degree factor))
	     (push (cons (clist-primitive-part! factor) n) factors))))

(defun clist-deriv (f)
  (if (cl:= (clist-degree f) 0)
      (coef-list0 *coefficient-domain*)
      (let ((deriv (make-clist (cl:- (clist-length f) 1))))
        (do ((exp (clist-degree f) (- exp 1)))
            ((cl:= exp 0)
             (clist-simplify deriv))
          (setf (clist-get deriv (- exp 1))
                (* (coerce exp *coefficient-domain*)
		   (clist-get f exp)))))))

(defmethod derivation ((f upolynomial))
  (bind-domain-context (domain-of f)
    (make-upolynomial (domain-of f)
		      (clist-deriv (poly-form f)))))

;; this assumes that poly is a proper pth power -- i.e. that its 
;; degree is a multiple of p, and all non-zero terms have degree
;; a multiple of p.  in this case, the pth root is just the polynomial
;; generated by dividing all the exponents by p.
(defun clist-pth-root (poly)
  (let* ((p (characteristic *coefficient-domain*))
         (rootdegree (cl:/ (clist-degree poly) p))
         (root (make-clist (cl:+ rootdegree 1))))
    (do ((pexp (clist-degree poly) (cl:- pexp p))
         (rexp rootdegree (cl:- rexp 1)))
        ((cl:= rexp -1)
         root)
      (setf (clist-get root rexp) (clist-get poly pexp)))))

;; this assumes that poly is square-free, and returns a list of factors
;; (as clists).
(defun clist-factor-squarefree (poly)
  (if (cl:= (clist-degree poly) 0)
      (list poly)
      (let ((factlist nil)
            (x (clist-x *coefficient-domain*))    ; the poly 'x'
            (p (characteristic *coefficient-domain*))
            dpoly)
        (when (0? (clist-get poly 0))
	  (setq factlist (append (list x) factlist))
	  (setq poly (clist-quotient poly x)))
        (do ((n 1 (+ n 1))
             (power p (* power p))    ; always equals p^n
             (f poly (cond ((cl:= (clist-degree dpoly) 0) f)
                           ((cl:= (clist-degree dpoly) (clist-degree f))
                            (coef-list1 *coefficient-domain*))
                           (t (clist-quotient f dpoly)))))
            ((cl:= (clist-degree f) 0)
             factlist)
          (cond ((cl:< (clist-degree f) (cl:* 2 n))
		 (setq n (clist-degree f))
		 (setq dpoly f))
		(t (setq dpoly (clist-gcd f (clist-difference 
					     (clist-expt-mod-poly x power f)
					     x)))))
          (if (cl:> (clist-degree dpoly) 0)
              (setq factlist 
                    (append factlist
                            (clist-factor-product-nth-degrees dpoly n))))))))
          
;; this evaluates (x)^n mod poly by repeated squaring
(defun clist-expt-mod-poly (x n poly)
  (%funcall (repeated-squaring
	      #'(lambda (a b) (clist-remainder (clist-times a b) poly))
	      (coef-list1 *coefficient-domain*))
	    x n))

;; this assumes that poly is a product of nth degree irreducible factors.
;; it returns a list of factors (as clists, of course)
(defun clist-factor-product-nth-degrees (poly n)
  (if (cl:= (clist-degree poly) n)
      (list poly)
      (let ((exp (/ (- (expt (characteristic *coefficient-domain*) n) 1) 2))
            (factlist nil)
            (base (clist-x *coefficient-domain*)))  ; the poly 'x+0'
        (do ((reducibles (list poly) newreducibles)
             (newreducibles nil nil))
            ((null reducibles)
             factlist)
          (dolist (f reducibles)
            (let* ((q1 (clist-gcd f
                           (clist-plus (clist-expt-mod-poly base exp f)
                                       (coef-list1 *coefficient-domain*))))
                   (q2 (if (cl:= (clist-degree q1) 0)
                           f
                           (clist-quotient f q1))))
              (cond ((cl:> (clist-degree q1) n) (push q1 newreducibles))
                    ((cl:= (clist-degree q1) n) (push q1 factlist)))
              (cond ((cl:> (clist-degree q2) n) (push q2 newreducibles))
                    ((cl:= (clist-degree q2) n) (push q2 factlist)))))
          (setf (clist-get base 0)
                     (random *coefficient-domain*)))))) ; reset base to 'x+a'



;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Expanded Polynomials
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; epolynomial.lisp,v 1.9 1994/11/15 19:55:25 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(defgeneric make-epolynomial* (domain greater-function terms)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-epolynomial*
    ((domain multivariate-polynomial-ring) greater-function terms)
  (make-instance 'epolynomial :domain domain
		 :greater-function greater-function
		 :form terms))

(defgeneric make-epolynomial (domain greater-function poly)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-epolynomial
    ((domain multivariate-polynomial-ring) greater-function (poly mpolynomial))
  (make-epolynomial domain greater-function (poly-form poly)))

(defmethod make-epolynomial
    ((domain multivariate-polynomial-ring) greater-function (poly epolynomial))
  (if (eql greater-function (greater-function-of poly)) poly
      (make-epolynomial* domain greater-function
			 (sort (poly-form poly) greater-function))))

(defmethod make-epolynomial 
    ((domain multivariate-polynomial-ring) greater-function
     (r rational-function))
  (let ((c (coercible? r (coefficient-domain-of domain)))
	term)
    (cond ((null c) (call-next-method))
	  ((0? c)
	   (make-epolynomial* domain greater-function ()))
	  (t (setq term (make-array (cl:1+ (length (ring-variables domain)))
				    :initial-element 0))
	     (setf (aref term 0) c)
	     (make-epolynomial* domain greater-function (list term))))))

(defmethod make-epolynomial
	   ((domain multivariate-polynomial-ring) greater-function (form list))
  (let* ((dimension (length (ring-variables domain)))
	 poly-form)
    (labels ((scan-poly-form (form exp next-var)
	       (cond ((poly-coef? form)
		      (unless (0? form)
			(loop for i fixnum upfrom next-var below dimension
			      do (push 0 exp))
			(push (make-array (1+ dimension) 
					  :initial-contents
					  (cons form (reverse exp)))
			      poly-form)))
		     (t (let ((var-index (poly-order-number form)))
			  (loop for i fixnum upfrom next-var below var-index
				do (push 0 exp))
			  (setq var-index (1+ var-index))
			  (map-over-each-term (poly-terms form) (e c)
			    (scan-poly-form c (cons e exp)
					    var-index)))))))
      (scan-poly-form form () 0)
      (setq greater-function 
	    (get-comparison-fun dimension greater-function))
      (make-epolynomial* domain greater-function
			(sort poly-form greater-function)))))
	
(defmethod print-object ((poly epolynomial) stream)
  (if (0? poly) (princ 0 stream)
      (with-slots (var-count) poly
        (let* ((first? t)
	       (variables (ring-variables (domain-of poly)))
	       (dim (cl:1+ (length variables))))
	  (loop for term in (poly-form poly)
		for c = (svref term 0)
		do (cond ((minus? c)
			  (setq c (minus c))
			  (if (and (1? c) (not (gterm-constant? term dim)))
			      (princ " -" stream)
			      (format stream " - ~S" c)))
			 ((null first?)
			  (if (and (1? c) (not (gterm-constant? term dim)))
			      (princ " + " stream)
			      (format stream " + ~S" c)))
			 (t (unless (and (1? c) (not (gterm-constant? term dim)))
			      (format stream "~S" c))))
	    (loop for var in variables
		  for i fixnum upfrom 1
		  for exp = (svref term i) do
              (unless (cl:zerop exp)
		(princ " " stream)
		(display var stream)
		(unless (eql 1 exp)
		  (format stream "^~D" exp))))
		(setq first? nil))))))

(defmethod 0? ((x epolynomial))
  (null (poly-form x)))

(defmethod 1? ((x epolynomial))
  (let ((form (poly-form x)))
    (and (null (rest form))
	 (0? (le form))
	 (1? (lc form)))))

(defmethod binary= ((p epolynomial) (q epolynomial))
  (0? (- p q)))

(defun get-comparison-fun (num-vars name)
  (cond ((and (not (symbolp name)) (functionp name))
	 name)
	((fboundp name) (symbol-function name))
	((eql name :lexical)
	 (make-comparison-fun num-vars 
			      (loop for i fixnum below num-vars 
				    collect (cl:1+ i))
			      :total? nil :reverse? nil))
	((eql name :revlex)
	 (make-comparison-fun num-vars 
			      (reverse
				(loop for i fixnum below num-vars
				      collect (cl:1+ i)))
			      :total? t :reverse? t))
	((eql name :total-lexical)
	 (make-comparison-fun num-vars
			      (loop for i fixnum below num-vars
				    collect (cl:1+ i))
			      :total? t :reverse? nil))
	((eql name :total-revlex)
	 (make-comparison-fun num-vars
			      (reverse 
				(loop for i fixnum below num-vars
				      collect (cl:1+ i)))
			      :total? t :reverse? t))
	(t (error "Unknown comparison function ~S" name))))

(defun make-comparison-fun (num-vars var-order &key total? reverse? new?)
 (let ((name (intern (format nil "COMPARE<~D>-~D~{.~D~}-~S-~S"
			     num-vars (first var-order) (rest var-order)
			     total? reverse?)))
       great less)
   (if (null reverse?)
       (setq great 'cl:> less 'cl:<)
       (setq great 'cl:< less 'cl:>))
   (unless (and (null new?) (fboundp name))
     (compile name 
       `(lambda (a b)
	  (let (a-temp b-temp)
	       (declare (fixnum a-temp b-temp)
			(optimize (safety 0)))
	    (cond
	      ,@(when total?
		  `(((cl:plusp
		       ,(if (> num-vars 10)
			    `(loop for i fixnum upfrom 1 below ,num-vars
				   summing
				   (the fixnum
					(- (the fixnum (svref a i))
					   (the fixnum (svref b i)))))
			    `(cl:+ ,@(loop for i upfrom 1 below num-vars
					   collect
					   `(the fixnum
						 (- (the fixnum (svref a ,i))
						    (the fixnum (svref b ,i))))))))
		     t)
		    ((cl:< a-temp b-temp) nil)))
	      ,@(loop for v in var-order
		      append `(((,great (setq a-temp (svref a ,v))
				      (setq b-temp (svref b ,v)))
				   t)
			       ((,less a-temp b-temp) nil))))))))
   (values (symbol-function name) name)))

(defmethod make-polynomial 
    ((domain multivariate-polynomial-ring) (epol epolynomial))
  (unless (eql domain (domain-of epol))
    (error "Don't understand this: ~S" epol))
  (let* ((terms (poly-form epol))
         (dim (cl:- (array-dimension (first terms) 0) 1)))
    (labels ((convert-term (term i)
	       (cond ((cl:zerop i) (aref term 0))
		     ((cl:zerop (aref term i))
		      (convert-term term (cl:- i 1)))
		     (t (cons (cl:- i 1) 
			      (make-terms (aref term i) 
					  (convert-term term (cl:- i 1))))))))
      (loop with ans = (convert-term (first terms) dim)
	    for term in (rest terms)
	    do (setq ans (poly-plus ans (convert-term term dim)))
	    finally (return (make-polynomial domain ans))))))
  
(defmethod coerce ((p epolynomial) (d general-expressions))
  (let* ((domain (domain-of p))
	 (vars (ring-variables domain))
	 (n (length vars)))
    (labels ((transform-term (term)
	       (let ((factors (list (coerce (aref term 0) d))))
		 (loop for i below n
		       for e = (aref term (cl:1+ i))
		       do (unless (cl:zerop e)
			    (push (expt (nth i vars) e) factors)))
		 (simp-times-terms d factors))))
       (simp-plus-terms d (loop for term in (poly-form p)
				collect (transform-term term))))))

;;; ===========================================================================
;;;			Polynomial Arithmetic
;;; ===========================================================================

;; Notice that we can use MAP-OVER-EACH-TERM since it doesn't depend
;; upon the exponent arithmetic, while UPDATE-TERMS does.

(defmacro same-greater-functions ((x y) &body body)
  `(with-slots (greater-function) x
     (unless (eq greater-function (slot-value ,y 'greater-function))
       (error "EPolynomials don't have same compare function: ~S and ~S"
	      ,x ,y))
    ,@body))

(defun make-eterm (exp coef)
  (declare (type simple-array exp)
	   (optimize (safety 0)))
  (let* ((dim (array-dimension exp 0))
	 (term (make-array dim)))
    (declare (fixnum dim)
	     (type simple-array term))
    (setf (svref term 0) coef)
    (do ((i 1 (cl:1+ i)))
	((cl:> i dim) term)
      (declare (fixnum i))
      (setf (svref term i) (svref exp i)))))

;; These should really check to make sure that the number of variables
;; hasn't changed.
(defmethod-sd plus ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-plus greater-function (poly-form x) (poly-form y))))))

(defun gterms-plus (greater-function x y) ;x and y are term lists
  (let ((ans-terms (list nil))
	(terms nil)
	sum)
    (macrolet
	((collect-term (.e. .c.)
	   `(progn (setf (rest terms) (list (make-eterm , .e. , .c.)))
		   (setf terms (rest terms))))
	 (collect-old-term (term)
	   `(progn (setf (rest terms) (list ,term))
		   (setf terms (rest terms)))))
      (setq terms ans-terms)
      (loop
       (cond ((terms0? x)
	      (cond ((terms0? y) (return (rest ans-terms)))
		    (t (collect-old-term (lt y))
		       (setq y (red y)))))
	     ((or (terms0? y)
		  (%funcall greater-function (lt x) (lt y)))
	      (collect-old-term (lt x))
	      (setq x (red x)))
	     ((%funcall greater-function (lt y) (lt x))
	      (collect-old-term (lt y))
	      (setq y (red y)))
	     (t (setq sum (+ (svref (lt x) 0) (svref (lt y) 0)))
		(unless (0? sum)
		  (collect-term (lt x) sum))
		(setq x (red x) y (red y))))))))

(defmethod minus ((x epolynomial))
  (let ((domain (domain-of x)))
    (bind-domain-context domain      
      (make-epolynomial* domain (slot-value x 'greater-function)
			 (gterms-minus (poly-form x))))))

(defun gterms-minus (x)
  (loop for term in x
	collect (make-eterm term (- (svref term 0)))))

(defmethod-sd difference ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-difference greater-function
					    (poly-form x) (poly-form y))))))

(defun gterms-difference (greater-function x y) ;x and y are term lists
  (let ((ans-terms (list nil))
	(terms nil)
	sum)
    (macrolet
	((collect-term (.e. .c.)
	   `(progn (setf (rest terms) (list (make-eterm , .e. , .c.)))
		   (setf terms (rest terms))))
	 (collect-old-term (term)
	   `(progn (setf (rest terms) (list ,term))
		   (setf terms (rest terms)))))
      (setq terms ans-terms)
      (loop
       (cond ((terms0? x)
	      (cond ((terms0? y) (return (rest ans-terms)))
		    (t (collect-term (lt y) (- (elt y 0)))
		       (setq y (red y)))))
	     ((or (terms0? y)
		  (%funcall greater-function (lt x) (lt y)))
	      (collect-old-term (lt x))
	      (setq x (red x)))
	     ((%funcall greater-function (lt y) (lt x))
	      (collect-term (lt y) (- (elt (lt y) 0)))
	      (setq y (red y)))
	     (t (setq sum (- (elt (lt x) 0) (elt (lt y) 0)))
		(unless (0? sum)
		  (collect-term (lt x) sum))
		(setq x (red x) y (red y)))))))) 

(defmethod-sd times ((x epolynomial) (y epolynomial))
  (same-greater-functions (x y)
    (bind-domain-context domain
      (make-epolynomial* domain greater-function
			 (gterms-times greater-function
				       (poly-form x) (poly-form y))))))

;; Assumes we are working over an integral domain.
(defun gterms-mon-times (poly-terms term)
  (let ((c (svref term 0))
	(dim (array-dimension term 0)))
    (if (0? c) (terms0)
	(loop for pterm in poly-terms
	      collect (let ((nterm (make-array dim)))
			(setf (svref nterm 0)
			      (* (svref pterm 0) (svref term 0)))
			(loop for i fixnum upfrom 1 below dim
			      do (setf (svref nterm i)
				       (cl:+  (svref pterm i)
						(svref term i))))
			nterm)))))

(defun gterm-times (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i)
		   (the fixnum (cl:+ (the fixnum (svref x-term i))
				       (the fixnum (svref y-term i))))))
    e))

(defun gterm-quot (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i)
		   (the fixnum (cl:- (the fixnum (svref x-term i))
				       (the fixnum (svref y-term i))))))
    e))

(defun gterm-lcm (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (let ((e (make-array dim)))
    (loop for i fixnum upfrom 1 below dim
	  do (setf (svref e i) (cl:max (the fixnum (svref x-term i))
					(the fixnum (svref y-term i)))))
    e))

(defun gterm-disjoint (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	when (not (or (cl:zerop (the fixnum (svref x-term i)))
		      (cl:zerop (the fixnum (svref y-term i)))))
	  do (return nil)
	finally (return t)))

(defun gterm-dominates (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	when (cl:< (the fixnum (svref x-term i))
		     (the fixnum (svref y-term i)))
	  do (return nil)
	finally (return t)))

(defun gterm-equal (x-term y-term dim)
  (declare (type simple-array x-term y-term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	do (when (not (cl:= (the fixnum (svref x-term i))
			      (the fixnum (svref y-term i))))
	     (return nil))
	finally (return t)))

(defun gterm-constant? (term dim)
  (declare (type simple-array term)
	   (optimize (safety 0)))
  (loop for i fixnum upfrom 1 below dim
	do (when (not (cl:zerop (the fixnum (svref term i))))
	     (return nil))
	finally (return t)))

(defun gterms-times (greater-function x y)
  (let (;; Multiply x by the first term of y.  This is the initial
	;; term list we will modify.
	(answer (gterms-mon-times x (lt y))) 
	(dim (length (first x)))
	e c)
    (setq answer (cons nil answer))
    (loop for y-term in (red y)
	  for ans = answer do
      (loop for x-term in x do
	(unless (0? (setq c (* (svref x-term 0) (svref y-term 0))))
	  (setq e (gterm-times x-term y-term dim))
	  ;; Find place to insert this term.
	  (loop	for red-ans = (red ans) do
	    ;; Sure would be nice if the complier recognized and optimized
	    ;; the usages of (red ans)
	    (cond ((or (terms0? red-ans)
		       (%funcall greater-function e (lt red-ans)))
		   (setf (svref e 0) c)
		   (setf (red ans) (list e))
		   (return t))	
		  ((gterm-equal e (lt red-ans) dim)
		   (setf (svref (lt red-ans) 0)
			 (+ (svref (lt red-ans) 0) c))
		   (return t))
		  (t (setq ans red-ans)))))))
    (loop for ans on answer
	  do (when  (and (red ans) (0? (svref (lt (red ans)) 0)))
	       (setf (red ans) (red (red ans)))))
    (red answer)))

(defmethod expt ((base epolynomial) (expt integer))
  (let ((domain (domain-of base))
	(cf (greater-function-of base)))
    (bind-domain-context domain
      (make-epolynomial* domain cf (gterms-expt cf (poly-form base) expt)))))

(defun gterms-expt (cf terms exp)
  (cond ((e0? exp)
	 (list (make-eterm (lt terms) (one *coefficient-domain*))))
	(t (let ((ans terms))
	     (loop for i below exp
		   do (setf ans (gterms-times cf ans terms)))
	     ans))))


(defmethod make-mpolynomial
    ((domain multivariate-polynomial-ring) (poly epolynomial))
  (let* ((dimension (length (ring-variables domain))))
    (labels ((convert-term (term i)
	       (cond ((> i dimension)
		      (svref term 0))
		     ((cl:zerop (aref term i))
		      (convert-term term (cl:+ i 1)))
		     (t `(,(cl:- i 1)
			   (,(aref term i) .
			      ,(convert-term term (cl:+ i 1))))))))
      (make-polynomial domain
	(loop for term in (poly-form poly)
	      for sum = (convert-term term 1)
		then (poly-plus sum (convert-term term 1))
	      finally (return sum))))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		      Sparse Polynomial Routines for GCD
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; sparsegcd.lisp,v 1.5 1995/03/13 22:36:50 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

;; The linear-form macro makes sure that the linear polynomials we
;; generate are well formed.
;;; FIXME : Generates defined but never used warnings for zero in some cases.
(defmacro with-linear-support (&body body)
  `(let ((zero (zero *coefficient-domain*))
	 (one (one *coefficient-domain*)))
     (macrolet ((linear-form (pt)
		  `(if (0? ,pt) (make-terms (e1) one)
		      (make-terms (e1) one (make-terms (e0) (- ,pt))))))
       ,@body)))

;; This is a general interpolation routine.  Given the value of a
;; univariate polynomial at several points it determines the polynomial.
(defun terms-interpolate (pts vals)
  (with-linear-support
      (do ((u (make-terms (e0) (car vals))
	      (terms-plus
	       u (terms-mon-times qk (e0)
				  (/ (- (car uk)
					(terms-horners-rule u (car xk)))
				     denom))))
	   (qk (linear-form (first pts))
	       (terms-times qk (linear-form (first xk))))
	   (uk (cdr vals) (cdr uk))
	   (xk (cdr pts) (cdr xk))
	   (denom))
	  ((null xk) u)
	(when (poly-0? (setq denom (terms-horners-rule qk (first xk))))
	  (error "~S occurs twice in list of evaluation points: ~S"
		 (first xk) pts)))))

(defun poly-interpolate (var pts vals)
  (poly-simp var (terms-interpolate pts vals)))

(defgeneric interpolate (vars pts vals &key degrees)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod interpolate ((x polynomial) pts vals &rest ignore)
  (declare (ignore ignore))
  (let ((domain (domain-of x)))
    (bind-domain-context domain
      (make-polynomial domain
        (poly-interpolate (poly-form x) pts
			  (loop for v in vals
				collect (if (and (typep v 'mpolynomial)
						 (eql (domain-of v) domain))
					    (poly-form v)
					    v)))))))

(defmethod interpolate ((vars list) pts vals &key (degrees :total))
  (let ((var-cnt (length vars))
	ring array)
    (cond ((typep (first vars) 'domain-element)
	   (setq ring (domain-of (first vars))))
	  (t (setq ring *general*)))
    (setq vars
	  (loop for v in vars
		collect (cond ((symbolp v)
			       (coerce v ring))
			      ((eql ring (domain-of v))
			       v)
			      (t (error
				   "Variables are not of the same domain: ~S"
				   vars)))))
    (loop for pt in pts
	  do (unless (eql (length pt) var-cnt)
	       (error "Point ~S does not have ~D components"
		      pt var-cnt))) 
    (unless (eql (length pts) (length vals))
      (error "Points (~D) and values (~D) are of different lengths"
	     (length pts) (length vals)))

    (when (or (eql degrees :total)
	      (eql degrees :maximum))
      (setq degrees
	    (mapcar #'rest
		    (degree-partition
		      (1+ var-cnt)
		      (bound-degree-term-count var-cnt
					       (length pts)
					       :type degrees)))))
    (setq array (make-degree-matrix ring degrees pts))
    (loop for i below (length pts)
	  with poly = (zero ring)
	  for expt in degrees
	  do (setq poly
		   (+ poly
		     (* (expt-list vars expt)
			(loop for j below (length pts)
			      for v in vals
			      with c = (zero ring)
			      do (setq c (+ c (* v (aref array i j))))
			      finally (return c)))))
	  finally (return poly))))

	     
(defun expt-list (base expt)
  (loop with ans = (one (domain-of (first base)))
	for b in base
	for e in expt
	do (setq ans (* ans (expt b e)))
	finally (return ans)))

#|  The following is often useful for debugging |
(defun display-array (arr)
  (let ((dims (array-dimensions arr)))
    (loop for i below (first dims)
	  do (loop for j below (second dims)
		   do (format t "~S " (aref arr i j)))
	     (fresh-line))))
||#	

(defun bound-degree-term-count (num-vars term-count &key (type :total))
  (cond ((eql type :total)
	 (loop for i upfrom 0
	       do (when (> (combinations (+ i num-vars) i) term-count)
		    (return (1- i)))))
	((eql type :maximum)
	 (1- (truncate (integer-nth-root term-count num-vars))))
	(t (error "Degree bound must be either :total or :maximum: ~S"
		  type))))

;; The following routine returns a list of all the exponent vectors
;; for monomials in v variables of total degree equal to d.  If one
;; wants all the monomials of total degree less than or equal to d,
;; incrase n by one and drop the first component of every list.
(defun degree-partition (v d)
  (cond ((1? v) `((,d)))
	((0? d)
	 (loop for part in (degree-partition (1- v) d)
	       collect (cons 0 part)))
	(t (loop for u below (1+ d)
		 append (loop for part in (degree-partition (1- v) (- d u))
			      collect (cons u part))))))

;; This routine takes a list of exponent vectors and points (values)
;; for the variables and returns the inverse of the matrix where
;; each row consists of one of the vals raised to each of the exponent
;; vectors.
(defun make-degree-matrix (domain expt-vects pts)
  (let ((array (make-array (list (length pts) (length expt-vects)))))
    (loop for i upfrom 0
	  for pt in pts
	  do (loop for j upfrom 0
		   for expt-vect in expt-vects
		   for temp = (one domain)
		   do (setf (aref array i j)
			    (loop for e in expt-vect
				  for p in pt
				  do (setq temp (* temp (expt p e)))
				  finally (return temp)))))
    (invert-array domain array)))

;; This routine could be sped up by computing Q in place.  Theoretical
;; speedups are possible (Kaltofen&Yagati), but they are impractical.
(defun compute-vandermonde-Q (pts)
  (with-linear-support
      (let ((ans (make-terms (e1) (one *coefficient-domain*)
			     (make-terms (e0) (- (first pts))))))
	(loop for pt in (rest pts)
	      do (setq ans (terms-times ans (linear-form pt))))
	ans)))

(defun solve-vandermonde (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let* ((sols (make-array (length vals) :initial-element zero))
	   qi)
      (loop for pt in pts
	    for val in vals
	    do (setq qi (terms-quotient q (linear-form pt)))
	       (map-over-each-term
		    (terms-cquotient qi (terms-horners-rule qi pt))
		    (e c)
		 (setf (aref sols e) (+ (aref sols e) (* c val)))))
      (loop for i below (array-dimension sols 0)
	    collect (aref sols i)))))

(defun solve-vandermondeT (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let ((k (make-array (length vals) :initial-contents vals))
	  qi x sols)
      (loop for pt in pts do
	(setq qi (terms-quotient q (linear-form pt)))
	(setq x zero)
	(map-over-each-term (terms-cquotient qi (terms-horners-rule qi pt))
			    (e c)
          (setq x (+ x (* c (aref k e)))))
	(push x sols))
      sols)))

(defun solve-vandermondeTD (pts vals &optional Q)
  (unless Q
    (setq Q (compute-vandermonde-Q pts)))
  (with-linear-support
    (let ((k (make-array (length vals) :initial-contents vals))
	  qi x sols)
      (loop for pt in pts do
	(setq qi (terms-quotient q (linear-form pt)))
	(setq x zero)
	(map-over-each-term (terms-cquotient qi (terms-horners-rule qi pt))
			    (e c)
          (setq x (+ x (* c (aref k e)))))
	(push x sols))
      (loop for sol in (nreverse sols)
	    for pt in pts
	    collect (/ sol pt)))))

(defun poly-skeleton (poly vars)
  (cond ((poly-coef? poly)
	 (list (loop for i in vars collect 0)))
	((same-variable? poly (first vars))
	 (let ((skeleton ()))
	   (map-over-each-term (poly-terms poly) (e c)
	     (loop for skel in (poly-skeleton c (rest vars))
		   do (push (cons e skel) skeleton)))
	   (nreverse skeleton)))
	((more-main? poly (first vars))
	 (error "Involves a variable that it shouldn't"))
	(t (loop for skel in (poly-skeleton poly (rest vars))
		 collect (cons 0 skel)))))

(defgeneric pskeleton (poly vars)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod pskeleton ((p mpolynomial) vars)
  (let ((vars (mapcar
		#'(lambda (x) (poly-form (coerce x (domain-of p))))
		      vars)))
       (poly-skeleton (poly-form p) vars)))

(defgeneric sparseinterpstage (poly bp d k)
  (:documentation
   "The purpose of this function is unknown."))

;; Sparse multivariate Interpolation.
(defmethod SparseInterpStage ((Pkminus1 mpolynomial) Bp D k)
  (SparseInterpStagei (coefficient-domain-of (domain-of Pkminus1))
		      Pkminus1 Bp D k))

(defgeneric sparseinterpstagei (coef-domain pkminus1 bp d k &optional coef-bound)
  (:documentation
   "The purpose of this function is unknown."))

;; If the polynomial ring is over integers then we can compute over Zp
;; and use Chinese Remainder Theorem.
(defmethod SparseInterpStagei ((coef-domain rational-integers)
			       (Pkminus1 mpolynomial) Bp D k
			       &optional (coef-bound most-positive-fixnum))
  (let* ((domain (domain-of Pkminus1))
	 (vars (list-coerce (list-of-variables Pkminus1) domain))
	 (Skel (pskeleton Pkminus1 vars))
	 (l (length Skel))
	 (S (make-array (list l (1- k)) :initial-contents (reverse Skel)))
	 (primes (choice-primes (* 2 coef-bound)))
	 (gfp nil)
	 (gfp-domain nil)
	 (Y '())
	 (m '())
	 (cur-pt 0)
	 (X '())
	 (vals '())	 
	 (B '())
	 (EQ (make-array (list D l)))
	 (poly (make-array (list (length primes) l)))
	 (Pprime '())
	 (factor (one domain))
	 (temp-poly (zero domain))
	 (Pk (zero domain)))
	(loop for ip upfrom 0 as p in primes do
	  (setq X '())
	  (setq gfp (get-finite-field p))
	  (setq gfp-domain (get-polynomial-ring gfp (ring-variables domain)))
	  (setq *coefficient-domain* gfp)
	  (setq vals (loop for i below (- (length (ring-variables domain)) k)
			   collect (random gfp)))
	  (multiple-value-setq (Y m) (InitY gfp S k l))
	  (loop for i below D do
	    (loop while (member (setq cur-pt (random gfp))
				X))
	    (setq X (append X (list cur-pt)))
	    (setq B (loop for j below l
			  collect (%funcall Bp (append
						 (loop for pt in Y
						       collect (expt pt j))
						 (list cur-pt)
						 vals))))
	    (setq Pprime (Solve-vandermondeT m B))
	    (loop for j upfrom 0 as eqn in Pprime do
	      (setf (aref EQ i j) eqn)))
	  (loop for j below l do
	    (setf (aref poly ip j)
		  (interpolate
		    (coerce (nth (1- k) (ring-variables domain))
			    gfp-domain)
		    X
		    (loop for i below D 
			  collect (aref EQ i j))))))
	(loop for j below l do
	  (setq temp-poly (zero domain))
	  (loop for i below D do
	    (setq temp-poly (+ temp-poly
			       (* (expt (coerce
					  (nth (1- k) (ring-variables domain))
					  domain) i)
		    (compute-result
		      (use-chinese-remainder
			(loop for ip below (length primes)
			  collect (poly-form (coefficient (aref poly ip j)
			       (nth (1- k) (ring-variables domain))
					       i)))))))))
	  (setf (aref poly 0 j) temp-poly))
	(loop for j below l do
	  (setq factor (one domain))
	  (loop for k upfrom 0
		as var in (list-of-variables Pkminus1) do
		  (setq factor (* factor (expt (coerce var domain)
					       (aref S j k)))))
	  (setq Pk (+ Pk (* factor (aref poly 0 j)))))
	Pk))

(defgeneric inity (coef-domain s k l)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod InitY ((coef-domain field) S k l)
  (let ((Y '())
	(m '())
	(f 1))
       (loop while (< (length m) l) do
	 (setq m '())
	 (setq Y (loop for i below (1- k)
		       collect (random coef-domain)))
	 (loop for j below l do
	   (setq f 1)
	   (loop for k upfrom 0
		 as pt in Y do
		   (setq f (* f (expt pt (aref S j k)))))
	   (pushnew f m))
	 finally
      (return (values Y m)))))

;; Bp is the function name representing the black box, i.e, the
;; function should return the value of the polynomial at the
;; requested point.
(defmethod interpolate ((domain multivariate-polynomial-ring)
			Bp degree-bounds &rest ignore)
  (declare (ignore ignore))
;  (setq *cur-poly* (coerce (coerce *cur-poly* *general*) domain))
  (let* ((vars (list-coerce (ring-variables domain) domain))
	 (var (first vars))
	 (coef-domain (coefficient-domain-of domain))
	 (pts (loop for i from 1 below (length vars)
	            collect (coerce i coef-domain)))
	 (p (interpolate var (loop for i upto (first degree-bounds)
				   collect i)
		(loop for i upto (first degree-bounds)
		      collect (%funcall
				Bp
				(cons (coerce i coef-domain) pts))))))
	(setq *coefficient-domain* (coefficient-domain-of domain))
	(loop for k from 2 to (length vars) do
	  (setq p (SparseInterpStage
		    p
		    Bp
		    (1+ (nth (1- k) degree-bounds))
		    k)))
	p))

(defgeneric list-coerce (list domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod list-coerce ((l list) domain)
  (loop for x in l 
    collect (coerce x domain)))

;; Implementation of the algorithm given in the book by Zippel in
;; the chapter on Multivariate Interpolation.
(defmethod SparseInterpStagei ((coef-domain field)
			       (Pkminus1 mpolynomial) Bp D k
			       &optional ignore)
  (declare (ignore ignore))
  (let* ((domain (domain-of Pkminus1))
	 (vars (list-coerce (list-of-variables Pkminus1) domain))
	 (Skel (pskeleton Pkminus1 vars))
	 (l (length Skel))
	 (S (make-array (list l (1- k)) :initial-contents (reverse Skel)))
	 (Y '())
	 (m '())
	 (cur-pt 0)
	 (X '())
	 (vals '())	 
	 (B '())
	 (EQ (make-array (list D l)))
	 (Pprime '())
	 (factor (one domain))
	 (Pk (zero domain)))
	(setq vals (loop for i below (- (length (ring-variables domain)) k)
			 collect (random coef-domain)))
	(multiple-value-setq (Y m) (InitY coef-domain S k l))
	(loop for i below D do
	  (loop while (member (setq cur-pt (random coef-domain))
			      X))
	  (setq X (append X (list cur-pt)))
	  (setq B (loop for j below l
			collect (%funcall Bp (append
					       (loop for pt in Y
						     collect (expt pt j))
					       (list cur-pt)
					       vals))))
	  (setq Pprime (Solve-vandermondeT m B))
	  (loop for j upfrom 0 as eqn in Pprime do
	    (setf (aref EQ i j) eqn)))
	(loop for j below l do
	  (setq factor (one domain))
	  (loop for k upfrom 0
		as var in (list-of-variables Pkminus1) do
		  (setq factor (* factor (expt (coerce var domain)
					       (aref S j k)))))
	  (setq Pk (+ Pk (* factor
			    (interpolate
			      (coerce (nth (1- k) (ring-variables domain))
				      domain)
			      X
			      (loop for i below D 
				    collect (aref EQ i j)))))))
	Pk))

;;SPMOD-GCD takes two polynomials over the integers and computes their GCD
;;SPMOD-GCD1 takes two polynomials over a field and computes their GCD.

(defun spmod-gcd (p q)
  (let ((b (min (poly-factor-cbound p) (poly-factor-cbound q)))
        (primes (list (newprime))))
    (loop for prod = (first primes) then (* prod (first primes))
          while (> (+ (* 4 b) 2) prod)
          do (push (newprime (first primes)) primes))
    (print b)))

;; Vars is a list var-number and value, so the entries themselves can
;; be used as polynomials representing the variable.
(defun spmod-gcd1 (p q)
  (multiple-value-bind (pc pp) (poly-content-and-prim-part p)
    (multiple-value-bind (qc qp) (poly-content-and-prim-part q)
      (let ((vars (sort (union (poly-list-of-variables pp)
			   (poly-list-of-variables qp))
		    #'cl:<))
	    (plc (lc (poly-terms pp)))
	    (qlc (lc (poly-terms qp)))
	    start-point lc)

	(unless (and (eql (poly-order-number p)
			  (poly-order-number q))
		     (eql (poly-order-number p) (first vars)))
	  (error "Polynomials do not involve the same variables -- SPMOD-GCD"))

    ;; Assuming p and q are primitive

    (setq vars (loop for var on (rest vars)
		     collect (list (first var))))

    (setq lc (poly-gcd plc qlc))

    (setq start-point
	  (loop for var in vars
		collect (list (first var) (random *coefficient-domain*))))

    (poly-times (poly-gcd pc qc)
		(poly-prim-part
		 (spmod-gcd2 lc
			     (poly-times pp (poly-quotient (lc (poly-terms qp)) lc))
			     (poly-times qp (poly-quotient (lc (poly-terms pp)) lc))
			     start-point)))))))

(defun spmod-gcd2 (lc p q vars)
  (cond ((null vars)
	 (poly-times lc (poly-gcdu p q)))
	(t (let ((image (spmod-gcd2 (poly-subst lc (list (first vars)))
				    (poly-subst p (list (first vars)))
				    (poly-subst q (list (first vars)))
				    (rest vars)))
		 (max-terms 0)
		 skel skels rand-vals gcds subst ans) 
	     (map-over-each-term (poly-terms image) (e c)
	       (setq skel (poly-skeleton c (rest vars)))
	       (push (list e skel) skels)
	       (setq max-terms (cl:max max-terms (length skel))))

	     (setq skels (nreverse skels))

	     (push image gcds)
	     (push (second (first vars)) rand-vals)

	     (loop for i below (min (poly-degree p (first vars))
				    (poly-degree q (first vars)))
		   for rand = (random *coefficient-domain*)
		   do (setq subst (list (list (first (first vars)) rand)))
		      (push rand rand-vals)
		      (push (spmod-gcd3 skels
					(poly-subst lc subst)
					(poly-subst p subst)
					(poly-subst q subst)
					(rest vars)
				  max-terms)
			    gcds)
		   #+Testing
		    (print (list 'GCD2 (first subst) (first gcds))))
	     (setq ans 
		   (dense-interpolate-coefficients gcds rand-vals
						   (first vars)))
	     (cond ((and (poly-test-quotient p ans)
			 (poly-test-quotient q ans))
		    ans)		   
		   (t (error "Test divide failed: ~S" ans)))))))

(defun non-zero-random (domain)
  (loop for rand = (random domain)
	do (when (not (0? rand))
	     (return rand))))
		      
(defun spmod-gcd3 (skels lc p q vars max-terms)
  (flet ((check-degree (ans)
	   (when (e> (le ans) (first (first skels)))
	     (error "Degree of skeleton is too large"))
	   (when (e> (first (first skels)) (le ans))
	     (error "Leading coefficient of GCD vanished"))
	   (cons (first p) ans)))
    (if (null vars)
	(check-degree (terms-gcdu+ lc (poly-terms p) (poly-terms q)))
	(let ((rand-vars
	       (loop for (var-num) in vars
		     collect (list var-num
				   (non-zero-random *coefficient-domain*))))
	      subst init-pt gcds coef ans)
	  (setq init-pt (loop for (nil xi) in rand-vars
			      collect xi))

	  (loop for i upfrom 1 below (cl:1+ max-terms) do
	    (setq subst (loop for (var-num xi) in rand-vars
			      collect (list var-num (expt xi i))))
	    (push (terms-gcdu+ (poly-subst lc subst)
			       (poly-terms (poly-subst p subst))
			       (poly-terms (poly-subst q subst)))
		  gcds)
	    #+Testing
	    (print (list 'GCD3 subst (first gcds))))
	  (setq gcds (reverse gcds))
	  (loop for (e skel) in skels
		for vals = ()
		do (loop for g on gcds do
		  (cond ((e> e (le (first g)))
			 (push (zero *coefficient-domain*) vals))
			(t (push (lc (first g)) vals)
			   (setf (first g) (red (first g))))))
		   (setq coef (interpolate-from-skel skel init-pt
							  (reverse vals) vars))
		   (unless (poly-0? coef)
		     (push (cons e coef) ans)))
	  (check-degree (nreverse ans))))))
		
(defun eval-monomial (exps vals)
  (let ((ans (expt (first vals) (first exps))))
    (loop for e in (rest exps)
	  for val in (rest vals)
	  do (setq ans (* ans (expt val e))))
    ans))

(defun interpolate-from-skel (skel init-pt vals vars)
  (let* ((pts (loop for term in skel
		    collect (eval-monomial term init-pt)))
	 (coefs (solve-vandermondetd pts vals))
	 (ans (zero *coefficient-domain*))
	 (one (one *coefficient-domain*)))
    (labels ((make-monomial (c exp vars)
	       (cond ((null exp)
		      c)
		     ((null vars)
		      (error "Not enough variables"))
		     ((e0? (first exp))
		      (make-monomial c (rest exp) (rest vars)))
		     (t (poly-times (poly-simp (first vars)
					       (make-terms (first exp) one))
				    (make-monomial c (rest exp) (rest vars)))))))
      (loop for exps in skel
	    for c in coefs
	    do (unless (poly-0? c)
		 (setq ans (poly-plus ans (make-monomial c exps vars)))))
      ans)))


(defun dense-interpolate-coefficients (poly-list vals var)
  (with-linear-support
    (let ((ans zero) this-var terms-list degree)
      (loop for poly in poly-list do
	(cond ((poly-coef? poly))
	      ((or (null this-var)
		   (more-main? poly this-var))
	       (setq this-var poly))))
      (cond ((null this-var)    
	     ;; Everything is constant at this point, so we can do the dense
	     ;; interpolation and return
	     (poly-interpolate var vals poly-list))
	    (t 
	     ;; Find the maximum degree that appears and turn everything into a 
	     ;; list of terms
	     (loop for poly in poly-list
		   do (push (cond ((poly-coef? poly)
				   (make-terms (e0) poly))
				  ((null degree)
				   (setq degree (le (poly-terms poly)))
				   (setq this-var poly)
				   (poly-terms poly))
				  ((same-variable? this-var poly)
				   (setq degree (max degree (le (poly-terms poly))))
				   (poly-terms poly)))
			    terms-list))
	     (setq terms-list (reverse terms-list))

	     ;; Now work down the list of terms and recurse
	     (loop for deg downfrom degree
		   with sub-polys
		   while (not (eminus? deg))
		   do (setq sub-polys nil) 
		      (loop for terms on terms-list do
			(cond ((null (first terms))
			       (push zero sub-polys))
			      ((e= (le (first terms)) deg)
			       (push (lc (first terms)) sub-polys)
			       (setf (first terms) (red (first terms))))
			      (t (push zero sub-polys))))
		      (setq ans (poly-plus
				 (poly-times (dense-interpolate-coefficients
					      (reverse sub-polys) vals var)
					     (poly-simp this-var (make-terms deg one)))
				     ans)))
	     ans)))))

    
;;; GCD of univariate polynomials over a finite field.
(defun poly-gcdu (p q)
  (cond ((or (poly-coef? p) (poly-coef? q))
	 (one *coefficient-domain*))
	((same-variable? p q)
	 (poly-simp p (terms-gcdu (poly-terms p) (poly-terms q))))
	(t (error "Must be same main variable"))))

(defun terms-gcdu (pt qt)
  (do ()
      ((terms0? qt)
       (if (e0? (le pt))
	   (make-terms (e0) (one *coefficient-domain*))
	   (terms-monicize pt)))
    (psetq pt qt
	   qt (terms-pseudo-remainder pt qt))))

(defun terms-gcdu+ (lc pt qt)
  (terms-mon-times (terms-gcdu pt qt) (e0) lc))

;;;  Testing functions

#+Testing
(defun random-poly (domain degree vars)
  (labels ((random-poly1 (vars)
	     (if (null vars) (random domain)
		 (poly-simp vars
		   (loop for i downfrom degree
			 with c
			 while (not (cl:minusp i))
			 when (and (cl:zerop (cl:random 2))
				   (not (poly-0?
					 (setq c (random-poly1 (rest vars))))))
			   collect (cons i c))))))
    (random-poly1 vars)))

#+Testing
(defun random-poly (degree vars terms)
  (let ((one (one *coefficient-domain*))
	(ans (zero *coefficient-domain*)))
    (labels ((random-monomial (vars)
	       (if (null vars) (one *coefficient-domain*)
		                ;; (random *coefficient-domain*)
		   (poly-times
		    (random-monomial (rest vars))
		    (poly-simp vars
			       (make-terms (cl:random degree) one))))))
      (loop for i below terms
	    do (setq ans (poly-plus ans (random-monomial vars))))
      ans)))

#+Testing
(defun initialize-pq (n deg terms)
  (let ((vars (loop for i below n collect i)))
    (flet ((rand-poly ()
	     (random-poly deg vars terms)
	     #+Ignore
	     (poly-plus `(0 (,(+ 1 deg) . ,(one *coefficient-domain*)))
			(random-poly deg vars terms))))
      (setq g (rand-poly))
      (setq p (poly-times g (rand-poly)))
      (setq q (poly-times g (rand-poly))))))


;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Expanded Polynomials
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; grobner.lisp,v 1.8 1995/05/24 17:42:02 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.8")

(defmethod initialize-instance :after ((id ideal) &rest ignore)
  (declare (ignore ignore))
  (let ((ring (ring-of id)))
    (with-slots (coefficient-domain print-function) id
      (unless coefficient-domain
	(setf coefficient-domain (ring-of id)))

      (setf print-function 'ideal-print-object))
    (unless (super-domains-of id)
      (setf (super-domains-of id) (cons ring (super-domains-of ring))))))

(defun ideal-print-object (id stream)
  (let ((gens (generators-of id)))
    (format stream "#Id(~S~{, ~S~})" (first gens) (rest gens))))

(defgeneric make-ideal (ring &rest generators)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-ideal ((ring ring) &rest generators)
  (make-instance 'ideal
		 :ring ring
		 :generators (loop for g in generators
				   collect (coerce g ring))))

(defmethod make-ideal ((ring field) &rest generators)
  (declare (ignore generators))
  (make-instance 'ideal :ring ring :generators (list (one ring))))

;;FIXTHIS:  The following assumes that PID are GCD domains, which isn't true. 
(defmethod make-ideal ((ring rational-integers) &rest generators)
  (make-instance 'PID-ideal :ring ring
		 :generators (list
			       (loop with g = (coerce (first generators) ring)
				     for e in (rest generators)
				     do (setq g (gcd g (coerce e ring)))
					finally (return g)))))

(defmethod reduce-basis ((id PID-ideal))
  id)

(defmethod plus ((id1 ideal) (id2 ideal))
  (cond ((and (eql (ring-of id1) (ring-of id2))
	      (eql (coefficient-domain-of id1) (coefficient-domain-of id2)))
	 (apply #'make-ideal (ring-of id1)
		(append (generators-of id1) (generators-of id2))))
	(t (call-next-method))))

(defmethod times ((id1 ideal) (id2 ideal))
  (cond ((and (eql (ring-of id1) (ring-of id2))
	      (eql (coefficient-domain-of id1) (coefficient-domain-of id2)))
	 (apply #'make-ideal (ring-of id1)
		(loop for e1 in (generators-of id1)
		      append (loop for e2 in (generators-of id2)
				   collect (* e1 e2)))))
	(t (call-next-method))))

(defmethod binary= ((id1 ideal) (id2 ideal))
  (or (eql id1 id2)
      (loop with id2-gen = (generators-of id2)
	    for p in (generators-of id1)
	    do (unless (member p id2-gen :test #'binary=)
		 (return nil))
	    finally (return t))))
		     

(defmacro with-grobner-operations (grobner-basis &body body)
  `(with-slots (greater-function ring generators undones reducibles possibles)
	       ,grobner-basis
     (let ((dim (cl:1+ (length (ring-variables ring)))))
       (macrolet ((e> (a b) `(%funcall greater-function ,a ,b))
		  (e< (a b) `(%funcall greater-function ,b ,a)))
	 ,@body))))

;; Grobner calculations are done within the context of an instance of
;; the Grobner-Basis flavor.  Each instance has its own variable list
;; and flags sets.  At any time the user can add polynomials or
;; extract information from the structure.

#|  ;; The following is actually in algebraic-domains.lisp |
(defclass grobner-basis (ideal has-comparison)
  (;; The exponent comparison function is managed by HAS-COMPARISON

   (undones :initform ())
   ;; A list of triples pairs (lt f g), lt(f)<=lt(g), of elements of
   ;; GENERATORS such that if any pair is not in the list, its s-poly
   ;; is guaranteed to be writable as a linear combination of
   ;; GENERATORS, with smaller s-degs
   
   (reducibles :initform nil :accessor reducibles-of)
   (possibles :initform nil)	
   ))
||#

(defmethod initialize-instance :after ((gb grobner-basis) &rest ignore)
  (declare (ignore ignore))
  (with-slots (greater-function ring) gb
    (setq greater-function 
	  (get-comparison-fun (length (ring-variables ring))
			      greater-function))))

(defun check-same-domain (exprs)
  (let ((domain (domain-of (first exprs))))
    (loop for exp in (rest exprs)
	  do (unless (eql domain (domain-of exp))
	       (return nil))
	  finally (return domain))))


(defmethod make-ideal ((ring polynomial-ring) &rest polys)
  (let (ideal)
    (cond ((field? (coefficient-domain-of ring))
	   (setq ideal (make-instance 'grobner-basis :ring ring
				      :greater-function :lexical)))
	  (t (error "Can't deal with polynomials not over fields: ~S"
		    ring))) 
    (loop for p in polys
	  do (add-relation ideal (coerce p ring)))
    ideal))

(defmethod (setf greater-function) (new-function (grob grobner-basis))
  (with-slots (ring greater-function generators reducibles possibles) grob
    (unless (eql greater-function new-function) 
      (flet ((convert-list (list) 
	       (loop for poly in list 
		     collect (sort poly new-function))))
	(unless (functionp new-function)
	  (setq new-function
		(get-comparison-fun (length (ring-variables ring))
				    new-function)))
	(setq generators (convert-list generators))
	(setq reducibles (convert-list reducibles))
	(setq possibles (convert-list possibles))
	(setq greater-function new-function)))
    grob))

(defgeneric add-relation (basis poly)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod add-relation ((grob-struct grobner-basis) (relation mpolynomial))
  (let ((ring (ring-of grob-struct)))
    (if (not (eql ring (domain-of relation)))
	(add-relation grob-struct (coerce relation ring)))
    (let ((poly (make-epolynomial ring (greater-function-of grob-struct)
				  relation)))
      (push (poly-form poly) (reducibles-of grob-struct))
      poly)))

(defmethod add-relation ((grob-struct grobner-basis) (relation epolynomial))
  (let ((ring (ring-of grob-struct)))
    (if (not (eql ring (domain-of relation)))
	(add-relation grob-struct (coerce relation ring)))
    (let ((poly (make-epolynomial ring (greater-function-of grob-struct)
				  relation)))
      (push (poly-form poly) (reducibles-of grob-struct))
      poly)))

(defmethod generators-of ((grob-struct grobner-basis))
  (with-slots (generators reducibles greater-function ring) grob-struct
    (append
      (loop for g in generators
	    collect (make-instance 'epolynomial
				   :domain ring
				   :greater-function greater-function
				   :form g))
      (loop for g in reducibles
	    collect (make-instance 'epolynomial
				   :domain ring
				   :greater-function greater-function
				   :form g)))))

(defmethod reset-grobner-basis ((grob-struct grobner-basis))
  (with-slots (generators undones possibles reducibles) grob-struct
    (setq generators nil undones nil
	  possibles nil reducibles nil)))

#+Ignore
(defun terms-s-poly (greater-function terms1 terms2)
  (let ((m (max (le terms1) (le terms2))))
    (gterms-difference greater-function
     (gterms-mon-times terms1 (- m (le terms1)) (lc terms2))
     (gterms-mon-times terms2 (- m (le terms2)) (lc terms1)))))

;; The following saves a bunch of consing, but not as much as I would expect
(defun terms-s-poly (greater-function terms1 terms2)
  #+Lucid
  (declare (optimize (safety 0)))
  (let* ((dim (length (first terms1))) 
	 (m (gterm-lcm (lt terms1) (lt terms2) dim))
	 (ans-terms (list nil))
	 (terms ans-terms)
	 (x (red terms1))
	 (y (red terms2))
	 (xe (gterm-quot m (lt terms1) dim))
	 (xc (svref (lt terms2) 0))
	 (ye (gterm-quot m (lt terms2) dim))
	 (yc (- (svref (lt terms1) 0)))
	 temp sum new-xe new-ye)
    (loop
      (cond ((terms0? x)
	     (cond ((terms0? y) (return (rest ans-terms)))
		   (t (setq temp (gterm-times ye (lt y) dim))
		      (setf (svref temp 0) (* yc (svref (lt y) 0)))
		      (setf (rest terms) (list temp))
		      (setf terms (rest terms))
		      (setq y (red y)))))
	    ((or (terms0? y)
		 (%funcall greater-function
			   (setq new-xe (gterm-times xe (lt x) dim))
			   (setq new-ye (gterm-times ye (lt y) dim))))
	     (setq temp (gterm-times xe (lt x) dim))
	     (setf (svref temp 0) (* xc (svref (lt x) 0)))
	     (setf (rest terms) (list temp))
	     (setf terms (rest terms))
	     (setq x (red x)))
	    ((%funcall greater-function new-ye new-xe)
	     (setf (svref new-ye 0) (* yc (svref (lt y) 0)))
	     (setf (rest terms) (list new-ye))
	     (setf terms (rest terms))
	     (setq y (red y)))
	    (t (setq sum (+ (* xc (svref (lt x) 0))
			    (* yc (svref (lt y) 0))))
	       (unless (0? sum)
		 (setf (svref new-xe 0) sum)
		 (setf (rest terms) (list new-xe))
		 (setf terms (rest terms)))
	       (setq x (red x) y (red y)))))))

(defmethod reduce-basis ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (flet ((criterion1 (degree f1 f2)
	     (loop for p in generators do
	       (when (and (not (eql p f1))
			  (not (eql p f2))
			  (gterm-dominates degree (lt p) dim))
		 (unless (member nil undones
				 :test
				 #'(lambda (x prod)
				     (declare (ignore x))
				     (let ((b1 (second prod))
					   (b2 (third prod)))
					  (or (and (eql f1 b1) (eql p b2))
					      (and (eql f1 b2) (eql p b1))
					      (and (eql p b1) (eql f2 b2))
					      (and (eql p b2) (eql f2 b1))))))
		   (return-from criterion1 t))))))
      (let (temp f1 f2 h)
	(reduce-all grob-struct)
	(new-basis grob-struct)
      	(loop while undones do
          (setq temp (pop undones))
	  (setq f1 (second temp) f2 (third temp))
	  (when (and (null (criterion1 (first temp) f1 f2))
		     (not (gterm-disjoint (lt f1) (lt f2) dim)))
	    (setq h (terms-reduce greater-function
				  (gterms-prim*
				   (terms-s-poly greater-function f1 f2))
				  generators))
	    (when (not (terms0? h))
	      (setq reducibles nil)
	      (setq possibles (list h))
	      (setq generators
		    (loop for g in generators
			  when (gterm-dominates (lt g) (lt h) dim)
			    do (push g reducibles)
			  else collect g))
	      (setq undones
		    (loop for undone in undones
			  unless (or (member (second undone) reducibles)
				     (member (third undone) reducibles))
			    collect undone)) 
	      (reduce-all grob-struct)
	      (new-basis grob-struct)))))))
  grob-struct)

(defgeneric reduce-all (basis)
  (:documentation
   "The purpose of this function is unknown."))

;; This makes sure that all of the polynomials in generators and
;; possibles are AUTOREDUCED.
(defmethod reduce-all ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (let (h g0)
      (loop while (not (null reducibles)) do
        (setq h (terms-reduce greater-function
			      (pop reducibles)
			      (append generators possibles)))
	(unless (terms0? h)
	  (setq generators (loop for elt in generators 
				 when (gterm-dominates (lt elt) (lt h) dim)
				   do (push elt reducibles)
				      (push elt g0)
				 else collect elt))
	  (setq possibles (loop for elt in possibles
				when (gterm-dominates (lt elt) (lt h) dim)
				  do (push elt reducibles)
				else collect elt))
	  (setq undones (loop for (nil f1 f2) in undones
			      when (and (not (member f1 g0))
					(not (member f2 g0)))
				collect (list (gterm-lcm (lt f1) (lt f2) dim)
					      f1 f2)))
	  (push h possibles))))))

(defgeneric new-basis (basis)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod new-basis ((grob-struct grobner-basis))
  (with-grobner-operations grob-struct
    (flet ((add-undone (f g)
	     (when (e> (lt f) (lt g))
	       (rotatef f g))
	     (loop for (nil ff gg) in undones
		   do (when (and (eql ff f) (eq gg g))
			(return t))
		   finally (push (list (gterm-lcm (lt f) (lt g) dim) f g)
				 undones))))
      (setq generators (append generators possibles))
      (loop for g in generators do
	(loop for elt in possibles do
	  (when (not (eql elt g))
	    (add-undone elt g))))
      (setq possibles nil)
      (setq undones (sort undones #'(lambda (a b) (e< (first a) (first b)))))
      #+ignore
      (setq generators
	    (loop for g in generators
		  for h = (terms-reduce greater-function g (remove g generators))
		  when (not (terms0? h))
		    collect h)))))

;; Reduce terms modulo the current basis
(defun terms-reduce (greater-function terms basis)
  (let ((dim (length (first terms))))
    #+ignore
    (format t "~&~%Poly = ~S~%Basis: "
	    (le terms))
    #+ignore
    (princ (mapcar #'(lambda (f) (le f)) basis))
    (let ((again t))
      (loop while again do
	(when (terms0? terms)
	  (return nil))
	#+ignore
	(format t "~&Terms = ~S"
		(make-instance 'epolynomial
			       :domain (slot-value grob-struct 'ring)
			       :greater-function greater-function
			       :form terms))
	(loop for g in basis
	      do (when (gterm-dominates (lt terms) (lt g) dim)
		   (setq terms (gterms-prim*
				 (terms-s-poly greater-function terms g)))
		   (return t))
	      finally (setq again nil))))
    #+ignore
    (format t "~&Result = ~S~%" (le terms))
    terms))

;; Make poly primitive.  
;; This isn't really well defined since coefs are in a field.  Idea is
;; to make the coefficients smaller.  Its really worth avoiding
;; dividing out a content of 1!!!
#+ignore  ;; Use for integral domains
(defun gterms-prim* (poly) 
  (unless (terms0? poly)
    (let ((coef-domain (domain-of (lc poly)))
	  (num-gcd (numerator (lc poly)))
	  (den-gcd (denominator (lc poly)))
	  1/content)
      ;; Should really use a probabilistic algorithm content algorithm
      ;; here
      (map-over-each-term (red poly) (nil c)
	(if (1? num-gcd)
	    (if (1? den-gcd) (return t)
		(setq den-gcd (gcd den-gcd (denominator c))))
	    (if (1? den-gcd)
		(setq num-gcd (gcd num-gcd (numerator c)))
		(setq num-gcd (gcd num-gcd (numerator c))
		      den-gcd (gcd den-gcd (denominator c))))))
      (unless (and (1? num-gcd) (1? den-gcd))
	(setq 1/content (make-quotient-element coef-domain den-gcd num-gcd))
	(map-over-each-term poly (e c)
	  (update-term e (* c 1/content))))))
  poly)

;; Use for fields
(defun gterms-prim* (poly)
  (unless (terms0? poly)
    (let ((1/lc (/ (svref (lt poly) 0))))
      (unless (1? 1/lc)
	(loop for term in poly
	      do (setf (svref term 0) (* (svref term 0) 1/lc))))))
  poly)
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		       Truncated Power Series Domain
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.16")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator tpower-series-ring
      ((coefficient-domain domain) variable)
    (let* ((field? (field? coefficient-domain))
           (domain
            (make-instance (if field? 'tpower-series-field
                               'tpower-series-ring)
                           :variables (coerce variable *general*)
                           :coefficient-domain coefficient-domain
                           :print-function (if field? 'tp-field-print-object
                                               'tp-ring-print-object))))
      (make-homomorphism coefficient-domain
                         #'(lambda (c) (make-tpower-series domain c))
                         domain)
      domain)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-element-classes tpower-series-ring
    tpower-series))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-element-classes tpower-series-field
    tpower-series))

(defun tp-field-print-object (d stream)
  (with-slots (coefficient-domain) d
    (format stream "~A((" coefficient-domain)
    (display (ring-variables d) stream)
    (princ "))" stream)))

(defun tp-ring-print-object (d stream)
  (with-slots (coefficient-domain) d
    (format stream "~A[[" coefficient-domain)
    (display (ring-variables d) stream)
    (princ "]]" stream)))

;; GET-TPOWER-SERIES-DOMAIN
;;
;; If the coefficient domain is a ring, then construct a TPS which is
;; also a ring.  If the coefficient domain is a field, construct a TPS
;; which is also a field.
(defun get-tpower-series-domain (cdom variable)
  (let ((gvariable (coerce variable *general*)))
    (cond ((ring? cdom)
	   (add-domain #'(lambda (d)
			   (and (typep d 'tpower-series-ring)
				(eql (coefficient-domain-of d) cdom)
				(equal (ring-variables d) (list gvariable))))
	     (make-tpower-series-ring* cdom gvariable)))
	  ((field? cdom)
	   (add-domain #'(lambda (d)
			   (and (typep d 'tpower-series-ring)
				(eql (coefficient-domain-of d) cdom)
				(equal (ring-variables d) (list gvariable))))
	     (make-tpower-series-ring* cdom gvariable)))
	  (t (error "~S must be at least a ring" cdom)))))

(defgeneric make-tpower-series (domain series &key &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

;; This is a copy constructor.  It creates a new TPS like the one given as
;; a parameter with modifications as indicated by keyword parameters
(defmethod make-tpower-series ((domain tpower-series-domain) (tp tpower-series)
			       &key (valence (valence tp))
			            (order (order tp))
			            (branch-order (branch-order tp))
			            (coeffs (coeffs tp))
                                    &allow-other-keys)
  (make-instance 'tpower-series
		 :domain domain
		 :valence valence
		 :order (max valence order)
		 :branch-order branch-order
		 :coeffs coeffs))

;; Construct a TPS.  Assume the 2nd parameter is a constant and can
;; be coerced into the coefficient domain
(defmethod make-tpower-series ((domain tpower-series-domain) coef
			       &rest options &key &allow-other-keys)
  (apply #'make-instance 'tpower-series
		 :domain domain
		 :coeffs (vector (coerce coef (coefficient-domain-of domain)))
		 options))

;; Construct a zero valence unit branching order TPS from a list
;; of coefficients.
(defmethod make-tpower-series ((domain tpower-series-domain) (plist list)
			       &rest options &key &allow-other-keys)
  (let ((coef-domain (coefficient-domain-of domain)))
    (apply #'make-tpower-series
	   domain
	   (map 'array #'(lambda (e) (coerce e coef-domain))
		plist)
	   options)))

(defun trim-zeroes (coeffs)
  (let ((lead 0)
	(trail (length coeffs)))
    (loop for i fixnum below trail
	  do (if (0? (svref coeffs i)) (incf lead)
		 (return t)))
    #+ignore
    (loop for i fixnum downfrom trail above (1- lead)
	  do (if (0? (svref coeffs i)) (decf trail)
		 (return t))) 
    (if (cl:= lead trail) (values (vector (svref coeffs 0)) nil)
	(loop with vect = (make-array (cl:- trail lead))
	      for i upfrom lead below trail
	      do (setf (svref vect (cl:- i lead)) (svref coeffs i))
	      finally (return (values vect lead))))))

;; The argument is an array then, the we assume the arguments are
;; already coerced into the proper domain.
(defmethod make-tpower-series ((domain tpower-series-domain) (terms array) 
                                &key (valence 0) (order *positive-infinity*)
			       (branch-order 1))
  (multiple-value-bind (ncoeffs shift) (trim-zeroes terms)
    (make-instance 'tpower-series
		   :domain domain :coeffs ncoeffs
		   :valence (if shift (cl:+ valence shift) 0)
		   :order order :branch-order branch-order)))

(defmethod initialize-instance :after ((d power-series-domain)
				       &rest plist)
  (declare (ignore plist))
  (with-slots (zero one coefficient-domain) d
    (setq zero (make-tpower-series d (zero coefficient-domain)))
    (setq one (make-tpower-series d (one coefficient-domain)))))

;;
;; Printing functions

;; PRINT-GROUPED
;; Print n to the stream and parenthesize it if is contains any non-
;; alphanumeric characters.  This seem to be a good heuristic for a
;; human comprehensible output form.
(defun print-grouped (n stream)
  (let ((str (format nil "~A" n)))
    (if (or (every #'alphanumericp str)
	    (grouped? str #\( #\) )
	    (grouped? str #\[ #\] )
	    (grouped? str #\{ #\} ))
	(princ str stream)
	(format stream "(~A)" str))))

(defun grouped? (str a b)
  (let ((last (- (length str) 1)))
  (and (char= (char str 0) a )
       (char= (char str (- (length str) 1)) b)
       (loop for i from 1 to (- last 1)
	     with depth = 1
	     do (cond ((char= (char str i) a) (incf depth))
		      ((char= (char str i) b) (decf depth))
		      (t))
		never (= depth 0)))))
		     

(defun print-exponent (e br stream)
  (if (not (1? (/ e br)))
      (progn
	(princ "^" stream)
	(print-grouped (/ e br) stream))))
        
(defun print-tpower-series (var tp stream)
  (labels ((print-term (e c)
		       (cond ((0? c)
			      (print-object c stream))
			     ((0? e)
			      (print-object c stream))
			     (t
			       (if (not (1? c))
				   (print-grouped c stream))
			       #+Genera
			       (format stream "~'i~A~" var)
			       #-Genera
			       (display var stream)
			       (print-exponent e (branch-order tp) stream)))))
	  (progn
	    (print-term (valence tp) (aref (coeffs tp) 0))
	    (loop for exp from (+ (valence tp) 1)
		  with coef
		  until (>= (- exp (valence tp))
				   (array-dimension (coeffs tp) 0)) do
				     (setq coef (aref (coeffs tp)
							  (- exp (valence tp))))
					 (cond ((0? coef) nil)
					       ((minus? coef) (princ " - " stream)
						(print-term exp (minus coef)))
					       (t (princ " + " stream)
						  (print-term exp coef))))
	    (if (/= (order tp) *positive-infinity*)
		(progn
		  (princ " + o(" stream)
		  (print-term (order tp) 1)
		  (princ ")" stream ) )))))

(defmethod print-object ((p tpower-series) stream)
  (print-tpower-series
     (ring-variables (domain-of p))
     p
     stream))


;; SPREAD-COEFFS
;;
;; Construct a list of coefficients from a TPS but place (b-1) zeros
;; in between each coefficient of the given TPS.  This is used to
;; construct a new TPS with a larger branching order from an old TPS.
(defun tps-spread-coeffs (coeffs b)
  (let* ((len (length coeffs))
	 (zed (zero (domain-of (aref coeffs 0))))
	 (rval (make-array (cl:+ (cl:* b (cl:- len 1)) 1))))
    (loop for i fixnum from 0 below len
	  do (setf (aref rval (cl:* b i)) (aref coeffs i))
	     (if (cl:< (1+ i) len)
		 (loop for j fixnum from 1 to b
		       do (setf (aref rval (cl:+ (cl:* b i) j)) zed)))
	  finally (return rval))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tps-var-val (v)
    (intern (format nil "~A-VAL" v) (symbol-package v)))
  
  (defun tps-var-bo (v)
    (intern (format nil "~A-BO" v) (symbol-package v)))

  (defun tps-var-order (v)
    (intern (format nil "~A-ORDER" v) (symbol-package v)))

  (defun tps-var-coeffs (v)
    (intern (format nil "~A-COEFFS" v) (symbol-package v))))

(defmacro with-tpower-series (vars-and-tps &body body)
  (let ((decls nil)
	(new-body body))
    (loop for (form . rest) on body
	  while (and (not (atom form))
		     (eql (first form) 'declare))
	  do (setq decls (append decls (rest form)))
	     (setq new-body rest))
  (setq body new-body)
  `(let (,@(loop for (var tps) in vars-and-tps 
		 append `((,(tps-var-val var) (valence ,tps))
			  (,(tps-var-bo var) (branch-order ,tps))
			  (,(tps-var-order var) (order ,tps))
			  (,(tps-var-coeffs var) (coeffs ,tps)))))
     ,@(when decls `((declare ,@decls)))
     (macrolet ((set-branch-order (var ord)
		  `(progn
		     (setf ,(tps-var-coeffs var)
			   (tps-spread-coeffs ,(tps-var-coeffs var) ,ord))
		     (setf ,(tps-var-order var)
			   (cl:* ,(tps-var-order var) ,ord))
		     (setf ,(tps-var-val var) (cl:* ,(tps-var-val var) ,ord))
		     (setf ,(tps-var-bo var) (cl:* ,(tps-var-bo var) ,ord))))
		(tps-rotatef (x y)
		  `(progn
		     (rotatef ,(tps-var-val x) ,(tps-var-val y))
		     (rotatef ,(tps-var-bo x) ,(tps-var-bo y))
		     (rotatef ,(tps-var-order x) ,(tps-var-order y))
		     (rotatef ,(tps-var-coeffs x) ,(tps-var-coeffs y)))))
       ,@body))))


;; MODIFY-BRANCH-ORDER

(defgeneric modify-branch-order (series factor)
  (:documentation
   "The purpose of this function is unknown."))
;;
;; Create a new TPS equivalent to the given TPS but with a branchorder
;; larger by a factor.  This is similar to TEMP-MODIFY-BR but the result
;; is a "genuine" TPOWER-SERIES whose use need not be temporary.
(defmethod modify-branch-order ((a tpower-series) (factor integer))
  (if (1? factor)
      a
      (make-tpower-series (domain-of a)
			  (tps-spread-coeffs (coeffs a) factor)
			  :valence (* factor (valence a))
			  :order (* factor (order a))
			  :branch-order (* factor (branch-order a)))))

(defmethod set-branch-order ((a tpower-series) (bo integer))  
  (cond ((cl:= (branch-order a) bo) 
	 a)
	((zerop (rem bo (branch-order a)))
	 (modify-branch-order a (cl:/ bo (branch-order a))))
	(t (error "New branch order must a multiple of old: ~S does not divide ~S"
		  (branch-order a) bo))))

(defsetf branch-order set-branch-order)

;; MAP-TPOWER-SERIES
;;
;; Create a new TPS by mapping a function onto all the coefficient of
;; an existing TPS.  This is convenient for computing minus.
(defun map-tpower-series (tp f)
  (make-tpower-series (domain-of tp) tp
		      :coeffs (map 'array f (coeffs tp))))

;; Coercions 
(defmethod coerce (elt (domain tpower-series-domain))
  (let ((value (coercible? elt (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-tpower-series domain (vector value)))
	  (t (call-next-method)))))

(defmethod coerce ((exp symbol) (domain tpower-series-domain))
  (coerce (coerce exp *general*) domain))

(defmethod coerce ((exp list) (domain tpower-series-domain))
  (coerce (coerce exp *general*) domain))

;; INCOMPLETE!!!
#+ignore
(defmethod coerce ((p tpower-series) (d general-expressions))
  )

(defmethod coerce ((exp general-expression) (domain tpower-series-domain)) 
  (with-slots (variables) domain 
    (cond ((ge-equal exp variables)
	   (make-tpower-series domain
			       (vector (zero (coefficient-domain-of domain))
				       (one (coefficient-domain-of domain)))))
	  ((and (ge-atom? exp)
		(let ((var (coercible? exp (coefficient-domain-of domain))))
		  (and var  (make-tpower-series domain (vector var))))))
	  ((ge-plus? exp)
	   (let ((sum (zero domain)))
	     (loop for x in (terms-of exp)
		   do (setq sum (+ sum (coerce x domain))))
	     sum))
	  ((ge-times? exp)
	   (let ((prod (one domain)))
	     (loop for x in (terms-of exp)
		   do (setq prod (* prod (coerce x domain))))
	     prod))
	  ((and (ge-expt? exp)
		(integer? (exponent-of exp)))
	   (expt (coerce (base-of exp) domain) (exponent-of exp)))
	  (t (coerce exp (coefficient-domain-of domain))))))

;; Polynomial coercions
;;
;; Since the TPS can represent any polynomial, there is an obvious mapping
;; from a polynomial domain to the power series domain when the variables
;; of the domains are the same.
;;
;; The coercion is successful only for upolynomials and univariate
;; mpolyomials
(defmethod coerce ((a upolynomial) (domain tpower-series-domain))
  (if (coercible? a (coefficient-domain-of domain)) (call-next-method)  
      (if (not
	    (equal (ring-variables (domain-of a)) (list (ring-variables domain))))
	  (error "Can't coerce element of ~A into domain ~A." (domain-of a)
		 domain)
	  (make-instance 'tpower-series :domain domain
			 :coeffs (copy-seq (poly-form a))))))

(defmethod coerce ((a mpolynomial) (domain tpower-series-domain))
  (if (coercible? a (coefficient-domain-of domain)) (call-next-method)
      (if (not
	    (equal (ring-variables (domain-of a)) (list (ring-variables domain))))
	  (error "Can't coerce element of ~A into domain ~A." (domain-of a)
		 domain)
	  (let* ((ord (clist-degree (poly-form a)))
		 (val (reduce #'(lambda (a b) (min (car a) (car b)))
			      (poly-terms (poly-form a))))
		 (arr (make-array (+ (- ord val) 1) :initial-element
				  (zero (coefficient-domain-of domain)))))
		(map 'array #'(lambda (a)
				      (setf (aref arr (- (car a) val)) (cdr a)))
		     (poly-terms (poly-form a)))
		(make-instance 'tpower-series :domain domain
			       :coeffs arr
			       :valence val)))))

;; LARGEST-STORED-COEFF
;;
;; Computes the numerator of the largest exponent for which a coefficient
;; is stored in (coeffs tp).
(defmethod largest-stored-coeff ((tp tpower-series))
  (+ (valence tp)
     (- (array-dimension (coeffs tp) 0) 1)))

;; TRUNCATE-ORDER
;;
;; Truncates the order of a TPS to an integer.  All coefficients with
;; exponents greater than the new order are discarded.
(defmethod truncate-order ((tp tpower-series) (torder integer))
  (let ((mtorder (* (branch-order tp) torder)))
    (cond ((< mtorder (valence tp))
	   (make-tpower-series (domain-of tp) tp
			       :valence mtorder
			       :order mtorder
			       :coeffs (vector (zero (coefficient-domain-of
						       (domain-of tp))))))
	  ((>= mtorder (order tp)) tp)
	  ((>= (+ 1 (- mtorder (valence tp)))
	       (array-dimension (coeffs tp) 0))
	   (make-tpower-series (domain-of tp) tp
			       :order mtorder))
	  (t (make-tpower-series (domain-of tp) tp
				 :order mtorder
				 :coeffs (subseq (coeffs tp)
						 0
						 (+ 1 (- mtorder (valence tp)))))))))

#+ignore
(defsetf order truncate-order)


;; unary MINUS
;;
(defmethod minus ((tp tpower-series))
  (map-tpower-series tp #'minus))

;; PLUS
;; 
;; Computes the sum of two TPS.
;; General procedure
;;    1) make branching orders equal
;;    2) compute coefficients
;;    3) simplify out zeros
;;    4) package result
(defmethod-sd plus ((a tpower-series) (b tpower-series))
  (with-tpower-series ((a a) (b b))
    (let* ((bo (lcm a-bo b-bo))
	   (zero (zero (coefficient-domain-of domain)))
	    a-lim b-lim valence order coeffs com)

      (unless (= a-bo b-bo)
	(set-branch-order a (/ bo a-bo))
	(set-branch-order b (/ bo b-bo))) 

      (setq valence (cl:min a-val b-val))
      (setq order (min a-order b-order))

      (when (< b-val a-val)
	(tps-rotatef a b))

      (setq com (cl:- b-val a-val))
      
      (setq a-lim (length a-coeffs)
	    b-lim (cl:+ com (length b-coeffs)))

      (setq coeffs (make-array
		     (if (integerp order) (cl:- order valence -1)
			 (max (+ a-val (length a-coeffs))
			      (+ b-val (length b-coeffs))))
		     :initial-element zero))

      (loop for i fixnum below com
	    do (setf (svref coeffs i)
		     (if (cl:< i a-lim) (svref a-coeffs i) zero)))

      (loop for i upfrom com below (array-dimension coeffs 0)
	    do (setf (svref coeffs i)
		     (+ (if (cl:< i a-lim) (svref a-coeffs i) zero)
			(if (cl:< i b-lim) (svref b-coeffs (cl:- i com))
			    zero))))
      (make-tpower-series domain coeffs
			  :valence valence :order order :branch-order bo))))

;; DIFFERENCE
;;
;; Use PLUS
(defmethod-sd difference ((a tpower-series) (b tpower-series))
  (plus a (minus b)))

;; TIMES
;; 
;; Computes the product of two TPS.
;; General procedure
;;    1) make branching orders equal
;;    2) compute coefficients
;;    3) simplify out zeros
;;    4) package result
(defmethod-sd times ((a tpower-series) (b tpower-series))
  (with-tpower-series ((a a) (b b))
    (let* ((bo (lcm a-bo b-bo)) 
	   (zero (zero (coefficient-domain-of domain)))
	   valence order coeffs)

      (unless (= a-bo b-bo)
	(set-branch-order a (/ bo a-bo))
	(set-branch-order b (/ bo b-bo)))

      (setq valence (cl:+ a-val b-val))
      (setq order (cl:+ (min (cl:- a-order a-val)
                             (cl:- b-order b-val))
                        valence))
            
      (setq coeffs (make-array
		     (if (integerp order) (cl:- order valence -1)
			 (cl:+ (length a-coeffs) (length b-coeffs) -1))
		     :initial-element zero))

      (loop with n-terms = (array-dimension coeffs 0)
            for i below n-terms
	    with a-lim = (min (length a-coeffs) n-terms)
            and b-lim = (min (length b-coeffs) n-terms)
	    do (loop for j below (1+ i)
                     do (when (and (cl:< j a-lim)
                                   (cl:< (cl:- i j) b-lim))
                          (setf (svref coeffs i)
		                (+ (svref coeffs i)
                                   (* (svref a-coeffs j)
                                      (svref b-coeffs (cl:- i j))))))))
      (make-tpower-series domain coeffs
			  :valence valence :order order :branch-order bo))))
;; EXPT
;;
;; Compute tp^s for a TPS.  The exponent may be a rational-integer, a
;; rational-number, or a element of the coefficient domain.
;;
(defmethod expt ((a tpower-series) k) 
  (if (not (or (integerp k) (typep k 'rational-integer)
	       (typep k 'ratio) (typep k 'rational-number)
               (0? (valence a))))
      (call-next-method)
      (with-tpower-series ((a a))
        (let* ((domain (domain-of a))
               (zero (zero (coefficient-domain-of domain)))
	       valence order coeffs)

	  (setq k (convert-to-lisp-number k))
	      
	  (cond ((zerop a-val)
		 (setq valence a-val)
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		((integerp k)
		 (setq valence (cl:* k a-val))
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		((typep k 'ratio)
		 (set-branch-order a (denominator k))
		 (setq valence (cl:/ (cl:* (numerator k) a-val)
				     (denominator k)))
		 (setq order (cl:+ (cl:- a-order a-val) valence)))
		(t (error "Internal error")))

	  (setq coeffs (make-array
			 (cond ((integerp order) (cl:- order valence -1))
			       ((not (integerp k))
				(error "Can't compute ~S ^ ~S"
				       a k))
			       (t (1+ (cl:* k (1- (length a-coeffs))))))
			 :initial-element zero))

	  (setf (svref coeffs 0) (expt (svref a-coeffs 0) k))

	  (loop with n-terms = (array-dimension coeffs 0)
		and a-lim = (array-dimension a-coeffs 0)
		for i upfrom 1 below n-terms
		do (loop for j below (1+ i)
			 do (setf (svref coeffs i)
				  (+ (svref coeffs i)
				     (* (- (* (+ k 1) j) i)
					(if (cl:< j a-lim) (svref a-coeffs j)
					    zero)
					(svref coeffs (cl:- i j)))))
			 finally (setf (svref coeffs i) 
				       (/ (svref coeffs i)
					  (* i (svref a-coeffs 0))))))
	  (make-tpower-series domain coeffs
			      :valence valence
			      :order order
			      :branch-order a-bo)))))

;; QUOTIENT
;;
;; This is the easy way.  There may be a more efficient way.
(defmethod quotient ((a tpower-series) (b tpower-series))
  (* a (expt b -1)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		       Taylor Expansions
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

 
(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmacro define-taylor-expansion-fun (name (domain order) &body body)
  (let ((maker-name (intern (format nil "TAYLOR-~A" name))))
       `(progn
	  (defun ,maker-name (,domain ,order) ,@body)
	  (setf (getf (get-function nil ',name) 'taylor-expansion-fun)
		',maker-name))))

(define-taylor-expansion-fun SIN (domain order)
  (let ((coeffs))
       (loop for i from 1 to order
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs
			     (append coeffs
				     (list (cl:* (cl:expt -1 (cl:/ (cl:- i 1) 2))
						 (cl:/ 1 (factorial i))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun COS (domain order)
  (let ((coeffs))
       (loop for i from 0 to order       
	     if (oddp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs
					    (list (cl:* (cl:expt -1 (cl:/ i 2))
							(cl:/ 1 (factorial i))))))
	     finally
	  (return (make-tpower-series domain coeffs :order order)))))
	    
(define-taylor-expansion-fun TAN (domain order)
  (taylor (/ (sin (ring-variables domain))
	     (cos (ring-variables domain)))
	  domain order))

(define-taylor-expansion-fun LOG (domain order)
  (let ((coeffs))
       (loop for i from 1 to order       
	     do (setq coeffs (append coeffs
				     (list (cl:* (cl:expt -1 (cl:- i 1))
						 (cl:/ 1 i)))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun ASIN (domain order)
  (let ((coeffs))
       (loop for i from 1 to order              
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (let ((temp-coeff 1))
			    (loop for j from 1 below i
				  if (oddp j) do
				    (setq temp-coeff (cl:* temp-coeff j))
				  else do
				    (setq temp-coeff (cl:/ temp-coeff j))
				  finally
			       (setq coeffs (append coeffs (list (cl:* temp-coeff
								    (cl:/ 1 i)))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))
		     
(define-taylor-expansion-fun SINH (domain order)
  (let ((coeffs))
       (loop for i from 1 to order
	     if (evenp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(define-taylor-expansion-fun COSH (domain order)
  (let ((coeffs))
       (loop for i from 0 to order
	     if (oddp i) do (setq coeffs (append coeffs (list 0)))
	       else do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	  finally
       (return (make-tpower-series domain coeffs :order order)))))

(define-taylor-expansion-fun TANH (domain order)
  (taylor (/ (sinh (ring-variables domain))
	     (cosh (ring-variables domain)))
	  domain order))

(define-taylor-expansion-fun ASINH (domain order)
  (let ((coeffs))
       (loop for i from 1 to order              
	     if (evenp i) do
	       (setq coeffs (append coeffs (list 0)))
	     else do
	       (let ((temp-coeff 1))
		    (loop for j from 1 below i
			  if (oddp j) do
			    (setq temp-coeff (cl:* temp-coeff j))
			  else do
			    (setq temp-coeff (cl:/ temp-coeff j))
			  finally
		       (setq coeffs
			     (append coeffs
				     (list (cl:* (cl:expt -1 (cl:/ (cl:- i 1) 2))
						 (cl:* temp-coeff
						       (cl:/ 1 i))))))))
	     finally
	  (return (make-tpower-series domain coeffs :valence 1 :order order)))))

(defun TAYLOR-EXP (domain order)
  (let ((coeffs))
       (loop for i from 0 to order
	     do (setq coeffs (append coeffs (list (cl:/ 1 (factorial i)))))
	     finally
	  (return (make-tpower-series domain coeffs :order order)))))

(defmethod substitute (value (variable symbol) (tp tpower-series)
			     &rest ignore)
  (declare (ignore ignore))
  (substitute value (coerce variable *general*) tp))

(defmethod substitute ((value tpower-series) (variable ge-variable)
		       (tp tpower-series) &rest ignore)
  (declare (ignore ignore))  
  (cond ((not (ge-equal variable (ring-variables (domain-of tp))))
	 (error "Can not substitute for ~A in ~A." variable (domain-of tp)))
	((not (plusp (valence value)))
	 (error "Can not substitute ~S for ~A in ~A." value variable tp))
	(t (let ((result (zero (domain-of value)))
		 (one (one (domain-of value))))
	     (with-tpower-series ((tp tp))
	       (declare (ignore tp-bo tp-order))
	       (loop for exp from (valence tp)
		     until (>= (- exp tp-val)
				      (array-dimension tp-coeffs 0))
		     do (setq result (+ result
				       (* (coerce (aref tp-coeffs
							(- exp tp-val))
						  (domain-of value))
					  (if (= exp 0) one
					      (expt value exp)))))))
		result))))

(defgeneric taylor (exp domain order)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod taylor (exp (domain tpower-series-domain) order)
  (taylor1 (coerce exp *general*) domain order))

(defgeneric taylor1 (exp domain order)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod taylor1 ((exp ge-variable) (domain tpower-series-domain) order)
  (cond ((ge-equal exp (ring-variables domain))
	 (make-tpower-series domain 1 :valence 1 :order order))
	(t (make-tpower-series domain (coerce exp (coefficient-domain-of domain))
					    :order order))))

(defmethod taylor1 ((exp ge-plus) (domain tpower-series-domain) order)
  (let ((sum (zero domain)))
       (loop for x in (terms-of exp)
	     do (setq sum (+ sum (taylor1 x domain order))))
       sum))

(defmethod taylor1 ((exp ge-times) (domain tpower-series-domain) order)
  (let ((new-valence 0)
	(max-valence 0)
	(new-order order)
	(prod (taylor1 1 domain order))
	(temp))
       (loop for x in (terms-of exp)
	     do (setq temp (taylor1 x domain order))
		(setq prod (* prod temp))
		(if (> (valence temp) max-valence)
		    (setq max-valence (valence temp)))
		(setq new-valence (+ new-valence (valence temp))))
       (setq new-order (+ new-order (- max-valence new-valence)))
       (if (= new-order order)
	   prod
	   (let ((prod (taylor1 1 domain new-order)))
		(loop for x in (terms-of exp)
		      do (setq prod (* prod (taylor1 x domain new-order))))
		prod))))

(defmethod taylor1 ((exp ge-expt) (domain tpower-series-domain) order)
  (let ((temp (taylor1 (base-of exp) domain order))
	(exponent (coerce (exponent-of exp) *general*)))
       (if (= (valence temp) 0)
	   (expt temp exponent)
	   (let ((new-order))
		(cond
		  ((typep exponent 'rational-integer)
		   (setq new-order (+ order (* (- 1 (integer-value exponent))
						  (valence temp)))))
		  ((typep exponent 'rational-number)
		   (let* ((new-br (lcm (denominator exponent)
				       (branch-order temp)))
			  (factor-old (/ new-br (branch-order temp)))
			  (factor-new (/ new-br (denominator exponent))))
			 (setq new-order (+ (ceiling
					      (/ (- (* order new-br)
						    (* factor-new
						       (numerator exponent)
						       (valence temp)))
						 factor-old))
					      (valence temp)))))
		  (t (error "can not raise ~s to ~s" (base-of exp) exponent)))
		(expt (taylor1 (base-of exp) domain new-order) exponent)))))

(defmethod taylor1 ((exp ge-application) (domain tpower-series-domain) order)
  (if (get-function nil (name-of (funct-of exp)))
      (let ((arg (car (args-of exp))))
	   (if (string= (name-of (funct-of exp)) "log")
	       (setq arg (- arg 1)))
	   (if (ge-equal arg (ring-variables domain))
	       (apply (getf (funct-of exp) 'taylor-expansion-fun)
		      (list domain order))
	       (substitute (taylor1 arg domain order)
			   (ring-variables domain)
			   (apply (getf (funct-of exp) 'taylor-expansion-fun)
				  (list domain order)))))
      (let ((tp (get-default-taylor-expansion domain)))
	   (if (typep (funct-of exp) 'ge-function-deriv)
	       (loop for i in (derivs-of (funct-of exp))
		     do (setq tp (deriv tp (nth i (args-of exp))))))
	   tp)))

(defmethod taylor1 (exp (domain tpower-series-domain) order)
  (let ((value (coercible? exp (coefficient-domain-of domain))))
    (cond ((not (null value))
	   (make-tpower-series domain value :order order))
	  (t (error "don't know how to coerce ~s to be an element of ~s"
		    exp domain)))))

(defmethod plus ((tp tpower-series) (exp general-expression))
  (+ tp (taylor1 exp (domain-of tp) (order tp))))

(defmethod plus ((exp general-expression) (tp tpower-series))
  (+ tp exp))

(defmethod difference ((tp tpower-series) (exp general-expression))
  (- tp (taylor1 exp (domain-of tp) (order tp))))

(defmethod difference ((exp general-expression) (tp tpower-series))
  (- (taylor1 exp (domain-of tp) (order tp)) tp))

(defmethod times ((tp tpower-series) (exp general-expression))
  (let ((temp (taylor1 exp (domain-of tp) (order tp))))
       (if (= (- (valence temp) (valence tp)) 0)
	   (* tp temp)
	   (* tp (taylor1 exp (domain-of tp) (+ (- (order tp) (valence tp))
						(valence temp)))))))

(defmethod times ((exp general-expression) (tp tpower-series))
  (* tp exp))

(defmethod quotient ((tp tpower-series) (exp general-expression))
  (* tp (expt exp -1)))

(defmethod quotient ((exp general-expression) (tp tpower-series))
  (* (expt exp -1) tp))
(in-package :weyli)

(defun get-default-taylor-expansion (power-series-domain)
    (let* ((temp-ring (coefficient-domain-of power-series-domain))
	   (coef-ring (if (typep temp-ring 'rational-function-field)
			      (QF-ring temp-ring)
			      temp-ring))   
	   (ring-vars (ring-variables coef-ring)))
	  (make-tpower-series power-series-domain
			      (mapcar #'(lambda (x) (coerce x coef-ring))
				      ring-vars)
			      :order (length ring-vars))))

(defmethod deriv ((tp tpower-series) &rest vars)
  (loop for v in vars
	with tp-var = (ring-variables (domain-of tp))
	do (setq tp (if (ge-equal (coerce v *general*) tp-var)
			(tps-deriv1 tp)
			(tps-deriv2 tp v))))
  tp)


;; Computes the derivative with respect to the power series variable.
(defun tps-deriv1 (a)
  (with-tpower-series ((a a))
    (let* ((domain (domain-of a))
	   (zero (zero (coefficient-domain-of domain)))
	   (n-terms (cl:- a-order a-val -1))
	   coeffs)

      (setq coeffs (make-array n-terms :initial-element zero))

      (loop for i below n-terms
	    do (setf (svref coeffs i)
		     (* (svref a-coeffs i) (+ a-val (/ i a-bo)))))
      (multiple-value-bind (ncoeffs shift) (trim-zeroes coeffs)
	(make-tpower-series domain ncoeffs
			    :valence (cl:+ a-val shift (cl:- a-bo))
			    :order (cl:- a-order a-bo)
			    :branch-order a-bo)))))

;; The coefficients are to be differentiated
(defun tps-deriv2 (a var)
  (with-tpower-series ((a a))
    (let* ((domain (domain-of a))
	   (zero (zero (coefficient-domain-of domain)))
	   (n-terms (cl:- a-order a-val -1))
	   coeffs)

      (setq coeffs (make-array n-terms :initial-element zero))

      (loop for i below n-terms
	    do (setf (svref coeffs i) (deriv (svref a-coeffs i) var)))
      (multiple-value-bind (ncoeffs shift) (trim-zeroes coeffs)
	(make-tpower-series domain ncoeffs
			    :valence (cl:+ a-val shift -1)
			    :order a-order
			    :branch-order a-bo)))))

(defmethod reversion ((tp tpower-series))
  (if (/= 1 (valence tp))
      (error "Can't compute reversion if valence is not 1")
      (let* ((domain (get-matrix-space (coefficient-domain-of (domain-of tp))))
	    (rank (length (coeffs tp)))
	    (array (make-array (list rank rank)))
	    (zero (zero (coefficient-domain-of (domain-of tp))))
	    (temp-tp tp))
	   (loop for i from 0 below rank do
	     (loop for k from 0 below i do
	       (setf (aref array i k) zero))
	     (loop for j from i below rank do
	       (setf (aref array i j) (aref (coeffs temp-tp) (- j i))))
	     (setq temp-tp (* temp-tp tp)))
	   (setq array (make-element domain array))
	   (setq array (recip array))
	   (make-tpower-series (domain-of tp)
			       (loop for j from 0 below rank
				     collect (ref array 0 j))
			       :valence 1
			       :order (order tp)))))

(defmethod solve-for-coeffs (coef-poly (domain rational-function-field)
				       coef-list value-list)
  (let ((cur-var (nth (length value-list) coef-list)))
       (quotient-reduce
	  domain
	  (- (coefficient (numerator coef-poly) (numerator cur-var) 0))
	  (coefficient (numerator coef-poly) (numerator cur-var) 1))))

(defmethod solve-for-coeffs (coef-poly domain coef-list value-list)
  (declare (ignore domain))
  (let ((cur-var (nth (length value-list) coef-list)))
	  (/ (- (coefficient coef-poly cur-var 0))
	     (coefficient coef-poly cur-var 1))))

;; diff-eqn is a differential-equation given as a general-expression
;; The solution of diff-eqn is assumed to be of the form
;;   a0 + a1 t + a2 t^2 + ... + an t^n
;; where "ai" are in the coef-ring, 't' is the variable and 'n' is the
;; order. The ai's are obtained using the initial value list "init-list"
;; containing (a0, a1, ..., aj) where 'j' is the order of the
;; differential equation.
#+ignore
(defmethod solve-diff-eqn ((diff-eqn general-expression)
			   coef-ring variable order init-list)
  (let* ((order (+ order 1))
	 (new-coefs (loop for i in init-list
			  if (not (coercible? i coef-ring)) collect i))
	 (coef-ring (if (null new-coefs) coef-ring
			  (get-polynomial-ring coef-ring new-coefs)))
	 (domain (get-quotient-field
		   (get-polynomial-ring coef-ring
			 (loop for i from 0 below order
			       collect (intern (format nil ".a~A" i))))))
	 (power-series-domain (get-tpower-series-domain domain variable))
	 (coef-list (mapcar #'(lambda (x) (coerce x domain)) init-list))
	 (var-list  (mapcar #'(lambda (x) (coerce x domain))
			    (ring-variables (QF-ring domain)))))	
	(setq diff-eqn (taylor diff-eqn power-series-domain order))
	(loop for i from 0 below (- (length var-list) (length coef-list)) do
	  (setq coef-list
		(append coef-list
			(list (solve-for-coeffs
				(substitute coef-list
					    (subseq var-list 0 (length coef-list))
					    (get-coeff diff-eqn i))
				domain var-list coef-list)))))
	(make-tpower-series power-series-domain
			    (simp-zeros coef-list)
			    :valence (num-leading-zeros coef-list)
			    :order (- (length coef-list) 1))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Rational Function Fields
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; rational-functions.lisp,v 1.9 1995/05/24 17:42:10 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(defmethod initialize-instance :after ((qf rational-function-field) &rest plist)
  (declare (ignore plist))
  (with-slots (print-function) qf
    (setf print-function 'ratfun-field-print-object))) 

(defun ratfun-field-print-object (qf stream)
  (format stream "~A(" (coefficient-domain-of (QF-ring qf)))
  (display-list (ring-variables (QF-ring qf)) stream)
  (princ ")" stream))

(defmethod ring-variables ((qf rational-function-field))
  (ring-variables (qf-ring qf)))

;; The general GET-Q... in quotient-field.lisp is good enough
(define-domain-creator quotient-field ((ring multivariate-polynomial-ring))
  (let* ((coefs (coefficient-domain-of ring))
         (qf (make-instance 'rational-function-field
                            :ring ring)))
    (with-slots (zero one) qf
      (setq zero (make-rational-function qf (zero coefs) (one coefs)))
      (setq one (make-rational-function qf (one coefs) (one coefs))))
    (make-homomorphism ring #'(lambda (x)
                                (make-quotient-element qf x (one ring)))
                       qf)
    qf))

(defsubst make-rational-function (domain numerator denominator)
  (make-instance 'rational-function :domain domain
		 :numerator numerator
		 :denominator denominator))

(defgeneric make-rational-function* (domain num den)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-rational-function* (domain num den)
  (let* ((poly-domain (qf-ring domain))
	 (coef-domain (coefficient-domain-of poly-domain)))
    (bind-domain-context poly-domain
      (cond ((and (typep coef-domain 'field)
		  (poly-coef? den))
	     (setq num (poly-times (recip den) num))
	     (setq den (one coef-domain)))
	    ((poly-minus? den)
	     (setq num (poly-minus num) den (poly-minus den)))))
    (make-rational-function domain num den)))

(defmethod make-quotient-element
    ((domain rational-function-field) numerator denominator)
  (make-rational-function domain
			  (poly-form (coerce numerator (qf-ring domain)))
			  (poly-form (coerce denominator (qf-ring domain)))))

(defmethod print-object ((ratfun rational-function) stream) 
  (with-numerator-and-denominator (numerator denominator) ratfun
    (cond ((poly-1? denominator)
	   (print-mpolynomial-form (QF-ring (domain-of ratfun)) numerator stream))
	  (t (princ "(" stream)
	     (print-mpolynomial-form (QF-ring (domain-of ratfun)) numerator stream)
	     (princ ")/(" stream)
	     (print-mpolynomial-form (QF-ring (domain-of ratfun)) denominator stream)
	     (princ ")" stream))))) 

(defmethod numerator ((r rational-function))
  (let ((domain (domain-of r)))
    (make-polynomial (QF-ring domain) (qo-numerator r))))

(defmethod denominator ((r rational-function))
  (let ((domain (domain-of r)))
    (make-polynomial (QF-ring domain) (qo-denominator r))))

(defmethod 0? ((r rational-function))
  (poly-0? (qo-numerator r)))

(defmethod 1? ((r rational-function))
    (and (poly-1? (qo-numerator r))
	 (poly-1? (qo-denominator r))))

(defmethod minus ((r rational-function))
  (let ((domain (domain-of r)))
    (with-numerator-and-denominator (numerator denominator) r
      (bind-domain-context (qf-ring domain)
	(make-rational-function domain (poly-minus numerator) denominator)))))

(defmethod quotient-reduce ((qf rational-function-field) num &optional den)
  (with-slots (ring) qf
    (when (not (eql (domain-of num) ring))
      (error "The numerator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of num) ring))
    (when (not (eql (domain-of den) ring))
      (error "The denominator's domain, ~S, is not the ring of the quotient field ~S"
	     (domain-of den) ring))
    (ratfun-reduce qf (poly-form num) (poly-form den))))

;; The arguments to ratfun-reduce are poly-forms not polynomials!!!
(defun ratfun-reduce (qf num &optional den)
  (when (null den)
    (setq den (one (QF-ring qf))))
  (if (poly-0? num) (zero qf)
      (let ((common-gcd (poly-gcd num den)))
	(unless (poly-1? common-gcd)
	  (setq num (poly-quotient num common-gcd)
		den (poly-quotient den common-gcd)))
	(make-rational-function* qf num den))))

(defmethod-sd plus ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-plus (poly-times n1 d2) (poly-times n2 d1))
		       (poly-times d1 d2))))))

(defmethod-sd difference ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-difference (poly-times n1 d2) (poly-times n2 d1))
		       (poly-times d1 d2))))))

(defmethod-sd times ((r1 rational-function) (r2 rational-function)) 
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(let (common-gcd)
	  (setq common-gcd (poly-gcd n1 d2))
	  (if (not (poly-1? common-gcd))
	      (setq n1 (poly-quotient n1 common-gcd)
		    d2 (poly-quotient d2 common-gcd)))
	  (setq common-gcd (poly-gcd n2 d1))
	  (if (not (poly-1? common-gcd))
	      (setq n2 (poly-quotient n2 common-gcd)
		    d1 (poly-quotient d1 common-gcd)))
	  (setq d1 (poly-times d1 d2)
		n1 (poly-times n1 n2))
	  (make-rational-function* domain n1 d1))))))

(defmethod-sd quotient ((r1 rational-function) (r2 rational-function)) 
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2	       
      (bind-domain-context (qf-ring domain)
	(let (common-gcd)
	  (setq common-gcd (poly-gcd n1 n2))
	  (if (not (poly-1? common-gcd))
	      (setq n1 (poly-quotient n1 common-gcd)
		    n2 (poly-quotient n2 common-gcd)))
	  (setq common-gcd (poly-gcd d1 d2))
	  (if (not (poly-1? common-gcd))
	      (setq d2 (poly-quotient d2 common-gcd)
		    d1 (poly-quotient d1 common-gcd)))
	  (setq n1 (poly-times n1 d2)
		d1 (poly-times d1 n2))
	  (make-rational-function* domain n1 d1))))))

(defmethod recip ((r1 rational-function))
    (with-numerator-and-denominator (num den) r1
      (make-rational-function* (domain-of r1) den num)))

(defmethod expt ((r1 rational-function) (exp integer))
  (let ((domain (domain-of r1)))
    (with-numerator-and-denominator (n1 d1) r1
      (bind-domain-context (qf-ring domain)
	(if (minusp exp)
	    (make-rational-function domain
				    (poly-expt d1 (cl:- exp))
				    (poly-expt n1 (cl:- exp)))
	    (make-rational-function domain
				    (poly-expt n1 exp) (poly-expt d1 exp)))))))

(defmethod expt ((r1 rational-function) (exp rational-integer))
  (expt r1 (integer-value exp)))

(defmethod-sd binary-gcd ((r1 rational-function) (r2 rational-function))
  (with-numerator-and-denominator (n1 d1) r1
    (with-numerator-and-denominator (n2 d2) r2
      (bind-domain-context (qf-ring domain)
	(make-rational-function domain (poly-gcd n1 n2) (poly-lcm d1 d2))))))


(defmethod list-of-variables
    ((x rational-function) &optional list-of-variables)
  (let* ((domain (domain-of x))
	 (ring-domain (qf-ring domain)))
    (with-numerator-and-denominator (num-x den-x) x
      (loop for order-number in (poly-list-of-variables
				     den-x (poly-list-of-variables num-x))
	    do (pushnew (get-variable-name order-number ring-domain)
			list-of-variables :test #'ge-equal)))
    list-of-variables))

;; This is just like poly-subst, but its result is a rational function
;; and thus the values being substituted can be rational functions. 
(defun rational-poly-subst (poly var-value)
  (let ((temp nil))
    (cond ((null var-value)
	   poly)
	  ((poly-coef? poly) (coerce poly *domain*))
	  (t (setq temp (or (second (assoc (poly-order-number poly) var-value
				     :test #'eql))))
	   (when (null temp)
	     (error "This variable can't be mapped into the domain ~S"
		    *domain*))
	   (rational-terms-horners-rule (poly-terms poly) temp var-value)))))

(defun rational-terms-horners-rule (terms value &optional var-value)
  (let ((old-e (le terms))
	(ans (rational-poly-subst (lc terms) var-value)))
    (map-over-each-term (red terms) (e c) 
      (setq ans (+ (* (expt value (e- old-e e)) ans)
		   (rational-poly-subst c var-value)))
      (setq old-e e))
    (* ans (expt value old-e))))

(defmethod substitute 
    ((value rational-function) (variable rational-function)
     (p rational-function) &rest ignore)
  (declare (ignore ignore))
  (substitute (list value) (list variable) p))

(defmethod substitute ((values list) (variables list) (p rational-function)
		       &rest ignore)
  (declare (ignore ignore))
  (let* ((domain (domain-of p))
	 (ring (qf-ring domain))
	 (new-domain (domain-of (first values)))
	 subst-list)
    (loop for var in variables
	  unless (eql (domain-of var) domain)
	    do (error "Domain of ~S was expected to be ~S" var domain))
    (loop for val in values
	  unless (eql (domain-of val) new-domain)
	    do (error "Domain of ~S was expected to be ~S" val new-domain))
    (loop for var in (ring-variables ring)
	  do (unless (find var variables 
			   :test #'(lambda (a b) 
				     (eql a (variable-symbol
					   ring (numerator b))))) 
	       (push (coerce var domain) variables)
	       (push (if (coercible? var new-domain)
			 (coerce var new-domain)
			 nil)
		     values)))
    (setq subst-list (loop for var in variables
			   for val in values
			   collect (list (variable-index ring (numerator var))
					 val)))
    (with-numerator-and-denominator (num den) p
      (bind-domain-context new-domain
	(/ (rational-poly-subst num subst-list)
	   (rational-poly-subst den subst-list))))))

(defmethod partial-deriv ((p rational-function) x)
  (error "Don't know how to compute the partial deriv with respect to ~S"
	 x))

(defmethod partial-deriv ((p rational-function) (x symbol))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p rational-function) (x list))
  (partial-deriv p (coerce x *general*)))

(defmethod partial-deriv ((p rational-function) (x general-expression))
  (let ((domain (domain-of p)))
    (with-slots (variables) (qf-ring domain)
      (if (member x variables :test #'ge-equal)
	  (partial-deriv p (coerce x domain))
	  (call-next-method)))))

(defmethod partial-deriv ((p rational-function) (x rational-function))
  (with-numerator-and-denominator (num-x den-x) x
    (with-numerator-and-denominator (num-p den-p) p
      (let ((domain (domain-of p))
	    terms)
	(unless (and (eql domain (domain-of x))
		     (1? den-x)
		     (null (red (setq terms (poly-terms num-x))))
		     (e1? (le terms))
		     (poly-1? (lc terms)))
	  (error "~S is not a variable in ~S" x domain))
	(bind-domain-context (qf-ring domain)
	  (ratfun-reduce domain
			 (poly-difference
			  (poly-times (poly-derivative num-p num-x) den-p)
			  (poly-times (poly-derivative den-p num-x) num-p))
			 (poly-times den-p den-p)))))))

(defmethod deriv ((poly rational-function) &rest vars)
  (let* ((domain (domain-of poly))
	 deriv diff)
    (bind-domain-context domain
      (loop for var in vars do
	(setq var (coerce var *general*))
	(setq deriv (zero domain))
	(loop with variables = (list-of-variables poly)
	      for kernel in variables do
	  (when (depends-on? kernel var)
	    (setq diff (deriv kernel var))
	    (loop for new in (different-kernels diff variables) do
	      (add-new-variable (qf-ring domain) new))
	    (setq deriv
		  (+ deriv (* (partial-deriv poly kernel)
			      (coerce diff domain))))))
	(setq poly deriv)))
    poly))

(defmethod coerce ((x ge-expt) (domain rational-function-field))
  (if (ge-minus? (exponent-of x))
      (recip (coerce (expt (base-of x) (- (exponent-of x)))
		     domain))
      (call-next-method)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			     Differential Rings
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; differential-domains.lisp,v 1.7 1995/05/24 17:41:58 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(defmethod ring-variables ((domain differential-polynomial-ring))
  (with-slots ((vars variables)) domain
    (loop for v in vars
	  when (or (atom v) (not (eql (first v) 'derivation)))
	    collect v)))

(defsetf variable-derivation set-variable-derivation)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator differential-ring ((coefficient-domain ring) variables) 
    (progn
      (setq variables (loop for var in variables
                            collect (coerce var *general*)))
      (let ((ring (make-instance 'differential-polynomial-ring 
                                 :variables variables
                                 :coefficient-domain coefficient-domain
                                 :print-function 'differential-ring-print-object)))
        (loop for var in variables do
              (setf (variable-derivation ring var) :generate))
        ring))
    :predicate 
    #'(lambda (d)  
        (and (eql (class-name (class-of d)) 'differential-polynomial-ring) 
             (eql (coefficient-domain-of d) coefficient-domain)
             (eql (ring-variables d) variables)
             ;; And check that the derivations are the same.
             )))) 

(defun differential-ring-print-object (d stream)
  (format stream "~A<" (coefficient-domain-of d))
  (display-list (ring-variables d))
  (princ ">" stream))

(defmethod coerce ((variable list) (domain differential-polynomial-ring))
  (cond ((member variable (ring-variables domain))
	 (make-polynomial domain
	    (cons (variable-index domain variable)
		  (make-terms 1 (one (coefficient-domain-of domain))))))
	((and (not (atom variable))
	      (eql (first variable) 'deriv))
	 (loop for i below (third variable)
	       for p = (deriv (coerce (second variable) domain)) then (deriv p)
	       finally (return p)))
	((coercible? variable (coefficient-domain-of domain)))
	(t (call-next-method))))

;; Derivations are more complex than differentation.
;; This returns the derivation of the main variable of the polynomial.
;; In general this polynomial is expected to be of degree 1 with
;; coefficient 1.
(defmacro variable-derivation (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :derivation))

(defmacro variable-derivative-order (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :derivative-order))

(defgeneric set-variable-derivation (domain variable derivation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod set-variable-derivation
    ((domain differential-polynomial-ring)
     (variable symbol) derivation)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      #+Genera
      (error "~'i~A~ is not a variable of ~S" variable domain)
      #-Genera      
      (error "~A is not a variable of ~S" variable domain)))
  (cond ((eql derivation :generate)
	 (setf (get-variable-number-property domain
					     (variable-index domain variable)
					     :derivation)
	       :generate))
	(t (cond ((eql (domain-of derivation) *general*)
		  (setq derivation (coerce derivation domain)))
		 ((not (eql (domain-of derivation) domain))
		  (error "The derivation ~S is not an element of ~S" 
			 derivation domain)))
	   (setf (get-variable-number-property domain 
					       (variable-index domain variable)
					       :derivation)
		 (poly-form derivation)))))

(defmethod set-variable-derivation
    ((domain differential-polynomial-ring)
     (variable general-expression) derivation)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      #+Genera
      (error "~'i~A~ is not a variable of ~S" variable domain)
      #-Genera      
      (error "~A is not a variable of ~S" variable domain)))
  (cond ((eql derivation :generate)
	 (setf (get-variable-number-property domain
					     (variable-index domain variable)
					     :derivation)
	       :generate))
	(t (cond ((eql (domain-of derivation) *general*)
		  (setq derivation (coerce derivation domain)))
		 ((not (eql (domain-of derivation) domain))
		  (error "The derivation ~S is not an element of ~S" 
			 derivation domain)))
	   (setf (get-variable-number-property domain 
					       (variable-index domain variable)
					       :derivation)
		 (poly-form derivation)))))

(defmethod add-new-variable ((domain differential-ring) variable)
  (prog1
    (call-next-method)
    (setq variable (coerce variable *general*))
    (setf (variable-derivation domain variable) :generate)))

(defun standard-derivation (p)
  (let ((deriv (variable-derivation *domain* p)))
    (cond ((null deriv) (zero *coefficient-domain*))
	  ((eql deriv :generate)
	   (let* ((old-var (variable-symbol *domain* (poly-order-number p)))
		  (new-order
		   (cond ((ge-variable? old-var) 1)
			 ((eql (first old-var) 'derivation)
			  (1+ (third old-var)))
			 (t 1)))
		  (new-var `(derivation
			     ,(if (or (ge-variable? old-var)
				      (not (eql (first old-var) 'derivation)))
				  old-var
				  (second old-var))
			     ,new-order))
		  new-var-num)
	     (add-new-variable *domain* new-var)
	     (setq new-var-num (variable-index *domain* new-var))
	     (setf (variable-derivation *domain* old-var) new-var)
	     #+ignore
	     (setf (variable-derivative-order *domain* new-var) new-order)
	     (cons new-var-num (make-terms 1 (one *coefficient-domain*)))))
 	  (t deriv))))

(defun poly-derivation (p &optional (derivation #'standard-derivation))
  (let ((deriv nil) (temp nil))
    (cond ((poly-coef? p) (zero *coefficient-domain*))
	  (t (setq deriv (%funcall derivation p))
	     (poly-plus
	       (if (poly-0? deriv) deriv
		   (poly-times
		     (make-poly-form
		       p
		       (map-over-each-term (poly-terms p) (e c)
			 (unless (e0? e)
			   (unless (poly-0?
				     (setq temp
					   (poly-times
					    (coerce e *coefficient-domain*)
					    c)))
			     (collect-term (e1- e) temp)))))
		     deriv))
	       (poly-differentiate-coefs p derivation))))))

(defun poly-differentiate-coefs (p derivation)
  (let* ((dc nil)
	 (one (one *coefficient-domain*))
	 (terms (poly-terms p))
	 (sum (poly-times (make-poly-form p (make-terms (le terms) one))
			  (poly-derivation (lc terms) derivation))))
    (map-over-each-term (red terms) (e c)
      (setq dc (poly-derivation c derivation))
      (setq sum (poly-plus sum 
			   (poly-times dc
				       (make-poly-form p 
						       (make-terms e one))))))
    sum))

(defmethod derivation ((poly polynomial))
  (let ((domain (domain-of poly)))
    (unless (typep domain 'differential-ring)
      (error "No derivation operator for ~S" domain))
    (bind-domain-context domain
      (make-polynomial domain (poly-derivation (poly-form poly))))))

(defmethod derivation ((rat rational-function))
  (let ((domain (domain-of rat)))    
    (unless (typep (qf-ring domain) 'differential-ring)
      (error "No derivation operator for ~S" domain))
    (with-numerator-and-denominator (n d) rat
      (bind-domain-context (qf-ring domain)
	(ratfun-reduce domain
		       (poly-difference
			(poly-times (poly-derivation n) d)
			(poly-times (poly-derivation d) n))
		       (poly-times d d))))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Finite Algebraic Extension
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;; algebraic-extension.lisp,v 1.5 1994/10/04 22:30:39 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator factor-ring ((ring ring) (ideal ideal))
    (cond ((eql (ring-of ideal) ring)
           (make-instance 'factor-ring :numerator ring :denominator ideal))
          (t (error "Don't know how to compute ~S/~S" ring ideal)))
    :predicate
    #'(lambda (d)
        (and (typep d 'factor-ring)
             (eql (factor-numer-of d) ring)
             (= (factor-denom-of d) ideal)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator algebraic-extension
      ((coefficient-domain ring) variables)
    (progn
      (unless (integral-domain? coefficient-domain)
        (error "Can only create algebraic extensions of integral domains: ~S"
               coefficient-domain))
      (let ((domain
             (make-instance 'algebraic-extension-ring
                            :variables (loop for var in variables
                                             collect (coerce var *general*))
                            :coefficient-domain coefficient-domain)))
        (make-homomorphism coefficient-domain
                           #'(lambda (c) (make-polynomial domain c))
                           domain)
        domain))
    :predicate
    #'(lambda (d) ;; FIXTHIS: the predicate needs to be improved
        (and (typep d 'algebraic-extension-ring)
             (eql (coefficient-domain-of d) coefficient-domain)
             (equal (ring-variables d) variables)))))

;; Use the polynomial print-object method for now

;; This returns the term list for the minimal polynomial of the main
;; variable of the polynomial.  This polynomial is expected to be monic.
(defmacro variable-minimal-polynomial (domain var)
  `(get-variable-number-property ,domain (poly-order-number ,var)
				 :minimal-polynomial))

(defgeneric minimal-polynomial (domain variable)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod minimal-polynomial ((domain algebraic-extension-ring) variable)  
  (with-slots (variables) domain
    (unless (member variable variables :test #'ge-equal)
      (error "~'i~A~ is not a variable of ~S" variable domain)))
  (get-variable-number-property domain (variable-index domain variable)
				:minimal-polynomial))

(defmethod minimal-polynomial
    ((domain algebraic-extension-ring) (variable integer))  
  (get-variable-number-property domain variable :minimal-polynomial))

(defmethod set-minimal-polynomial
	   ((domain algebraic-extension-ring) variable minimal-polynomial)
  (setq variable (coerce variable *general*))
  (with-slots (variables) domain    
    (unless (member variable variables :test #'ge-equal)
      (error "~'i~A~ is not a variable of ~S" variable domain)))
  (unless (eql (domain-of minimal-polynomial) domain)
    (error "The algebraic relation ~S is not an element of ~S" 
	   minimal-polynomial domain))
  (let ((poly-form (poly-form minimal-polynomial))
	(var-index (variable-index domain variable)))
    (unless (= var-index (poly-order-number poly-form))
      (error "~S is not the most main variable of ~S"
	     (with-output-to-string (string)
	       (display variable string))
	       minimal-polynomial))
    (setf (get-variable-number-property domain var-index :minimal-polynomial)
	  (poly-terms poly-form))))

(defsetf minimal-polynomial set-minimal-polynomial)

(defmethod make-polynomial ((domain algebraic-extension-ring) form)
  (make-instance 'algebraic-object :domain domain :form form))

(defmethod-sd times ((x algebraic-object) (y algebraic-object))
  (bind-domain-context domain
    (make-polynomial domain (alg-poly-times (poly-form x) (poly-form y)))))

(defun alg-poly-times (x y)
  (cond ((poly-coef? x)
	 (if (poly-coef? y) (* x y)
	     (poly-simp y (terms-mon-times (poly-terms y) (e0) x))))
	((poly-coef? y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	((same-variable? x y)
	 (let ((min-poly (minimal-polynomial *domain* (poly-order-number x))))
	   (poly-simp x (if min-poly
			    (terms-pseudo-remainder
			     (terms-times (poly-terms x) (poly-terms y))
			     min-poly)
			    (terms-times (poly-terms x) (poly-terms y))))))
	((more-main? x y)
	 (poly-simp x (terms-mon-times (poly-terms x) (e0) y)))
	(t (poly-simp y (terms-mon-times (poly-terms y) (e0) x)))))

(defmethod expt ((base algebraic-object) (expt integer))
  (let ((domain (domain-of base)))
    (bind-domain-context domain
      (make-polynomial domain
		       (%funcall (repeated-squaring #'alg-poly-times
							(one *coefficient-domain*))
				     (poly-form base) expt)))))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    Vector Space
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; vector.lisp,v 1.11 1995/06/05 20:38:00 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;; Useful macro until everything is ANSI compliant
(defmacro loop-vector-bind (index vars-vectors &body body)
  (let ((cnt 0) vectors limit var-bindings)
    (setq var-bindings
	  (loop for (var vect) in vars-vectors
		for vector = (intern (format nil ".VV~D." (incf cnt)))
		do (push (list var vector) vectors)
		collect `(,vector ,vect)
		finally (setq vectors (nreverse vectors))))
    (cond ((atom index)
	   (when (null index)
	     (setq index '.I.))
	   (setq limit `(min ,@(loop for (nil vect) in vectors
				     collect `(array-dimension ,vect 0)))))
	  (t (setq limit (second index))
	     (setq index (first index))))	  
    `(let ,var-bindings
       (declare (optimize (safety 1)))
       (loop for ,index fixnum below ,limit
	     ,@(loop for (var vect) in vectors
		     append `(for ,var = (svref ,vect ,index)))
	     do ,@body))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator free-module ((domain ring) dimension)
    (make-instance 'free-module
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'free-module-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'free-module)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator vector-space ((domain field) dimension)
    (make-instance 'vector-space
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'free-module-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'vector-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun free-module-print-object (domain stream)
  (format stream #+Genera "~S~D" #-Genera "~S^~D"
	  (coefficient-domain-of domain)
	  (dimension-of domain)))

(define-domain-element-classes free-module free-module-element)
(define-domain-element-classes vector-space vector-space-element)

(defmethod make-element ((domain free-module) (value vector) &rest values)
  (declare (ignore values))
  (make-element-free-module-vector domain value))

(defun make-element-free-module-vector (domain value)
  (let ((dim (dimension-of domain)))
    (unless (eql (array-dimension value 0) dim)
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values value)))

(defmethod make-element ((domain free-module) value &rest values)
  (let ((dim (dimension-of domain)))
    (unless (eql (1- dim) (length values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values (%apply #'vector value values))))

(defmethod weyl::make-element ((domain free-module) value &rest values)
  (let ((dim (dimension-of domain))
	(coef-domain (coefficient-domain-of domain)))
    (cond ((typep value 'vector)
	   (unless (and (eql (array-dimension value 0) dim)
			(null values))
	     (error "Wrong number of vector elements in ~S" domain))
	   (make-instance (first (domain-element-classes domain))
	     :domain domain
	     :values (%apply #'vector
			    (loop for i fixnum below (length value)
				  collect (coerce (aref value i) coef-domain)))))
	  (t (unless (eql (1- dim) (length values))
	       (error "Wrong number of vector elements in ~S" domain))
	     (make-instance (first (domain-element-classes domain)) 
	       :domain domain
	       :values (%apply #'vector
			      (coerce value coef-domain)
			      (loop for v in values
				    collect (coerce v coef-domain))))))))

(defmethod print-object ((elt free-module-element) stream)
  (print-free-module-element elt stream))

(defun print-free-module-element (elt stream)
  (let* ((domain (domain-of elt))
	(dim (if (typep domain 'dimensional-domain)
		 (dimension-of (domain-of elt))
		 (array-dimension (tuple-value elt) 0))))
    (write-char #\< stream)
    (unless (0? dim)
      (print-object (ref elt 0) stream)
      (loop for i upfrom 1 below dim
	    do (princ ", " stream)
	       (print-object (ref elt i) stream)))
    (write-char #\> stream)))

(defmethod dimensions ((v vector-space-element))
  (list (dimension-of (domain-of v))))

(defmethod 0? ((v free-module-element))
  (loop for i fixnum below (dimension-of (domain-of v))
	do (unless (0? (ref v i))
	     (return nil))
	finally (return t)))


(defmethod zero ((domain free-module))
  (let ((dim (dimension-of domain))
	(zero (zero (coefficient-domain-of domain))))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values (make-array dim :initial-element zero))))

(defmethod list-of-variables ((v free-module-element) &optional ignore)
  (declare (ignore ignore))
  (loop for i fixnum below (dimension-of (domain-of v))
	with list
	do (setq list (list-of-variables (ref v i) list))
	finally (return list)))

(defmethod-sd max-pair ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e1 (tuple-value v1)) 
			       (e2 (tuple-value v2)))
      (setf (svref ans i) (max e1 e2)))
    (make-element domain ans)))

(defmethod-sd min-pair ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e1 (tuple-value v1))
			       (e2 (tuple-value v2)))
      (setf (svref ans i) (min e1 e2)))
    (make-element domain ans)))

(defmethod-sd plus ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
	((e1 (tuple-value v1))
	 (e2 (tuple-value v2)))
      (setf (svref ans i) (+ e1 e2)))
    (make-element domain ans)))

(defmethod minus ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (minus e)))
    (make-element vector-space ans)))

(defmethod-sd difference ((v1 free-module-element) (v2 free-module-element))
  (let* ((dim (dimension-of domain))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
	((e1 (tuple-value v1))
	 (e2 (tuple-value v2)))
      (setf (svref ans i) (- e1 e2)))
    (make-element domain ans)))

;; The checks to see if the scalar is a free-module element are
;; necessary because we can multiple two quaternion's together.  There
;; are some complications here.  --RZ 11/2/94

(defmethod times :around (scalar (vector free-module-element))
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector coerced-scalar))
	  ((typep scalar 'free-module-element)
	   (call-next-method))
	  (t (multiply-vector-by-scalar vector scalar)))))

;; The :around methods for this method and the next are not really
;; needed since they are chosen in preference other similar methods
;; since they have a more specialized first argument.  Nonetheless,
;; I'm leaving the :around's here for symmetry and emphasis.
;;  --RZ 7/12/94
(defmethod times :around ((vector free-module-element) scalar)
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector coerced-scalar))
	  ((typep scalar 'free-module-element)
	   (call-next-method))
	  (t (multiply-vector-by-scalar vector scalar)))))

(defmethod quotient :around ((vector free-module-element) scalar)
  (let ((coeff-domain (coefficient-domain-of (domain-of vector)))
	(coerced-scalar nil))
    (cond ((and (not (numberp scalar))
		(eql (domain-of scalar) coeff-domain))
	   (multiply-vector-by-scalar vector (/ scalar)))
	  ((and *coerce-where-possible*
		;; Don't clobber the arg, the next cluase needs it
		(setq coerced-scalar (coercible? scalar coeff-domain)))
	   (multiply-vector-by-scalar vector (/ coerced-scalar)))
	  (t (call-next-method scalar vector)))))

(defun multiply-vector-by-scalar (vector scalar)
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (* e scalar)))
    (make-element vector-space ans)))

(defmethod-sd dot-product ((v1 free-module-element) (v2 free-module-element))
  (loop for i fixnum upfrom 1 below (dimension-of domain)
	with ans = (* (ref v1 0) (ref v2 0))
	do (setq ans (+ ans (* (ref v1 i) (ref v2 i))))
	finally (return ans)))

(defmethod-sd inner-product ((v1 free-module-element) (v2 free-module-element))
  (dot-product v1 v2))

(defmethod cross-product (v1 v2)
  (error "CROSS-PRODUCT product is not implemented for elements of ~S and ~S"
	 (domain-of v1) (domain-of v2)))

(defmethod cross-product ((v1 free-module-element) (v2 free-module-element))
  (let ((domain (domain-of v1))
	a b)
    (cond ((and (eql domain (domain-of v2))
		(= 3 (dimension-of domain)))
	   (setq a (tuple-value v1)
		 b (tuple-value v2))
	   (make-element domain 
			 (- (* (aref a 1) (aref b 2))
			    (* (aref a 2) (aref b 1)))
			 (- (* (aref a 2) (aref b 0))
			    (* (aref a 0) (aref b 2)))
			 (- (* (aref a 0) (aref b 1))
			    (* (aref a 1) (aref b 0)))))
	  (t (call-next-method)))))

(defmethod tilde (vect)
  (error "TILDE is not implemented for elements of ~S" (domain-of vect)))

(defmethod tilde ((vect free-module-element))
  (cond ((= 3 (dimension-of (domain-of vect)))
	 (let ((matrix-space (get-matrix-space
			      (coefficient-domain-of (domain-of vect))))
	       (v1 (ref vect 0))
	       (v2 (ref vect 1))
	       (v3 (ref vect 2))
	       (zero (zero (coefficient-domain-of (domain-of vect)))))
	   (make-element matrix-space 
			 (make-array (list 3 3) 
				     :initial-contents
				     `((,zero ,(- v3) ,v2) 
				       (,v3   ,zero   ,(- v1))
				       (,(- v2) ,v1   ,zero))))))
	(t (call-next-method))))

(defmethod derivation ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (coef-domain (coefficient-domain-of vector-space)))
    (cond ((member 'deriv (list-operations coef-domain))
	   (let* ((vector-space (domain-of vector))
		  (dim (dimension-of vector-space))
		  (ans (make-array dim)))
	     (loop-vector-bind (i dim) ((e (tuple-value vector)))
	       (setf (svref ans i) (deriv e)))
	     (make-element vector-space ans)))
	  (t (error "Derivation is not a legal operation for domain ~S~%"
		    vector-space)))))

(defmethod deriv ((vector free-module-element) &rest vars)
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value vector)))
      (setf (svref ans i) (%apply #'deriv e vars)))
    (make-element vector-space ans)))

;; v1 - v2 has no negative components
(defmethod-sd dominates ((v1 lisp-vector) (v2 lisp-vector))
  (loop	with dimension = (dimension-of domain)
        and vect1 = (tuple-value v1) and vect2 = (tuple-value v2)
        for i below dimension
	when (< (aref vect1 i) (aref vect2 i))
	  do (return nil)
        finally (return t)))

(defmethod-sd disjoint ((v1 lisp-vector) (v2 lisp-vector))
  (loop	with dimension = (dimension-of domain)
        and vect1 = (tuple-value v1) and vect2 = (tuple-value v2)
        for i below dimension
	when (not (or (0? (aref vect2 i)) (0? (aref vect1 i))))
	 do (return nil)
        finally (return t)))

(defmethod substitute ((values list) (variables list) (v free-module-element)
		       &rest ignore)
  (declare (ignore ignore))
  (let* ((dim (dimension-of (domain-of v)))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim) ((e (tuple-value v)))
      (setf (svref ans i) (substitute values variables e)))
    (make-element (domain-of v) ans)))


;; Should the absolute value of a vector be defined?  The phase?

(defmethod conjugate ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (conjugate e)))
    (make-element vector-space ans)))

(defmethod realpart ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (realpart e)))
    (make-element vector-space ans)))

(defmethod imagpart ((vector free-module-element))
  (let* ((vector-space (domain-of vector))
	 (dim (dimension-of vector-space))
	 (ans (make-array dim)))
    (loop-vector-bind (i dim)
      ((e (tuple-value vector)))
      (setf (aref ans i) (imagpart e)))
    (make-element vector-space ans)))


;;; the dimension of a vector is the dimension of it's domain
(defmethod dimension-of ((v vector-space-element))
  (dimension-of (domain-of v)))
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Projective Spaces
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; projective-space.lisp,v 1.5 1995/05/24 17:42:09 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.5")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator projective-space ((domain field) dimension)
    (make-instance 'projective-space
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'projective-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'projective-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun projective-space-print-object (domain stream)
  (format stream #+Genera "P~D(~S)" #-Genera "P^~D(~S)"
	  (dimension-of domain)
	  (coefficient-domain-of domain)))

(defun make-projective-space-element (domain value)
  (make-instance 'projective-space-element :domain domain :value value))

(defmethod make-element ((domain projective-space) value &rest values)
  (let ((dim (dimension-of domain))
	(num-values (1+ (length values)))
	(coeff-domain (coefficient-domain-of domain))
	array)
    (setq values (cons value values))
    (cond ((eql dim num-values)
	   (setq array (make-array (1+ dim)))
	   (loop for i below dim
		 for v in values do
	     (setf (aref array i) (coerce v coeff-domain)))
	   (setf (aref array dim) (one coeff-domain))
	   (make-projective-space-element domain array))
	  ((eql dim (1- num-values))
	   (setq array (make-array (1+ dim)))
	   (loop for i below dim
		 for v in values 
		 with denom = (coerce (first (last values)) coeff-domain) do
	     (setf (aref array i) (/ (coerce v coeff-domain) denom)))
	   (setf (aref array dim) (one coeff-domain))
	   (make-projective-space-element domain array))
	  (t (error "Wrong number of vector elements in ~S" domain)))))

(defmethod ref ((vect projective-space-element) &rest args)
  (aref (tuple-value vect) (first args)))

(defmethod vector-set-ref
    ((vect projective-space-element) new-value &rest args)
  (setf (aref (tuple-value vect) (first args)) new-value))

;; Create an affine space corresponding to 
(defmethod make-affine-space ((space projective-space) &optional n)
  (let* ((dim (dimension-of space))
	 (range-space (make-vector-space (coefficient-domain-of space) dim))
	 homo)
    (when (null n)
      (setf n dim))
    (labels ((project (vector)
	       (loop with denom = (ref vector n)
		     for i below (1+ dim)
		     unless (= i n)
		       collect (/ (ref vector i) denom)))
	     (map-fun (vector)
	       (%apply #'make-element range-space (project vector))))
      (setq homo (make-morphism space #'map-fun range-space))
      (values (morphism-range homo)
	      (morphism-map homo)))))

;;; -*- Mode:Lisp; Package: WEYLI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				  Quaternions
;;;
;;;
;;; based on "Applications of quaternions to computations with rotation",
;;;   Eugene Salamin.
;;; Contains quaternions over a field and unit quaternions.
;;;
;;; Not yet implemented: homomorphism between unit quaternions and SO(3)
;;;
;;; ===========================================================================

;;; (c) Copyright 1989, 1993 Cornell University

;;; quaternions.lisp,v 1.9 1995/05/24 17:42:09 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.9")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator quaternion-domain ((domain field))
    (make-instance 'quaternion-domain
                   :coefficient-domain domain
                   :dimension 4
                   :print-function 'quaternion-domain-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'quaternion-domain)
                        (eql (coefficient-domain-of d) domain)))))

(defun quaternion-domain-print-object (domain stream)
  (format stream "Quat(~S)" (coefficient-domain-of domain)))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator unit-quaternion-domain ((domain field))
    (make-instance 'unit-quaternion-domain
                   :coefficient-domain domain
                   :dimension 4
                   :print-function 'unit-quaternion-domain-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'unit-quaternion-domain)
                        (eql (coefficient-domain-of d) domain)))))

(defun unit-quaternion-domain-print-object (domain stream)
  (format stream "UQuat(~S)" (coefficient-domain-of domain)))


;;; Quaternion elements themselves


(define-domain-element-classes quaternion-domain
    quaternion-domain-element)

(define-domain-element-classes unit-quaternion-domain
    unit-quaternion-domain-element)

(defmethod make-element ((domain quaternion-domain) (value vector)
			 &rest values)
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :values value))

(defmethod weyl::make-element ((domain quaternion-domain) (value vector)
			       &rest values)
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (loop for i below 4 do
      (setf (aref vector i) (coerce (aref value i) coef-domain)))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values vector)))

(defmethod make-element ((domain quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain
		 :values (%apply #'vector value values)))

(defmethod weyl::make-element ((domain quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (setf (aref vector 0) (coerce value coef-domain))
    (setf (aref vector 1) (coerce (first values) coef-domain))
    (setf (aref vector 2) (coerce (second values) coef-domain))
    (setf (aref vector 3) (coerce (third values) coef-domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values vector)))

(defmethod make-element ((domain unit-quaternion-domain) (value vector)
			 &rest values)  
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :values value))

;; FIXTHIS: Should check to make sure that quaternion is a unit
(defmethod weyl::make-element ((domain unit-quaternion-domain) (value vector)
			       &rest values)  
  (unless (and (eql (array-dimension value 0) 4)
	       (null values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (loop for i below 4 do
      (setf (aref vector i) (coerce (aref value i) coef-domain)))
    (make-instance (first (domain-element-classes domain))
		   :domain domain :values vector)))

(defmethod make-element ((domain unit-quaternion-domain) value &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (make-instance (first (domain-element-classes domain))
		 :domain domain
		 :values (%apply #'vector value values)))

;; FIXTHIS: Should check to make sure that quaternion is a unit
(defmethod weyl::make-element ((domain unit-quaternion-domain) value
			       &rest values)
  (unless (eql 3 (length values))
    (error "Wrong number of vector elements in ~S" domain))
  (let ((coef-domain (coefficient-domain-of domain))
	(vector (make-array 4)))
    (setf (aref vector 0) (coerce value coef-domain))
    (setf (aref vector 1) (coerce (first values) coef-domain))
    (setf (aref vector 2) (coerce (second values) coef-domain))
    (setf (aref vector 3) (coerce (third values) coef-domain))
    (make-instance (first (domain-element-classes domain))
		   :domain domain
		   :values vector)))

(defmethod conjugate ((q quaternion-with-multiplication))
  (let ((value (tuple-value q)))
    (make-element (domain-of q)
      (aref value 0) (- (aref value 1))
      (- (aref value 2)) (- (aref value 3)))))

(defmethod-sd dot-product
    ((q1 quaternion-with-multiplication) (q2 quaternion-with-multiplication))
  (loop for i upfrom 1 below 4
	with ans = (* (ref q1 0) (ref q2 0))
	do (setq ans (+ ans (* (ref q1 i) (ref q2 i))))
	finally (return ans)))

(defmethod-sd times
  ((p quaternion-with-multiplication) (q quaternion-with-multiplication))
  (let* ((pp (tuple-value p))
	 (p0 (aref pp 0))
	 (p1 (aref pp 1))
	 (p2 (aref pp 2))
	 (p3 (aref pp 3))
	 (qq (tuple-value q))
	 (q0 (aref qq 0))
	 (q1 (aref qq 1))
	 (q2 (aref qq 2))
	 (q3 (aref qq 3))) 
    (make-element domain
		  (- (* p0 q0) (+ (+ (* p1 q1) (* p2 q2)) (* p3 q3)))
		  (- (+ (+ (* p1 q0) (* p0 q1)) (* p2 q3)) (* p3 q2))
		  (- (+ (+ (* p2 q0) (* p0 q2)) (* p3 q1)) (* p1 q3))
		  (- (+ (+ (* p3 q0) (* p0 q3)) (* p1 q2)) (* p2 q1)))))
	     
#|  For metricized fields|

(defmethod norm ((q quaternion-domain-element))
  (sqrt (dot-product q q)))

(defmethod normalize ((q quaternion-domain-element))
  (let ((l (norm q))
	(v (tuple-value q)))
    (make-quaternion-domain-element 
     (domain-of q)
     (make-array 4 :initial-contents (list (/ (aref v 0) l)
					   (/ (aref v 1) l)
					   (/ (aref v 2) l)
					   (/ (aref v 3) l))))))
||#

(defmethod create-unit-quaternion
  ((domain unit-quaternion-domain) (v vector-space-element) (angle number))
  (unless (= 3 (dimension-of (domain-of v)))
	  (error "Illegal call to create-unit-quaternion: ~S" v))
  ;; must coerce domains
  (make-element domain
		(cos (/ angle 2)) 
		(* (sin (/ angle 2)) (ref v 0))
		(* (sin (/ angle 2)) (ref v 1))
		(* (sin (/ angle 2)) (ref v 2))))

;;
;; homomorphism SO(3) --> Unit Quaternions
;;

(defmethod coerce ((Q unit-quaternion-domain-element) (domain SO-n))
  (if (eql (dimension-of domain) 3)
      (let* ((q0 (ref Q 0))
	     (q1 (ref Q 1))
	     (q2 (ref Q 2))
	     (q3 (ref Q 3))
	     (q0q0 (* q0 q0))
	     (q0q1 (* q0 q1))
	     (q0q2 (* q0 q2))
	     (q0q3 (* q0 q3))
	     (q1q1 (* q1 q1))
	     (q1q2 (* q1 q2))		;
	     (q1q3 (* q1 q3))
	     (q2q2 (* q2 q2))
	     (q2q3 (* q2 q3))
	     (q3q3 (* q3 q3))
	     (mat (make-array '(3 3))))
	(setf (aref mat 0 0) (+ (* 2 q0q0) (* 2 q1q1) -1))
	(setf (aref mat 1 1) (+ (* 2 q0q0) (* 2 q2q2) -1))
	(setf (aref mat 2 2) (+ (* 2 q0q0) (* 2 q3q3) -1))
	(setf (aref mat 0 1) (* 2 (- q1q2 q0q3)))
	(setf (aref mat 0 2) (* 2 (+ q1q3 q0q2)))
	(setf (aref mat 1 2) (* 2 (- q2q3 q0q1)))
	(setf (aref mat 1 0) (* 2 (+ q1q2 q0q3)))
	(setf (aref mat 2 0) (* 2 (- q1q3 q0q2)))
	(setf (aref mat 2 1) (* 2 (+ q0q1 q2q3)))
	(make-element domain mat))
      (error "Cannot coerce a quaternion in SO(~D)" (dimension-of domain))))

;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;				    Matrices
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; matrix.lisp,v 1.14 1995/05/24 17:49:23 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.14")

;; This is a very general matrix implementation.   At some point it will
;; be worth while implementing some more specialized matrix spaces.

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator matrix-space ((ring ring))
    (make-instance 'matrix-space
                   :coefficient-domain ring
                   :print-function 'make-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'matrix-space)
                        (eql (coefficient-domain-of d) ring)))))

(defun matrix-space-print-object (domain stream)
  (format stream "Mat(~S)" (coefficient-domain-of domain)))

(defmethod make-element ((domain matrix-space) (value array) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'matrix-space-element
		 :domain domain
		 :dimension1 (array-dimension value 0)
		 :dimension2 (array-dimension value 1)
		 :value value))

(defmethod weyl::make-element ((domain matrix-space) (value array)
			       &rest ignore)
  (declare (ignore ignore))
  (let* ((array-dims (array-dimensions value))
	 (x-dim (first array-dims))
	 (y-dim (second array-dims))
	 (coef-domain (coefficient-domain-of domain))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum below x-dim do
	  (loop for j fixnum below y-dim do
		(setf (aref array i j) (coerce (aref value i j) coef-domain))))
    (make-instance 'matrix-space-element
		   :domain domain
		   :dimension1 x-dim 
		   :dimension2 y-dim 
		   :value array)))

(defmethod make-element ((domain matrix-space) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (make-element domain
		(make-array (list (length values) (length (first values)))
			    :initial-contents values)))

(defmethod weyl::make-element ((domain matrix-space) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (let* ((x-dim (length values))
	 (y-dim (length (first values)))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum  below x-dim
	  for row in values do
	    (loop for j fixnum below y-dim
		  for val in row do
		    (setf (aref array i j) val)))
    (weyl::make-element domain array)))

(defgeneric matrix-dimensions (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod matrix-dimensions ((m matrix-space-element))
  (with-slots (dimension1 dimension2) m
    (values dimension1 dimension2)))

(defmethod dimensions ((m matrix-space-element))
  (with-slots (dimension1 dimension2) m
    (list dimension1 dimension2)))

#+Genera
(defmacro with-matrix-dimensions ((dim1 dim2 &optional array) matrix &body body
				  &environment env)
  (scl:once-only (matrix &environment env)
    `(multiple-value-bind (,dim1 ,dim2) (matrix-dimensions ,matrix)
       ,(if array `(let ((,array (matrix-value ,matrix)))
		     ,@body)
	    `(progn ,@body)))))

#-Genera
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro with-matrix-dimensions ((dim1 dim2 &optional array) matrix &body body)
    (let ((decls (list `(fixnum ,dim1 ,dim2))))
      (loop while (and (not (atom (first body)))
                       (eql (first (first body)) 'declare))
            do (setq decls (append (rest (pop body)) decls)))
      `(multiple-value-bind (,dim1 ,dim2) (matrix-dimensions ,matrix)
        (declare ,@decls)
        ,(if array `(let ((,array (matrix-value ,matrix)))
                     ,@body)
             `(progn ,@body))))))

#-Genera
(defmethod print-object ((matrix matrix-space-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (princ "Mat<" stream)
    (loop for i fixnum below dim1
	  do (princ "<" stream)
	     (loop for j fixnum below dim2
		   do (print-object (aref array i j) stream)
		      (if (< (1+ j) dim2)
			  (princ ",  " stream)
			  (princ ">" stream)))	     
	     (if (< (1+ i) dim1)
		 (princ ",  " stream)
		 (princ ">" stream)))))

#+Genera
(defmethod print-object ((matrix matrix-space-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (dw:formatting-table (stream)
      (loop for i below dim1 do
	(dw:formatting-row (stream)
	  (loop for j below dim2 do
	    (dw:formatting-cell (stream :align-x :center)
	      (princ (aref array i j) stream))))))))

(defmethod ref ((matrix matrix-element) &rest args)
  (let ((x (first args))
	(y (second args)))
    (cond ((numberp x)
	   (cond ((numberp y)
		  (aref (matrix-value matrix) x y))
		 ((eql y :*)
		  (with-matrix-dimensions (rows cols array) matrix
		    (declare (ignore rows))
		    (let ((new-array (make-array (list 1 cols))))
		      (loop for j fixnum below cols
			    do (setf (aref new-array 0 j) (aref array x j)))
		      (make-element (domain-of matrix) new-array))))
		 (t (error "Unknown argument to REF(~S ~S)"
			   x y))))
	  ((eql x :*)
	   (cond ((numberp y)		  
		  (with-matrix-dimensions (rows cols array) matrix
		    (declare (ignore cols))
		    (let ((new-array (make-array (list rows 1))))
		      (loop for i fixnum below rows
			    do (setf (aref new-array i 0) (aref array i y)))
		      (make-element (domain-of matrix) new-array))))
		 (t (error "Unknown argument to REF(~S ~S)"
			   x y))))
	  (t (error "Unknown argument to REF(~S ~S)"
		    x y)))))

(defmethod set-ref ((matrix matrix-element) new-value &rest args)
  (setf (aref (matrix-value matrix) (first args) (second args)) new-value))

(defmethod zero-matrix ((domain matrix-space) &optional rank)
  (unless (numberp rank)
    (error "Must specify rank to ZERO-MATRIX (~D)" domain))
  (make-element domain
	       (make-array (list rank rank)
			   :initial-element (zero (coefficient-domain-of domain)))))

(defgeneric one-matrix (domain &optional rank)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod one-matrix ((domain matrix-space) &optional rank)
  (unless (numberp rank)
    (error "Must specify rank to ONE-MATRIX (~D)" domain))
  (let* ((zero (zero (coefficient-domain-of domain)))
	 (one (one (coefficient-domain-of domain)))
	 (array (make-array (list rank rank) :initial-element zero)))
    (loop for i fixnum below rank
	  do (setf (aref array i i) one))
    (make-element domain array)))

(defmethod plus ((m1 matrix-space-element) (m2 matrix-space-element))
  (let ((domain (domain-of m1)))
    (cond ((eql domain (domain-of m2))
	   (with-matrix-dimensions (1dim1 1dim2 1array) m1
	     (with-matrix-dimensions (2dim1 2dim2 2array) m2
	       (unless (and (eql 1dim1 2dim1) (eql 1dim2 2dim2))
		 (error "Trying to add matrices of different dimensions: (~D ~D) and (~D ~D)"
			1dim1 1dim2 2dim1 2dim2))
	       (let ((array (make-array (list 1dim1 1dim2))))
		 (loop for i fixnum below 1dim1 do
		   (loop for j fixnum below 1dim2 do
		     (setf (aref array i j)
			   (+ (aref 1array i j) (aref 2array i j)))))
		 (make-element domain array)))))
	  (t (error "Can't add these matrices")))))

(defmethod difference ((m1 matrix-space-element) (m2 matrix-space-element))
  (let ((domain (domain-of m1)))
    (cond ((eql domain (domain-of m2))
	   (with-matrix-dimensions (1dim1 1dim2 1array) m1
	     (with-matrix-dimensions (2dim1 2dim2 2array) m2
	       (unless (and (eql 1dim1 2dim1) (eql 1dim2 2dim2))
		 (error "Trying to subtract matrices of different dimensions: (~D ~D) and (~D ~D)"
			1dim1 1dim2 2dim1 2dim2))
	       (let ((array (make-array (list 1dim1 1dim2))))
		 (loop for i fixnum below 1dim1 do
		   (loop for j fixnum below 1dim2 do
		     (setf (aref array i j)
			   (- (aref 1array i j) (aref 2array i j)))))
		 (make-element domain array)))))
	  (t (error "Can't subtract these matrices")))))


(defmethod-sd times ((m1 matrix-element) (m2 matrix-element))
  (with-matrix-dimensions (1dim1 1dim2 1array) m1
    (with-matrix-dimensions (2dim1 2dim2 2array) m2
      (unless (eql 1dim2 2dim1)
	(error "Trying to multiply matrices of incompatible dimensions: (~D ~D) and (~D ~D)"
	       1dim1 1dim2 2dim1 2dim2))
      (make-element domain (times-array 1array 1dim1 1dim2 2array 2dim2)))))

(defun array-times (array1 array2)
  (let ((dims1 (array-dimensions array1))
	(dims2 (array-dimensions array2)))
    (unless (and (eql (length dims1) 2)
		 (eql (length dims2) 2)
		 (eql (second dims1) (first dims2)))
      (error "Incompatible array dimensions"))
    (times-array array1 (first dims1) (second dims1) array2 (second dims2))))


(defun times-array (1array 1dim1 1dim2 2array 2dim2)
  (let ((array (make-array (list 1dim1 2dim2))))
    (loop for i fixnum below 1dim1 do
      (loop for j fixnum below 2dim2 do
	(loop for k fixnum below 1dim2
	      for c = (* (aref 1array i k) (aref 2array k j))
		then (+ c (* (aref 1array i k) (aref 2array k j)))
	      finally (setf (aref array i j) c))))
    array))

(defmethod times ((m matrix-space-element) (v free-module-element))
  (matrix-fme-times m v))

(defun matrix-fme-times (m v)  
  (let ((elt-domain (coefficient-domain-of (domain-of m)))
	(vector-space (domain-of v)))
    (cond ((eql elt-domain (coefficient-domain-of vector-space))
	   (with-matrix-dimensions (dim1 dim2 array) m
	     (unless (eql dim2 (dimension-of vector-space))
	       (error "Trying to multiply a matrix and vector of incompatible dimensions: (~D ~D) and ~D"
		      dim1 dim2 (dimension-of vector-space)))
	     (%apply #'make-element
		     (if (cl:= dim1 dim2) vector-space
			 (get-free-module elt-domain dim1))
		     (loop for i fixnum below dim1
			   collect
			   (loop for k fixnum below dim2 
				 for c = (* (aref array i k) (ref v k))
				   then (+ c (* (aref array i k) (ref v k)))
				 finally (return c))))))
	  (t (error "Incompatible arguments: ~S and ~S" m v)))))

(defmethod times ((v free-module-element) (m matrix-space-element))
  (fme-matrix-times v m))

(defun fme-matrix-times (v m)  
  (let ((elt-domain (coefficient-domain-of (domain-of m)))
	(vector-space (domain-of v)))
    (cond ((eql elt-domain (coefficient-domain-of vector-space))
	   (with-matrix-dimensions (dim1 dim2 array) m
	     (unless (eql (dimension-of vector-space) dim1)
	       (error "Trying to multiply a vector and matrix of incompatible dimensions:  ~D and (~D ~D)"
		      (dimension-of vector-space) dim1 dim2))
	     (%apply #'make-element
		     (if (cl:= dim1 dim2) vector-space
			 (get-free-module elt-domain dim2))
		     (loop for i fixnum below dim2
			   collect
			   (loop for k fixnum below dim1
				 for c = (* (ref v k) (aref array k i))
				   then (+ c (* (ref v k) (aref array k i)))
				 finally (return c))))))
	  (t (error "Incompatible arguments: ~S and ~S" v m)))))

(defgeneric transpose (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod transpose ((m matrix-element))
  (let ((domain (domain-of m)))
    (with-matrix-dimensions (dim1 dim2 array) m
      (let ((transpose (make-array (list dim2 dim1))))
	(loop for i fixnum below dim1 do
	  (loop for j fixnum below dim2 do
	    (setf (aref transpose j i) (aref array i j))))
	(make-element domain transpose)))))

(defmethod-sd direct-sum ((x matrix-element) (y matrix-element))
  (with-matrix-dimensions (x-rows x-cols x-array) x
    (with-matrix-dimensions (y-rows y-cols y-array) y
      (cond ((eql x-rows y-rows)
	     (let ((array (make-array (list x-rows (cl:+ x-cols y-cols)))))
	       (loop for i fixnum below x-rows
		     for j fixnum = 0
		     do (loop for k fixnum below x-cols
			      do (setf (aref array i j) (aref x-array i k ))
				 (incf j))
			(loop for k fixnum below y-cols
			      do (setf (aref array i j) (aref y-array i k))
				 (incf j)))
	       (make-element domain array)))
	    (t (error "Incompatable dimensions (~D, ~D) and (~D, ~D)"
		      x-rows x-cols y-rows y-cols))))))

(defmethod recip ((m matrix-element))
  (let ((domain (domain-of m)))
    (with-matrix-dimensions (dim1 dim2 array) m
      (unless (eql dim1 dim2)
	(error "Can't invert a non-square matrix"))
      (let* ((dims (array-dimensions array))
	     (new-array (make-array dims)))
	(loop for i below (first dims) do
	  (loop for j below (second dims) do
	    (setf (aref new-array i j) (aref array i j))))
	(make-element domain
	  (invert-array (coefficient-domain-of domain) new-array))))))

(defgeneric invert-array (domain array &optional into-array)
  (:documentation
   "The purpose of this function is unknown."))

;; Invert an array of elements of domain in an ordered or un-ordered
;; ring. This operation destroys the first array.
(defmethod invert-array (domain array &optional into-array)
  (let ((dimension (array-dimensions array)))
    (unless (and (null (rest (rest dimension)))
		 (eql (first dimension) (second dimension)))
      (error "Wrong dimensions for recip: ~S" array))
    (cond (into-array
	   (unless (eql dimension (array-dimensions into-array))
	     (error "Wrong dimensions for ~S, expected ~S"
		    into-array dimension)))
	  (t (setq into-array (make-array dimension))
	     (loop for i fixnum below (first dimension) 
		   with zero = (zero domain) and one = (one domain) do
	       (loop for j fixnum below (second dimension) do
		 (setf (aref into-array i j) (if (eql i j) one zero))))))
    (setq dimension (first dimension))
    (flet ((exchange-rows (j k)
	     (loop for i fixnum below dimension do
	       (rotatef (aref array j i) (aref array k i))
	       (rotatef (aref into-array j i) (aref into-array k i))))
	   (find-pivot-ordered (i) 
	     (loop for j fixnum upfrom (1+ i) below dimension
		   for elt = (aref array j i)
		   with max = (aref array i i) and row = i do
	       (when (> (abs elt) (abs max))
		 (setq max elt
		       row j))
		   finally  (return (values row max))))
	   (find-pivot-unordered (i)
	     (loop for j fixnum upfrom (1+ i) below dimension
		   for elt = (aref array j i)
		   with max = (aref array i i) and row = i do
	       (when (and (0? max) (not (0? elt)))
		 (setq max elt
		       row j))
		   finally (if (0? max)
			       (error "Matrix is singular")
			       (return (values row max)))))
	   (subtract-rows (row1 row2)
	     (unless (0? (aref array row2 row1))
	       (let ((mult (aref array row2 row1)))
		 (loop for j fixnum upfrom row1 below dimension do
		   (setf (aref array row2 j)
			 (- (aref array row2 j) (* mult (aref array row1 j)))))
		 (loop for j fixnum below dimension do
		   (setf (aref into-array row2 j)
			 (- (aref into-array row2 j) (* mult (aref into-array row1 j)))))))))
      ;; Triangulate
      (loop for i fixnum below dimension do 
	(multiple-value-bind (row pivot)
			     (if (ordered-domain? domain)
				 (find-pivot-ordered i)
			       (find-pivot-unordered i))
	  (unless (eql i row)
	    (exchange-rows i row))
	  ;; Make the pivot 1
	  (unless (1? pivot) 
	    (loop for j fixnum upfrom i below dimension do 
	      (setf (aref array i j) (/ (aref array i j) pivot)))
	    (loop for j below dimension do
	      (setf (aref into-array i j) (/ (aref into-array i j) pivot))))
	  (loop for j fixnum upfrom (1+ i) below dimension do
	    (subtract-rows i j))))

      ;; Backsolve
      (loop for i fixnum downfrom (1- dimension) above -1 do
	(loop for j fixnum downfrom (1- i) above -1 do
	  (subtract-rows i j))))
    into-array))

(defmethod substitute ((values list) (variables list) (m matrix-space-element)
		       &rest ignore)
  (declare (ignore ignore))
  (with-matrix-dimensions (dim1 dim2 array) m
     (let ((new-array (make-array (list dim1 dim2))))
       (loop for i fixnum below dim1 do
	 (loop for j fixnum below dim2 do
	   (setf (aref new-array i j)
		 (substitute values variables (aref array i j)))))
       (make-element (get-matrix-space (domain-of (aref new-array 0 0)))
		     new-array))))

(defmethod jacobian ((function-list list) (var-list list))
  (let* ((ring (domain-of (first function-list)))
	 (dim-col (length var-list))
	 (dim-row (length function-list))
	 (array (make-array (list dim-row dim-col))))
    (loop for poly in function-list
	  for i fixnum below dim-row
	  do (loop for var in var-list
		   for j fixnum  below dim-col
		   do (setf (aref array i j) (partial-deriv poly var))))
    (make-element (get-matrix-space ring) array)))

;; Matrix Groups

;;; ==========================================================================
;;; The Groups GL(n), SL(n), PSL(n), O(n), SO(n) with the following
;;; hierarchy:
;;;  
;;;  det<>0  GL(n)
;;;            |
;;;            |
;;;  det=+-1 PSL(n) -------------> O(n) M*M^t = In
;;;            |                    |
;;;            V                    |
;;;  det=1   SL(n)                  |
;;;            \                   /
;;;              \               /
;;;                \           /
;;;                  \       /
;;;                    \   /
;;;                    SO(n)
;;;
;;;
;;; ==========================================================================


;; The coefficient domain of GL-n must be a field otherwise, it will
;; not be a group.  This is not necessary for the other matrix groups
;; because the determinants are required to be units.

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator GL-n ((domain field) dimension)
    (make-instance 'GL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'GL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'GL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun GL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "GL^~D(~S)" n (coefficient-domain-of domain))))

(defmethod print-object ((matrix GL-n-element) stream)
  (with-matrix-dimensions (dim1 dim2 array) matrix
    (format stream "~A<" (class-name (class-of (domain-of matrix))))
    (loop for i fixnum below dim1
	  do (princ "<" stream)
	     (loop for j fixnum below dim2
		   do (princ (aref array i j) stream)
		      (if (< (1+ j) dim2)
			  (princ ",  " stream)
			  (princ ">" stream)))	     
	     (if (< (1+ i) dim1)
		 (princ ",  " stream)
		 (princ ">" stream)))))

(define-domain-element-classes GL-n GL-n-element)

(defmethod matrix-dimensions ((m GL-n-element))
  (let ((dim (dimension-of (domain-of m))))
    (values dim dim)))

(defmethod make-element ((domain GL-n) (value array) &rest ignore)
  (declare (ignore ignore))
  (make-instance (first (domain-element-classes domain))
		 :domain domain :value value))

(defmethod weyl::make-element ((domain GL-n) (value array)
			       &rest ignore)
  (declare (ignore ignore))
  (destructuring-bind (x-dim y-dim) (array-dimensions value)
    (let ((coef-domain (coefficient-domain-of domain))
	  (array (make-array (list x-dim y-dim))))
      (loop for i below x-dim do
	(loop for j below y-dim do
	  (setf (aref array i j) (coerce (aref value i j) coef-domain))))
      (make-element domain value))))

(defmethod make-element ((domain GL-n) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (make-element domain
		(make-array (list (length values) (length (first values)))
			    :initial-contents values)))

(defmethod weyl::make-element ((domain GL-n) (value list) &rest values)
  (setq values (if (null values) value
		   (cons value values)))
  (unless (loop for row in (rest values)
		with n = (length (first values))
		do (unless (eql (length row) n)
		     (return nil))
		finally (return t))
    (error "All rows not the same length: ~S" values))
  (let* ((x-dim (length values))
	 (y-dim (length (first values)))
	 (array (make-array (list x-dim y-dim))))
    (loop for i fixnum  below x-dim
	  for row in values do
	    (loop for j fixnum below y-dim
		  for val in row do
		    (setf (aref array i j) val)))
    (make-element domain array)))

(defmethod one-matrix ((domain GL-n) &optional rank)
  (let ((computed-rank (dimension-of domain)))
    (if rank
	(if (not (eq rank computed-rank))
	    (error "rank argument conflicts with domain dimension")))
    (let* ((zero (zero (coefficient-domain-of domain)))
	   (one (one (coefficient-domain-of domain)))
	   (array (make-array (list computed-rank computed-rank)
			      :initial-element zero)))
      (loop for i fixnum below computed-rank do
	(setf (aref array i i) one))
      (make-element domain array))))

(defmethod one ((domain GL-n))
  (one-matrix domain))

(defmethod times ((m GL-n-element) (v free-module-element))
  (matrix-fme-times m v))

(defmethod times ((v free-module-element) (m GL-n-element))
  (fme-matrix-times v m))

;;
;; PSL(n) : group of matrices with determinant +1 or -1
;;

(defun PSL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "PSL^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes PSL-n PSL-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator PSL-n ((domain field) dimension)
    (make-instance 'PSL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'PSL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'PSL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))
;;
;; SL(n) : group of matrices with determinant +1 
;;


(defun SL-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "SL^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes SL-n SL-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator SL-n ((domain field) dimension)
    (make-instance 'SL-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'SL-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'SL-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defmethod determinant ((m SL-n-element))
  (one (coefficient-domain-of (domain-of m))))

;;
;; O(n) : group of orthogonal matrices
;;


(defun O-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "O^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes O-n O-n-element)

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator O-n ((domain field) dimension)
    (make-instance 'O-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'O-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'O-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))
;;
;; SO(n) : orthogonal matrices with unit determinant
;;

(defun SO-n-print-object (domain stream)
  (let ((n (dimension-of domain)))
    (format stream "SO^~D(~S)" n (coefficient-domain-of domain))))

(define-domain-element-classes SO-n SO-n-element)

(defmethod recip ((m SO-n-element))
  (transpose m))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator SO-n ((domain field) dimension)
    (make-instance 'SO-n
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'SO-n-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'SO-n)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))



;;; ====================================================================
;;; Routines for obtaining determinants and subdeterminants of matrices
;;; ====================================================================

(defvar *work-array*)

(defvar *work-matrix*)

(defmacro with-open-modular-arith (p &body body)
  `(let ((.prime. ,p)
	 temp)
	(macrolet ((c+ (x y)
		     `(progn (setq temp (cl:+ (the fixnum ,x) (the fixnum ,y)))
			     (if (cl:> temp .prime.) (cl:- temp .prime.)
				 temp)))
		   (c- (x y)
		     `(cl:mod (cl:- (the fixnum ,x) (the fixnum ,y))
			      .prime.))
		   (c* (x y)
		     `(the fixnum (cl:mod (cl:* (the fixnum ,x)
						(the fixnum ,y))
					  .prime.))))
	  temp
	  ,@body)))

(defgeneric determinant (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric determinant* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod determinant ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (/= dim1 dim2)
	(error "Matrix is not square: Can't compute the determinant"))
    (setq *work-array* (make-array (list dim1 dim2)))
    (determinant* (coefficient-domain-of (domain-of m)) m)))

(defmethod determinant* ((domain polynomial-ring) (m matrix-element))
  (setq *work-matrix* m)
  (interpolate domain 'Evaluate-matrix (degree-bounds m)))

(defgeneric degree-bounds (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod degree-bounds ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m  
    (let ((vars (ring-variables (coefficient-domain-of (domain-of m))))
	  ring-var d)
	 (loop for var in vars do
	   (setq ring-var (coerce var (coefficient-domain-of (domain-of m))))
	   (setq d (degree (aref array 0 0) ring-var))
	   (loop for i below dim1 do
	     (loop for j below dim2 do
	       (if (> (degree (aref array i j) ring-var) d)
		   (setq d (degree (aref array i j)ring-var)))))
	   collect (* (min dim1 dim2) d)))))

(defmethod substitute ((values list) (variables list) (m matrix-element)
		       &rest ignore)
  (declare (ignore ignore))
  (if (not (typep (coefficient-domain-of (domain-of m))
		  'multivariate-polynomial-ring))
      (error "Expected ~S to be over a multivariate-polynomial-ring" m))
  (with-matrix-dimensions (dim1 dim2 array) m
    (let ((new-array (make-array (list dim1 dim2))))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (setf (aref new-array i j)
		   (substitute values variables (aref array i j)))))
	 (make-element (domain-of m) new-array))))

(defmethod coerce ((m matrix-element) (domain matrix-space))
  (with-matrix-dimensions (dim1 dim2 array) m
    (let ((new-array (make-array (list dim1 dim2))))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (setf (aref new-array i j)
		   (coerce (coerce (aref array i j) *general*)
			   (coefficient-domain-of domain)))))
	 (make-element domain new-array))))

;; Used as a black box for sparse multivariate interpolation.
;; Note the use of *work-matrix*.
(defmethod evaluate-matrix ((vals list))
  (let ((poly-ring (coefficient-domain-of (domain-of *work-matrix*)))
	(coef-domain (domain-of (first vals)))
	(domain (get-matrix-space (domain-of (first vals)))))
       (cond ((eql (coefficient-domain-of poly-ring) coef-domain)
	      (determinant
		(coerce
		  (substitute
		    (list-coerce vals poly-ring)
		    (list-coerce (ring-variables poly-ring) poly-ring)
		    *work-matrix*)
		  domain)))
	     (t (setq poly-ring (get-polynomial-ring
				  coef-domain
				  (ring-variables poly-ring)))
		(determinant
		  (coerce
		    (substitute
		      (list-coerce vals poly-ring)
		      (list-coerce (ring-variables poly-ring) poly-ring)
		      (coerce *work-matrix* (get-matrix-space poly-ring)))
		    domain))))))

(defmethod determinant* ((domain rational-integers) (m matrix-element))
  (loop for p in (choice-primes (hadamard-bound m))
	collect
	(make-element (get-finite-field p) 
	  (determinant (weyl:make-element
			   (get-matrix-space
			     (get-finite-field p)) (matrix-value m))))
	  into remainders
	finally (return (compute-result (use-chinese-remainder remainders)))))

(defgeneric hadamard-bound (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod hadamard-bound ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m  
    (let ((d (aref array 0 0))
	  (m (min dim1 dim2)))
	 (loop for i below dim1 do
	   (loop for j below dim2 do
	     (if (> (aref array i j) d)
		 (setq d (aref array i j)))))
	 (* (expt m m) (expt d m)))))

(defmethod  determinant* ((domain GFp) (m matrix-element))
  (with-matrix-dimensions (dimension dim2 old-array) m
    (declare (ignore dim2))
    (let* ((determinant 1)
	   (sign 1)
	   (modulus (characteristic domain)))
    (flet ((exchange-rows (i j)
	     (loop for l from j below dimension do
	       (rotatef (aref *work-array* i l) (aref *work-array* j l)))
	     (setq sign (* sign -1)))
	   (find-pivot (j)
	     (loop for i fixnum upfrom j below dimension do
	       (if (not (= (aref *work-array* i j) 0))
		   (return i))
	       finally (return -1))))
    (loop for i fixnum below  dimension do
      (loop for j fixnum below dimension do
	(setf (aref *work-array* i j) (gfp-value (aref old-array i j)))))
    (with-open-modular-arith modulus
      (loop for j below (- dimension 1)
	    with row of-type fixnum and pivot fixnum and d do
	      (setq row (find-pivot j))
	      (if (/= row -1)
		  (setf determinant (c* determinant (aref *work-array* row j)))
		  (return (* sign determinant)))
	      (setq pivot (aref *work-array* row j))
	      (setq d (reduce-modulo-integer
		       (compute-inverse pivot modulus)
			     modulus))
;	    (format t "pivot = ~D d = ~D~%" pivot d)
	      (if (> row j)
		  (exchange-rows row j))
	      (loop for k upfrom (cl:+ j 1) below dimension
		    with ck do
		      (setq ck (c* d (aref *work-array* k j)))
;		    (format t "ck = ~D~%" ck)
		      (loop for l of-type fixnum upfrom j below dimension do
			(setf (aref *work-array* k l)
			      (c- (aref *work-array* k l)
				  (c* ck (aref *work-array* j l))))))
;	    (print *work-array*)
	    finally
	 (setq determinant (c* determinant (aref *work-array* j j))))))
    (* sign determinant))))

(defmethod determinant* ((domain field) (m matrix-element))
  (with-matrix-dimensions (dimension dim2 old-array) m
    (declare (ignore dim2))
    (let* ((determinant (one domain))
	   (sign (one domain)))
	  (flet ((exchange-rows (i j)
		   (loop for l from j below dimension do
		     (rotatef (aref *work-array* i l) (aref *work-array* j l)))
		   (setq sign (* sign (minus (one domain)))))
		 (find-pivot (j)
	           (loop for i fixnum upfrom j below dimension do
		     (if (not (0? (aref *work-array* i j)))
			 (return i))
			 finally
		      (return -1))))
          (loop for i fixnum below  dimension do
	    (loop for j fixnum below dimension do
	      (setf (aref *work-array* i j) (aref old-array i j))))
		;; Triangulate
	   (loop for j fixnum below (- dimension 1)
		 with row and pivot and d do
		   (setq row (find-pivot j))
		   (if (/= row -1)
		       (setf determinant (* determinant
					    (aref *work-array* row j)))
		       (return (zero domain)))
		   (setq pivot (aref *work-array* row j))
		   (setq d (recip pivot))
		   ;; (print (format nil "pivot = ~D d = ~D" pivot d))
		   (if (> row j)
		       (exchange-rows row j))
		   (loop for k fixnum upfrom (+ j 1) below dimension
			 with ck do
			   (setq ck (* d (aref *work-array* k j)))
			   ;; (print (format nil "ck = ~D" ck))
			   (loop for l fixnum upfrom j below dimension do
			     (setf (aref *work-array* k l)
				   (- (aref *work-array* k l)
				      (* ck (aref *work-array* j l))))))
		   ;; (print *work-array*)
		 finally
	      (return (* sign determinant (aref *work-array* j j))))))))

;; Use this method to find determinant only if the matrix is very sparse.
;; It computes determinant by expansion of the minors, so it is very slow
;; unless the matrix is very sparse.
(defmethod sparse-determinant ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (if (/= dim1 dim2)
	(error "Matrix is not square: Can't compute the determinant"))

    (let* ((domain (domain-of m))
	   (coefficient-domain (coefficient-domain-of domain))
	   (one (one coefficient-domain))	   
	   (zero (zero coefficient-domain)))
      (labels ((sparse-det (row cols)
		 #+ignore
	         (format t "in minor: row = ~D, cols = ~S~%" row cols)
		 (memoize `(sparse-det ,m ,row ,cols)
		   (if (null cols) one
		       (loop for col in cols
			     for positive-sign? = t then (not positive-sign?)
			     with minor and det = zero
			     do (unless (0? (aref array row col))
				  (setq minor 
					(* (aref array row col)
					   (sparse-det (1+ row) (remove col cols))))
				  (setq det
					(if positive-sign?
					    (+ det minor)
					    (- det minor))))
			     finally (return det))))))
    (sparse-det 0 (loop for i below dim1 collect i))))))

(defgeneric independent-rows (array)
  (:documentation
   "The purpose of this function is unknown."))

;; Independent rows of a LISP array are obtained by transforming it
;; into row echelon form.
(defmethod independent-rows (array)
  (let ((dim1 (first (array-dimensions array)))
	(dim2 (second (array-dimensions array))))
       (flet ((find-pivot (i)
		     (loop for j fixnum below dim2
		       do (if (not (0? (aref array i j)))
			      (return j))
			   finally
			(return -1))))
	     (loop for i fixnum below dim1 with col and pivot
		   do (setq col (find-pivot i))
		      (cond ((/= col -1)
			     (setq pivot (recip (aref array i col)))
			     (loop for k fixnum upfrom (+ i 1) below dim1
				   with ck
				   do (setq ck (* pivot (aref array k col)))
				      (loop for l fixnum upfrom col below dim2
					    do (setf (aref array k l)
						     (- (aref array k l)
							(* ck (aref array i l)))))))))
	     (loop for i below dim1
		   if (/= (find-pivot i) -1)
		     collect i))))

(defgeneric independent-cols (array)
  (:documentation
   "The purpose of this function is unknown."))

;; independent columns of the array are abtained by transforming it into
;; column echelon form.
(defmethod independent-cols (array)
  (let ((dim1 (first (array-dimensions array)))
	(dim2 (second (array-dimensions array))))
       (flet ((find-pivot (j)
			  (loop for i fixnum below dim1
			    do (if (not (0? (aref array i j)))
				   (return i))
				finally
			     (return -1))))
	     (loop for j fixnum below dim2
		   with row and pivot do
		     (setq row (find-pivot j))
		     (cond ((/= row -1)
			    (setq pivot (recip (aref array row j)))
			    (loop for l fixnum upfrom (+ j 1) below dim2
				  with ck
				  do (setq ck (* pivot (aref array row l)))
				     (loop for k fixnum upfrom row below dim1
					   do (setf (aref array k l)
						    (- (aref array k l)
						       (* ck (aref array k j)))))))))
	     (loop for j below dim2
		   if (/= (find-pivot j) -1)
		     collect j))))

(defgeneric subdeterminant (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric subdeterminant* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod subdeterminant ((m matrix-element))
  (subdeterminant* (coefficient-domain-of (domain-of m)) m))

(defmethod subdeterminant* ((domain field) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (setq *work-array* (make-array (list dim1 dim2)))
    (let ((rows '())
	  (cols '())
	  (new-array nil))
	 (loop for i below dim1
	       do (loop for j fixnum below dim2
			do (setf (aref *work-array* i j) (aref array i j))))
	 (setq rows (independent-rows *work-array*))
	 (loop for i below dim1
	       do (loop for j of-type fixnum below dim2
			do (setf (aref *work-array* i j) (aref array i j))))
	 (setq cols (independent-cols *work-array*))
	 (if (/= (length rows) (length cols))
	     (error "Internal error: row rank is not equal to column rank"))
	 (setq new-array (make-array (list (length rows) (length cols))))
	 (loop for i upfrom 0 as row in rows
	       do (loop for j upfrom 0 as col in cols
			do (setf (aref new-array i j)
				 (aref array row col))))
	 (values (determinant* domain
			       (weyl:make-element (domain-of m)
				 new-array))
		 new-array
		 rows))))

;; First, find a prime for which the rank of the matrix m is maximum.
;; Then obtain independent rows and columns for that prime to obtain
;; the largest nonsingular submatrix.
(defmethod subdeterminant* ((domain rational-integers) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (setq *work-array* (make-array (list dim1 dim2)))
    (let ((primes (choice-primes (hadamard-bound m)))
	  (gfp nil)
	  (rows '())
	  (temp-rows '())
	  (cols '())
	  (prime-of-maxrank nil)
	  (new-array nil))
	 (loop for p in primes
	       do (setq gfp (get-finite-field p))
		  (loop for i below dim1
			do (loop for j fixnum below dim2
				 do (setf (aref *work-array* i j)
					  (coerce (aref array i j) gfp))))
		  (setq temp-rows (independent-rows *work-array*))
		  (cond ((> (length temp-rows) (length rows))
			 (setq rows temp-rows)
			 (setq prime-of-maxrank p))))
	 (setq gfp (get-finite-field prime-of-maxrank))
	 (loop for i below dim1
	       do (loop for j below dim2
			do (setf (aref *work-array* i j)
				 (coerce (aref array i j) gfp))))
	 (setq cols (independent-cols *work-array*))
	 (if (/= (length rows) (length cols))
	     (error "Internal error: row rank is not equal to column rank"))
	 (setq new-array (make-array (list (length rows) (length cols))))
	 (loop for i upfrom 0 as row in rows
	       do (loop for j upfrom 0 as col in cols
			do (setf (aref new-array i j)
				 (aref array row col))))
	 (values (determinant* domain
			       (weyl:make-element (domain-of m)
				 new-array))
		 new-array
		 rows))))

;;; ====================================================================
;;; Routines for obtaining Hermite and Smith normal forms of matrices
;;; over the ring of integers.
;;; ====================================================================

(defgeneric hermite* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod hermite ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (> dim1 dim2)
	(setq m (transpose m)))
    (hermite* (coefficient-domain-of (domain-of m)) m)))

(defmethod hermite* ((domain rational-integers) (m matrix-element))
  (with-matrix-dimensions (dim1 dim2 array) m
    (declare (ignore dim1))
    (multiple-value-bind (R new-array rows) (subdeterminant m)
      (declare (ignore new-array))
      (let ((A (make-array (list (length rows) dim2))))
	   (loop for i upfrom 0 as row in rows
		 do (loop for j below dim2
			  do (setf (aref A i j)
				   (integer-value (aref array row j)))))
	   (weyl:make-element (domain-of m) (hermite-array A (abs R)))))))

;; Algorithm 2.4.8 on page 71 of the book by Cohen.
(defun hermite-array (A R)
  (let* ((m (first (array-dimensions A)))
	 (n (second (array-dimensions A)))
	 (B (make-array m))
	 (W (make-array (list m m)))
	 (j (- n 1))
	 (k j)
	 u v d q)
	(loop for i downfrom (- m 1) downto 0 by 1 do
	  (loop while (> j 0) do
	    (setf j (- j 1))
	    (cond
	      ((/= (aref A i j) 0)
	       (if (= (aref A i k) 0)
		   (setf (aref A i k) R))
	       (multiple-value-setq
		   (u v d) (extended-gcd (aref A i k) (aref A i j)))
	       (loop for l below m do
		 (setf (aref B l)
		       (cl:+ (cl:* u (aref A l k)) (cl:* v (aref A l j))))
		 (setf (aref A l j)
		       (sym-mod (cl:- (cl:* (cl:/ (aref A i k) d) (aref A l j))
				      (cl:* (cl:/ (aref A i j) d) (aref A l k)))
			   R))
		 (setf (aref A l k) (sym-mod (aref B l) R))))))
	  (multiple-value-setq (u v d) (extended-gcd (aref A i k) R))
	  (loop for l below m do
	    (setf (aref W l i) (mod (cl:* u (aref A l k)) R)))
	  (if (= (aref W i i) 0)
	      (setf (aref W i i) R))
	  (loop for j from (1+ i) below m do
	    (setf q (floor (cl:/ (aref W i j) (aref W i i))))
	    (loop for l below m do
	      (setf (aref W l j) (cl:mod (cl:- (aref W l j)
					       (cl:* q (aref W l i)))
				      R))))
	  (setf R (cl:/ R d))
	  (decf k)
	  (setf j k))
	W))

(defgeneric smith (matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defgeneric smith* (domain matrix)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod smith ((m matrix-element))
  (with-matrix-dimensions (dim1 dim2) m
    (if (> dim1 dim2)
	(setq m (transpose m)))
    (smith* (coefficient-domain-of (domain-of m)) m)))

(defmethod smith* ((domain rational-integers) (m matrix-element))
  (multiple-value-bind (R array) (subdeterminant m)
    (let ((A (make-array (array-dimensions array))))
	 (loop for i below (first (array-dimensions array))
	       do (loop for j below (second (array-dimensions array))
			do (setf (aref A i j)
				 (integer-value (aref array i j)))))
	 (smith-array A (abs R)))))

;; Algorithm 2.4.14 on page 77 of the book by Cohen.
(defun smith-array (A R)
  (let* ((n (first (array-dimensions A)))
	 (B (make-array n))
	 (Diagonal (make-array n))
	 (foundb '())
	 (failed '())
	 u v d b1 k l)
    (flet
      ((processi (i)
	 (let ((j i)
	       (c 1))
          (loop while (> c 0) do
	    (setf c 0)
	    (loop while (> j 0) do
	      (setf j (- j 1))
	      (cond
		((/= (aref A i j) 0)
		 (multiple-value-setq
		     (u v d) (extended-gcd (aref A i i) (aref A i j)))
		 (loop for l below n do
		   (setf (aref B l)
			 (cl:+ (cl:* u (aref A l i)) (cl:* v (aref A l j))))
		   (setf (aref A l j)
			 (sym-mod
			  (cl:- (cl:* (cl:/ (aref A i i) d) (aref A l j))
				(cl:* (cl:/ (aref A i j) d) (aref A l i)))
			     R))
		   (setf (aref A l i) (sym-mod (aref B l) R))))))
	    (setf j i)
	    (loop while (> j 0) do
	      (setf j (- j 1))
	      (cond
		((/= (aref A j i) 0)
		 (multiple-value-setq
		     (u v d) (extended-gcd (aref A i i) (aref A j i)))
		 (loop for l below n do
		   (setf (aref B l)
			 (cl:+ (cl:* u (aref A i l)) (cl:* v (aref A j l))))
		   (setf (aref A j l)
			 (sym-mod (cl:- (cl:* (cl:/ (aref A i i) d)
					      (aref A j l))
				     (cl:* (cl:/ (aref A j i) d) (aref A i l)))
			     R))
		   (setf (aref A i l) (sym-mod (aref B l) R)))
		 (incf c))))))))
    (if (= n 1)
	(setf (aref Diagonal 0) R))
    (loop for i downfrom (- n 1) above 0 by 1 do
      (setf foundb '())
      (loop while (not foundb) do
	(processi i)
	(setf b1 (if (= (aref A i i) 0) R (aref A i i)))
	(setf k (- i 1))
	(setf l (- i 1))
	(setf failed '())
	(loop while (and (>= k 0) (not failed)) do
	  (loop while (and (>= l 0) (not failed)) do
	    (cond ((/= (rem (aref A k l) b1) 0)
		   (setf failed 1)
		   (loop for m below n do
		     (setf (aref A i m) (cl:+ (aref A i m) (aref A k m))))))
	    (decf l))
	  (setf l (- i 1))
	  (decf k))
	(if (not failed)
	    (setf foundb 1)))
      (setf (aref Diagonal i) (gcd (aref A i i) R))
      (setf R (cl:/ R (aref Diagonal i))))
    (setf (aref Diagonal 0) (gcd (aref A 0 0) R))
    Diagonal)))
;;; -*- Base: 10; Mode: Lisp; Syntax: Common-lisp; Lowercase: T -*-

;;; ===========================================================================
;;;			 Topology and the Basics of Geometry
;;; ===========================================================================
;;; (c) Copyright 1994 Cornell University

;;; topology.lisp,v 1.37 1995/06/05 20:24:49 rick Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.37")

;; The following declaration causes all elements of euclidean spaces
;; to be points.
(define-domain-element-classes euclidean-space point)

;; This is needed to avoid a precedence problem. 
(defmethod make-element ((domain euclidean-space) (value vector) &rest values)
  (declare (ignore values))
  (make-element-free-module-vector domain value))

#+ignore
(defmethod print-object ((elt euclidean-space-element) stream)
  (print-free-module-element elt stream))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator euclidean-space (dimension &optional (domain *general*))
    (make-instance 'euclidean-space 
                   :coefficient-domain domain
                   :dimension dimension
                   :print-function 'euclidean-space-print-object)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'euclidean-space)
                        (eql (coefficient-domain-of d) domain)
                        (eql (dimension-of d) dimension)))))

(defun euclidean-space-print-object (domain stream)
  (format stream #+Genera "E~D" #-Genera "E^~D"
	  (dimension-of domain)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Points.
;;
(defmethod-sd binary= ((p1 abstract-point) (p2 abstract-point))
  (cl:= (id-number-of p1) (id-number-of p2)))

(defmethod-sd binary= ((p1 point) (p2 point))
  (let ((p1-tuple (tuple-value p1))
	(p2-tuple (tuple-value p2)))
    (loop for i fixnum below (array-dimension p1-tuple 0)
	  do (unless (= (svref p1-tuple i) (svref p2-tuple i))
	       (return nil))
	     finally (return t))))

(defgeneric make-point (domain value &rest values)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-point ((domain vector-space) (value vector) &rest values)
  (let ((coef-domain (coefficient-domain-of domain)))
    (unless (and (eql (array-dimension value 0) (dimension-of domain))
    	     (null values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance 'point :domain domain
    	       :values (%apply #'vector
    			       (loop for i fixnum below (length value)
    				     collect (coerce (aref value i)
    						     coef-domain))))))

(defmethod make-point ((domain vector-space) value &rest values)
  (let ((coef-domain (coefficient-domain-of domain)))
    (unless (eql (1- (dimension-of domain)) (length values))
      (error "Wrong number of vector elements in ~S" domain))
    (make-instance 'point :domain domain
    	       :values (%apply #'vector
    			       (coerce value coef-domain)
    			       (loop for v in values
    				     collect (coerce v coef-domain))))))

(defmethod make-point ((domain vector-space) (value vector-space-element)
    		   &rest values)
  (apply #'make-point domain (tuple-value value) values))

(defmethod make-point ((domain abstract-space) value &rest values)
  (declare (ignore values))
  (if (null value)
      (make-instance 'abstract-point :domain domain)
      (make-instance 'named-point :domain domain :name value)))

(defmethod print-object ((point named-point) stream)
  (format stream "<~A>" (name-of point)))

(defmethod print-object ((point abstract-point) stream)
  (format stream "<~S>" (id-number-of point)))

;;  #P appears before coordinates of points.
(defmethod print-object ((point point) stream)
  (format stream "#P") (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cells.

;;  Had to use defmethod instead of defmethod-sd.  The -sd version
;;  checks domains and cells don't have domains.
(defmethod binary= ((cell1 cell) (cell2 cell))
  (equal (cell-id cell1) (cell-id cell2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simplices.

(defgeneric make-simplex (point &rest rest)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-simplex ((point abstract-point) &rest rest)
  (loop with domain = (domain-of point)
	for other in rest
	if (not (eql domain (domain-of other)))
	  do
       (error "Cannot make simplex with points from differing domains. ~s"
	      (cons point rest)))
  (make-instance 'simplex :vertices (cons point (copy-list rest))))

(defmethod print-object ((simplex simplex) stream)
  (format stream "[~S~{, ~S~}]"
	  (first (vertices-of simplex)) (rest (vertices-of simplex))))

(defgeneric cell-id (simplex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod cell-id ((simplex simplex))
  (mapcar #'id-number-of (vertices-of simplex)))

(defmethod facets ((simplex simplex) (complex (eql nil)))
  (with-slots (vertices) simplex
    (if (rest vertices)
	(let ((f-list nil))
	  (choose (vertices-of simplex) (facet (dimension-of simplex))
		  (push (apply #'make-simplex facet) f-list))
	  f-list))))

(defmethod dimension-of ((s simplex))
  (- (length (vertices-of s)) 1))

;;  Return the list of vertices opposite the given face of the
;;  simplex.  The input face can be a simplex, a list of vertices, or
;;  a single point.
(defmethod opposite ((face simplex) (simplex simplex))
  (set-difference (vertices-of simplex) (vertices-of face)))

(defmethod opposite ((face list) (simplex simplex))
  (set-difference (vertices-of simplex) face))

(defmethod opposite ((face point) (simplex simplex))
  (remove face (vertices-of simplex)))

(defgeneric face? (points simplex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod face? ((points list) (simplex simplex))
  (subsetp points (vertices-of simplex)))

(defmethod face? ((simplex1 simplex) (simplex2 simplex))
  (subsetp (vertices-of simplex1) (vertices-of simplex2)))

(defun segment? (thing)
  (and (typep thing 'simplex) (= (length (vertices-of thing)) 2)))

(defun triangle? (thing)
  (and (or (typep thing 'simplex) (typep thing 'polygon))
       (= (length (vertices-of thing)) 3)))

(defun tetrahedron? (thing)
  (and (typep thing 'simplex) (= (length (vertices-of thing)) 4)))

(defun sign-of-permutation (lista listb)
  (cond ((and (null lista) (null listb)) 1)
	((eql (first lista) (first listb))
	 (sign-of-permutation (rest lista) (rest listb)))
	((member (first lista) (rest listb))
	 (* -1 (sign-of-permutation
		(rest lista)
		(substitute (first listb) (first lista) (rest listb)))))
	(t 0)))

(defmacro map-over-oriented-facets ((facet orientation simplex complex)
				    &rest body)
  "Map over the faces of SIMPLEX, taking ORIENTATION into account"
  `(loop  
    for ,facet in (facets ,simplex ,complex)
    for ,orientation in (if (orient-of ,simplex)
			    '(t nil t nil)
			  '(nil t nil t))
    do
    ,@body))

(defmacro map-over-oriented-cofacets ((cofacet orientation simplex complex)
				      &rest body)
  "Map over the faces of SIMPLEX, taking ORIENTATION into account"
  ;;; this is pretty ugly.  Could be fixed either by keeping
  ;;; orientation info with cofacets, or by iterating over the cells
  ;;; of 1 higher dimension higher up in the calling seequence
  (let ((facet (gensym)))
    `(loop  
      for ,cofacet in (cofacets ,simplex ,complex)
      for ,orientation = (map-over-oriented-facets
			  (,facet ,orientation ,cofacet ,complex)
			  (if (eql ,facet ,simplex)
			      (return ,orientation)))
      do
      ,@body)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Polygons.

(defmethod cell-id ((polygon polygon))
  (let ((id-list (mapcar #'id-number-of (vertices-of polygon))))
    (if (= 3 (length (vertices-of polygon)))
	id-list
	(cons :p id-list))))	       ; Uses :p to distinguish from simplices.

(defmethod facets ((polygon polygon) (complex (eql nil)))
  (loop with vertices = (vertices-of polygon)
	for a in (cons (first (last vertices)) vertices) and b in vertices
	collect (make-simplex a b)))

(defmethod dimension-of ((polygon polygon))
  2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Cell-Complexes.

(defgeneric get-cell (cell complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-cell ((cell cell) (complex cell-complex))
  (if (member cell complex) cell
      (gethash (cell-id cell) (cell-table-of complex))))

;;  Allow get-cell to take a list of vertices (representing a simplex).
(defmethod get-cell ((vertex-list list) (complex cell-complex))
  (gethash (sort (mapcar #'id-number-of vertex-list) #'cl:<)
	   (cell-table-of complex)))

(defmethod member ((cell cell) (complex cell-complex) &rest ignore)
  (declare (ignore ignore))
  (second (multiple-value-list (gethash (id-number-of cell)
					(facet-table-of complex)))))

(defgeneric facets (cell complex)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod facets ((cell cell) (complex cell-complex))
  (gethash (id-number-of cell) (facet-table-of complex)))

(defmethod facets ((cells list) (complex cell-complex))
  (loop for cell in cells
	append (facets cell complex) into facet-list
	finally (return (remove-duplicates facet-list))))

(defgeneric cofacets (cell complex)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod cofacets ((cell cell) (complex cell-complex))
  (gethash (id-number-of cell) (cofacet-table-of complex)))

(defmethod cofacets ((cells list) (complex cell-complex))
  (loop for cell in cells
	append (cofacets cell complex) into cofacet-list
	finally (return (remove-duplicates cofacet-list))))

(defgeneric maximal-cell? (cell complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod maximal-cell? ((cell cell) (complex cell-complex))
  (and (member cell complex) (null (cofacets cell complex))))

;;  Destructive modification of cell-complex.
(defmethod insert ((cell cell) (complex cell-complex) &rest ignore)
  (declare (ignore ignore))
  (with-slots (cell-table facet-table cofacet-table) complex
    ;;  Internal insert.  Checking only needs to be done on insert;
    ;;  %insert does no checking.  Implements distinction between
    ;;  user-level insert and internal insert; nice for triangulations
    ;;  where user-level inserts only triangles, while internal
    ;;  inserts can do 1- and 0-simplices.  If there is already an
    ;;  equivalent cell in the complex then we return the equivalent
    ;;  cell (and do no insertion); otherwise we return the newly
    ;;  inserted cell.  [May want to make %insert a method on its own
    ;;  at some point.]
    (labels ((%insert (cell complex)
	       (or (get-cell cell complex)
		   (loop with facets-list
			 for facet in (facets cell nil)
			 do (setf facet (%insert facet complex))
			    (push facet facets-list)
			    (push cell (gethash (id-number-of facet)
						cofacet-table))
			 finally
		      (setf (gethash (cell-id cell) cell-table) cell)
		      (setf (gethash (id-number-of cell) facet-table)
			    facets-list)
		      (return cell)))))
      (%insert cell complex))))

;;  Destructive modification.  Can only delete a maximal cell.
(defmethod delete-maximal-cell ((cell cell) (complex cell-complex))
  (unless (member cell complex)
    (error "Cannot delete ~s from ~s.  It is not a member." cell complex))
  (when (cofacets cell complex)
    (error "Cannot delete a cell that is not maximal. ~s" cell))
  (with-slots (cell-table facet-table cofacet-table) complex
    (labels ((%delete (cell complex)
	       (loop with cofacets
		     for facet in (facets cell complex) do
		       (setf cofacets (remove cell (cofacets facet complex)))
		       (cond
			 (cofacets (setf (gethash (id-number-of facet)
						  cofacet-table)
					 cofacets))
			 (t (remhash (id-number-of facet) cofacet-table)
			    (%delete facet complex))))
	       (remhash (cell-id cell) cell-table)
	       (remhash (id-number-of cell) facet-table)))
      (%delete cell complex))))

(defgeneric %map-over-cells (function complex dimension)
  (:documentation
   "The purpose of this function is unknown."))

;;  Use function on all cells of complex that have the given
;;  dimension.  If dimension is null then use function on all cells.
(defmethod %map-over-cells ((function function) (complex cell-complex)
			    dimension)
  (maphash #'(lambda (cell-id cell)
	       (declare (ignore cell-id))
	       (when (or (null dimension) (= dimension (dimension-of cell)))
		 (funcall function cell)))
	   (cell-table-of complex)))

;;  Syntactic sugar.
(defmacro map-over-cells ((cell &optional (dimension nil)) structure
			  &body body)
  `(%map-over-cells #'(lambda (,cell) ,@body) ,structure ,dimension))

;;  More syntactic sugar.
(eval-when (:compile-toplevel :load-toplevel)
  (defmacro map-over-maximal-cells ((cell) complex &body body)
    `(map-over-cells (,cell) ,complex
      (when (maximal-cell? ,cell ,complex)
        ,@body))))

(defmethod union ((complex1 cell-complex) (complex2 cell-complex) &rest rest)
  (when rest
    (error "Too many arguments to Union."))
  (unless (eql (class-of complex1) (class-of complex2))
    (error "~s and ~s are not of the same class." complex1 complex2))
  (let ((new (make-instance (class-of complex1))))
    (map-over-maximal-cells (cell) complex1 (insert cell new))
    (map-over-maximal-cells (cell) complex2 (insert cell new))
    new))

(defmethod intersection ((complex1 cell-complex) (complex2 cell-complex)
			 &rest rest)
  (when rest
    (error "Too many arguments to Intersection."))
  (unless (eql (class-of complex1) (class-of complex2))
    (error "~s and ~s are not of the same class." complex1 complex2))
  (let ((new (make-instance (class-of complex1))))
    (map-over-cells (cell) complex1
      (when (get-cell cell complex2) (insert cell new)))
    new))
		    
(defmethod vertex-set ((cell-complex cell-complex))
  (let ((vert-list nil))
    (map-over-cells (v-cell 0) cell-complex
		    (push (first (vertices-of v-cell)) vert-list))
    vert-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Simplicial-Complexes.

;;  Catch bad insertions.  This avoids a nasty precedence problem:
;;  without this, the command (insert cell simplicial-complex) can end
;;  up at command (insert cell cell-complex) and work even when cell
;;  is a nonsimplex.
(defmethod insert :before (thing (complex simplicial-complex) &rest ignore)
  (declare (ignore ignore))
  (unless (typep thing 'simplex)
    (error "Illegal attempt to Insert a nonsimplex ~s into ~s" thing complex)))

(defun make-simplicial-complex (cells)
  (loop with complex = (make-instance 'simplicial-complex)
	for cell in cells do
	(insert cell complex) 
	finally (return complex)))

(defgeneric max-dimension-of (cell)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod max-dimension-of ((cm cell-complex))
  (let ((max 0))
    (map-over-cells(cell) cm
		   (setf max (max max (dimension-of cell))))
    max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Coerce.
;;
;;  Coerce via the coercion-cache.  A single object can have multiple
;;  representations; this allows fast access without recomputing the
;;  map.  Maps are specified via Make-Homomorphism.  It's also
;;  possible to force a coercion to have a particular value via (setf
;;  coerce).
;;
;;  This has three advantages: (1) You can force a coercion to have a
;;  particular value via (setf coerce).  (2) You can save time (when
;;  the map between spaces is slow).  (3) You can save consing
;;  (nothing new is created after the first time that the cache is
;;  used; while a map is likely to create new objects during every
;;  use).
;;
;;  FIXTHIS: This method should probably be in a different file.
;;
(defmethod coerce ((thing has-coercion-cache) (domain domain))

  ;;  Return the thing itself if it is already in the correct domain.
  (or (if (eql (domain-of thing) domain) thing)

      ;;  Use the stored value if it exists.
      (rest (assoc domain (%coercion-cache-of thing)))

      ;;  Get the value, cache it, and return it.
      (let ((value (call-next-method)))
        (if value (push (cons domain value) (%coercion-cache-of thing)))
        value)))

(defmethod %set-coerce ((thing has-coercion-cache) (domain domain) value)
  (with-slots (coercion-cache) thing
    (when (assoc domain coercion-cache)
      (error "Multiple representations for a single item. ~s" thing))
    (unless (eql (domain-of value) domain)
      (error "Mismatch when defining coercion. ~s ~s" domain value))
    (push (cons domain value) coercion-cache)
    value))

(defsetf coerce %set-coerce)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chains
;;
;; CHAINs are elements of CHAIN-MODULE domains
;;
;;   Use GET-CHAIN-MODULE to create CHAIN-MODULE domains.
;;   CHAIN representations are based upon the canonical cells returned by
;;   GET-Canonical-Cell from the CELL-COMPLEX in the CHAIN-MODULE.

(defun chain-module-print-object (cm stream)
  (format stream "C_~D(~A;~A)" (dimension-of cm) (complex-of cm)
	  (coefficient-domain-of cm)))

(defun cochain-module-print-object (cm stream)
  (format stream "C^~D(~A;~A)" (dimension-of cm) (complex-of cm)
	  (coefficient-domain-of cm)))

(defmethod complex-of ((chain chain))
  (complex-of(domain-of chain)))

(defgeneric get-chain-module (cell integer &optional ring)
  (:documentation
   "The purpose of this function is unkown."))

(defmethod get-chain-module ((c cell-complex) (n integer)
			     &optional (ring (get-rational-integers)))
  (make-instance 'chain-module
		 :complex c
                 :dimension n
                 :coefficient-domain ring
		 :print-function 'chain-module-print-object))

(defgeneric get-cochain-module (cell integer &optional ring)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod get-cochain-module ((c cell-complex) (n integer)
			       &optional (ring (get-rational-integers)))
  (make-instance 'cochain-module
		 :complex c
                 :dimension n
                 :coefficient-domain ring
		 :print-function 'cochain-module-print-object
		 ))

(defgeneric boundary-domain (chain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary-domain ((c chain-module))
  (if (= (dimension-of c) 0)
     (error "Can not create a chain module with a less than zero dimension.")
     (get-chain-module (complex-of c)
                       (- (dimension-of c) 1)
                       (coefficient-domain-of c))))

(defmethod dimension-of ((chain chain))
  (dimension-of (domain-of chain)))

(defmethod boundary-domain ((c chain))
  (if (= (dimension-of (domain-of c)) 0)
      (error "Can not create boundary domain for a zero dimension chain.")
    (get-chain-module (complex-of c)
		      (- (dimension-of c) 1)
		      (coefficient-domain-of (domain-of c)))))

(defmethod boundary-domain ((s simplex))
  (get-chain-module (make-simplicial-complex (list s))
		    (- (dimension-of s) 1)
		    (get-rational-integers)))

(defmethod boundary-domain ((cc cell-complex))
  (get-chain-module cc (1- (max-dimension-of cc))
		    (get-rational-integers)))

(defgeneric coboundary-domain (cochain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod coboundary-domain ((c cochain))
  (if (= (dimension-of (domain-of c)) (max-dimension-of (complex-of c)))
      (error "Can not create coboundary domain for a maximal dimensional cochain.")
    (get-cochain-module (complex-of c)
			(+ (dimension-of c) 1)
			(coefficient-domain-of (domain-of c)))))

(defmethod coboundary-domain ((s simplex))
  (get-cochain-module (make-simplicial-complex (list s))
		      (+ (dimension-of s) 1)
		      (get-rational-integers)))


(defmethod print-object ((c chain) stream)
  (flet ((print-term (s coef)
		     (cond ((minus? coef)
		  (princ " - " stream)
		  (setq coef (- coef)))
		 (t (princ " + " stream)))
	   (unless (1? coef)
	     (print-grouped coef stream))
	   (princ s stream)))
    (let* ((terms (chain-terms-of c))
	   (s (first (first terms)))
	   (coef (rest (first terms))))
      (cond ((null terms) (princ 0 stream))
	    (t (cond ((minus? coef)
		      (princ " - " stream)
		      (setq coef (- coef))))
	       (unless (1? coef)
		 (print-grouped coef stream))
               (princ s stream)
	       (loop for (cell . coef) in (rest terms)
		     do (print-term cell coef)))))))

(defun canonical (pair domain)
  (multiple-value-bind (cached sign)
      (get-cell (complex-of domain) (first pair))
    (cons cached (if (> sign 0) (cdr pair) (minus (cdr pair))))))

(defmethod coerce ((simplex simplex) (cm chain-module))
  (make-chain cm (list(cons simplex
 			    (coerce 1 (coefficient-domain-of
 				       cm))))))

(defmethod coerce ((cc simplicial-complex) (cm chain-module))
  (let ((one (coerce 1 (coefficient-domain-of cm)))
 	(dim (dimension-of cm))
 	(list nil))
    (map-over-cells (face dim) cm
		    (push (cons face one) list))
    (make-chain cm list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CELL-ORDER
;;   defines a canonical ordering for cells
;;   - used to create canonical stored and printed representations for chains
(defun points-order (a b)
  (loop for av in a
	for bv in b
	do (cond ((cl:< (id-number-of av) (id-number-of bv))
		  (return t))
		 ((eql av bv) nil)
		 (t (return nil)))))

(defgeneric cell-order (cell1 cell2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod cell-order ((a cell) (b cell))
  (let ((aorder (sort (copy-list (vertices-of a)) #'cl:< :key #'id-number-of))
	(border (sort (copy-list (vertices-of b)) #'cl:< :key #'id-number-of)))
    (or (points-order aorder border)
	(points-order (vertices-of a) (vertices-of b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAKE-CHAIN
;;   Used to create CHAIN elements.  
;;   -  Currently does not verify that the simplexes given are of the proper
;;      dimension indicated in the CHAIN-MODULE


;; Internally, the chains are stored as dotted pairs of simplices and
;; coefficients.  The argument list to make-chain is also a list of
;; dotted pairs.

(defun make-chain-or-cochain (d simplices chain-or-cochain)
  (let ((simps nil))
    (loop for simp on 
	  (sort simplices
		#'(lambda (x y)
		    (cell-order (car y) (car x))))
	  do
	  (cond
	   ((eq (first simp) (second simp))
	    (setf (cdr (second simp))
		  (+ (cdr (second simp))
		     (cdr (first simp)))))
	   ((0? (cdr (first simp)))nil)
	   (t (push (first simp) simps))))
    (make-instance chain-or-cochain
		   :domain d
		   :terms simps)))

(defun make-chain (d simplices)
  (make-chain-or-cochain d simplices 'chain))

(defun make-cochain (d simplices)
  (make-chain-or-cochain d simplices 'cochain))

(defmethod zero ((d chain-module))
  (make-chain d ()))

(defmethod apply ((c chain) &rest args)
  (setq args (accum-apply-args args))
  (cond ((typep (first args) 'simplex)
	 (loop with s = (first args)
	       for (simplex . coef) in (chain-terms-of c)
	       do (when (face? s simplex)
		    (return coef))))))
  

(defun free-group-plus (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
		 (if simp1 (if simp2 (let ((c-sum (+ c1 c2)))
				       (if (not (0? c-sum))
					   (collect-term simp1 c-sum)))
			     (collect-term simp1 c1))
		   (collect-term simp2 c2))))

(defun free-group-difference (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
		 (if simp1 (if simp2 (let ((c-sum (- c1 c2)))
				       (if (not (0? c-sum))
					   (collect-term simp1 c-sum)))
			     (collect-term simp1 c1))
		   (collect-term simp2 c2))))

(defun free-group-minus (xt)
   (free-group-difference nil xt))

(defun free-group-scalar-times (c terms)
  (cond ((0? c) nil)
	(t (loop for (simp . coef) in terms
		 for c1 = (* c coef)	; coefficient ring need not be an 
		 unless (0? c1)		; integral domain!
		   collect (cons simp c1)))))

(defmethod-sd plus ((x chain) (y chain))
  (make-chain (domain-of x)
	      (free-group-plus (chain-terms-of x) (chain-terms-of y))))

(defmethod-sd difference ((x chain) (y chain))
  (make-chain (domain-of x)
	      (free-group-difference (chain-terms-of x) (chain-terms-of y))))

(defmethod times ((x number) (y chain))
  (make-chain  (domain-of y)
	       (free-group-scalar-times x (chain-terms-of y))))

(defmethod times ((x domain-element) (y chain))
  (make-chain  (domain-of y)
	       (free-group-scalar-times x (chain-terms-of y))))

(defmethod times ((x chain) (y number))
  (make-chain (domain-of x)
	      (free-group-scalar-times y (chain-terms-of x))))

(defmethod times ((x chain) (y number))
  (make-chain (domain-of x)
	      (free-group-scalar-times y (chain-terms-of x))))

(defmethod minus ((x chain))
  (make-chain (domain-of x)
	      (free-group-minus (chain-terms-of x))))

(defun chain-terms-times (xt yt)
  (pair-up-terms xt (simp1 c1) yt (simp2 c2) cell-order
    (when (and simp1 simp2)
      (let ((c-prod (* c1 c2)))
	   (if (not (0? c-prod))
	       (collect-term simp1 c-prod))))))

(defmethod-sd times ((x chain) (y chain))
  (make-chain domain
	      (chain-terms-times (chain-terms-of x) (chain-terms-of y))))

(defgeneric boundary (simplex &optional domain)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary ((s simplex) &optional (domain (boundary-domain s)))
  (let* ((list nil)
         (one (one (coefficient-domain-of domain)))
	 (simplicial-complex (complex-of domain))
         )
    (map-over-oriented-facets
     (f o s simplicial-complex)
     (push (cons f (if o  one (minus one))) list))
    (make-chain domain list)))

(defmethod boundary ((cc cell-complex) &optional (domain (boundary-domain cc)))
  (let ((bound (zero domain)))
    (map-over-cells
     (cell (1+(dimension-of domain))) cc
     (setf bound (+ bound (boundary cell domain))))
    bound))

(defmethod boundary ((c chain) &optional d)
  (let((pairs nil)
       (bd  (if d d (boundary-domain c)))
       (zero nil)
       (ht (make-hash-table))
       (complex (complex-of c)))
    
    (setf zero (zero(coefficient-domain-of bd)))
    (loop for term in (chain-terms-of c) do
	  (map-over-oriented-facets
	   (facet orientation (car term) complex)
	   (setf (gethash facet ht)
		 (if orientation
		     (+ (gethash facet ht zero) (cdr term))
		   (- (gethash facet ht zero) (cdr term))))))
    (maphash #'(lambda(x y) (push (cons x y) pairs)) ht)
    (make-chain bd pairs)))


;; Takes a boundary (which is chain and returns the list of simplices
;; that make up the boundary.
(defmethod boundary-set ((c chain))
  (loop for (simp . coef) in (chain-terms-of c)
	with simps = nil
	do (unless (even? coef)
	     (push simp simps))
	   finally (return simps)))

(defmethod deriv ((ch chain) &rest vars)
  (make-chain (domain-of ch)
	      (loop for (simp . coef) in (chain-terms-of ch)
		    for c1 = (apply #'deriv (cons coef vars))
		    unless (0? c1)
		      collect (cons simp c1))))



;;;COBOUNDARY
(defmethod coboundary ((c cochain) &optional d)
  (let((pairs nil)
       (bd  (if d d (coboundary-domain c)))
       (zero nil)
       (ht (make-hash-table))
       (complex (complex-of c)))
    
    (setf zero (zero(coefficient-domain-of bd)))
	  ;;; use chain-terms-of to take advantage of structural
	  ;;; isomorphism between finite chains and cochains
    (loop for (simp . coef)  in (chain-terms-of c) do
	  (map-over-oriented-cofacets
	   (cofacet orientation simp complex)
	   (setf (gethash cofacet ht)
		 (if orientation
		     (+ (gethash cofacet ht zero) coef)
		   (- (gethash cofacet ht zero) coef)))))
    (maphash #'(lambda(x y) (push (cons x y) pairs)) ht)
    (make-cochain bd pairs)))


;;;Eventually the duality relationship between chains and cochains
;;;should be taken into account.  For now -- these coersions

(defmethod coerce ((chain chain)  (cocm cochain-module))
  (make-cochain cocm (copy-tree (chain-terms-of chain))))

(defmethod coerce ((cochain cochain)  (cm chain-module))
  (make-chain cm (copy-tree (chain-terms-of cochain))))
;;; -*- Mode:Lisp; Package:WEYLI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			   Function Spaces
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

;;; funct-spaces.lisp,v 1.7 1994/10/21 18:16:36 rz Exp

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.7")

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator function-space ((domain domain) (range field))
    (make-instance 'function-space :domain domain :range range
                   :coefficient-domain range)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'function-space)
                        (eql (funct-domain-of d) domain)
                        (eql (funct-range-of d) range)))))

;; The points of a function space are the functions themselves.  (i.e.
;; function-space-element's)

(def-binary-coercion inner-product
  "No way to compute the inner-product of ~S and ~S"
  "Ambiguous coercion for inner-product (~S, ~S)")

;; FIXTHIS:: I'm leaving this here for now, but being a Hilbert space
;; should bea property, not a class.  That way, function spaces AND
;; vector space could both be Hilbert Spaces.

;; Default norm for a Hilbert space
(defmethod norm ((x hilbert-space-element))
  (inner-product x x))

(eval-when (:compile-toplevel :load-toplevel)
  (define-domain-creator hilbert-space ((domain dimensional-space)
                                        (range dimensional-space))
    (make-instance 'hilbert-space :domain domain :range range)
    :predicate #'(lambda (d)
                   (and (eql (class-name (class-of d)) 'free-module)
                        (eql (funct-domain-of d) domain)
                        (eql (funct-range-of d) range)))))


;; The following routines maintain a cache of abscissa vectors.

(defvar *maximum-number-of-cached-vectors* 12)
(defvar *cached-vectors* ())

(defun check-cached-vector (vector)
  (let ((len (length vector)))
    (flet ((equal-vector (vect)
	     (when (and vect (eql (length vect) len))
	       (loop for i below len
		     do (unless (= (aref vector i) (aref vect i))
			  (return nil))
		     finally (return t)))))
      (cond ((null *cached-vectors*)
	     (setq *cached-vectors*
		   (make-array *maximum-number-of-cached-vectors*))
	     (setf (svref *cached-vectors* 0) vector)
	     vector)
	    (t (loop for i below *maximum-number-of-cached-vectors*
		     for vect = (svref *cached-vectors* i)
		     do (cond ((null vect)
			       (setf (svref *cached-vectors* i) vector)
			       (return vector))
			      ((equal-vector vect)
			       (unless (zerop i)
				 (rotatef (svref *cached-vectors* (1- i))
					  (svref *cached-vectors* i)))
			       (return vect)))
		     finally
		  (setf (svref *cached-vectors*
			       (1- *maximum-number-of-cached-vectors*))
			vector)
		  (return vector)))))))

(defclass sampled-function (function-space-element)
  ((x :initarg :x
      :reader function-x)
   (y :initarg :y
      :reader function-y)))

(defmethod print-object ((obj sampled-function) stream)
  (let* ((x-coords (function-x obj))
	 (npts (array-dimension x-coords 0)))
    (format stream "#<SFun: [~S, ~S] (~D pts)>"
	    (aref x-coords 0) (aref x-coords (- npts 1))
	    npts)))

(defgeneric make-sampled-function (domain x y)
  (:documentation
   "The purpose of this function is unknown."))

;; Include domain here since eventually we'll need it.  For now we
;; use the global *FUNCTION-SPACE-RING*
(defmethod make-sampled-function ((domain function-space) x y)
  (make-instance 'sampled-function :domain domain
		 :x (check-cached-vector x) :y y))

(defmethod print-table
    ((func sampled-function) &optional (stream *standard-output*))
  (let ((x-array (function-x func))
	(y-array (function-y func)))
    (loop for i below (length x-array)
	  do (format stream "~D: ~D~%" (aref x-array i) (aref y-array i)))))

(defgeneric make-sampled-function-1 (domain x-list function)
  (:documentation
   "The purpose of this function is unknown."))

;; Creates a sampled function, by sampling the argument function at
;; the points given in x-list.
(defmethod make-sampled-function-1 ((domain function-space) x-list function)
  (let* ((num-pts (length x-list))
	 (y-array (make-array (list num-pts)))
	 (x-array
	  (if (listp x-list)
	      (make-array (list num-pts)
			  :initial-contents (mapcar #'float x-list))
	      x-list)))
    (loop for i below num-pts
	  do (setf (aref y-array i) (funcall function (aref x-array i))))
    (make-sampled-function domain x-array y-array)))

(defmethod make-sampled-function-1 ((domain (eql nil)) x-list function)
  (let* ((num-pts (length x-list))
	 (y-array (make-array (list num-pts)))
	 (x-array
	  (if (listp x-list)
	      (make-array (list num-pts)
			  :initial-contents (mapcar #'float x-list))
	      x-list))
	 domain range)
    (loop for i below num-pts
	  do (setf (aref y-array i) (funcall function (aref x-array i))))
    (setq domain (if (typep (aref x-array 0) 'domain-element)
		     (domain-of (aref x-array 0))
		   (domain-of (canonicalize-number (aref x-array 0)))))
    (setq range (if (typep (aref y-array 0) 'domain-element)
		    (domain-of (aref y-array 0))
		  (domain-of (canonicalize-number (aref y-array 0)))))
    (make-sampled-function (get-function-space domain range)
			   x-array y-array)))

;; Takes a sampled-function and re-samples it at n evenly spaced
;; points.

(defmethod resample ((func sampled-function) n)
  (let* ((x (make-array n))
	 (f-x (function-x func))
	 (x0 (aref f-x 0))
	 (step (/ (- (aref f-x (1- (length f-x))) x0) (1- n))))

    ;; Divide the domain of func into n-1 evenly spaced intervals.
    (loop for i below n
	  do (setf (aref x i) x0
		   x0 (+ x0 step)))
    (smooth2 func x)))

(defgeneric smooth2 (function x)
  (:documentation
   "The purpose of this function is unknown."))

;; Given sampled function, resample it at each point of new-x,
;; interpolating from func.
(defmethod smooth2 ((func sampled-function) new-x)
  (let ((new-y (make-array (length new-x)))
	(x (function-x func))
	(y (function-y func)))	  
    (loop for i below (length new-x)
	  for xval = (aref new-x i)
	  do (setf (aref new-y i) (polynomial-interpolate x y xval 4)))
    (make-sampled-function (domain-of func) new-x new-y)))
	  
(defmethod evaluate-at ((func sampled-function) pt)
  (polynomial-interpolate (function-x func) (function-y func) pt 4))

;; Intepolate the function that passes through x-vector/y-vector at
;; point x using a polynomial interpolate of degree n.
(defun polynomial-interpolate (x-vector y-vector x &optional (n 4))
  (let* ((dif (abs (- x (aref x-vector 0))))
	 (ns 0)
	 (vector-length (array-dimension x-vector 0))
	 p offset)
    ;; If the number of points is smaller than the desired order,
    ;; reduce the order.
    (setq n (min n vector-length))
    (setq p (make-array (list n)))
    (loop for i below vector-length
	  for dift = (abs (- x (aref x-vector i)))
	  do (when (< dift dif)
	       (setq ns i)
	       (setq dif dift)))  
    (setq offset (max 0 (floor (+ ns (- (/ n 2))
				  (if (minusp (- x (aref x-vector ns)))
				      0 1)))))
    (when (> (+ offset n) (1- vector-length))
      (setq offset (max 0 (- vector-length n)))) 
    (loop for i below n do
      (setf (aref p i) (aref y-vector (+ i offset))))
    (loop for m upfrom 1 below n do 
      (loop for i below (- n m) do
	(setf (aref p i)
	      (/ (+ (* (- x (aref x-vector (+ offset i m))) (aref p i))
		    (* (- (aref x-vector (+ offset i)) x) (aref p (1+ i))))
		 (- (aref x-vector (+ offset i))
		    (aref x-vector (+ offset i m)))))))
    (aref p 0)))
	    
(defmethod-sd plus ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports PLUS, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (sum (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref sum i) (cl:+ (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array sum))))

(defmethod-sd difference ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports DIFFERENCE, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (sum (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref sum i) (cl:- (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array sum))))

(defmethod-sd times ((func1 sampled-function) (func2 sampled-function))
  (let ((x-array (function-x func1)))
    (unless (eql x-array (function-x func2))
      (error "Different supports PLUS, ~A, ~A" func1 func2))
  (let* ((num-pts (length x-array))
	 (y1 (function-y func1))
	 (y2 (function-y func2))
	 (prod (make-array (list num-pts))))
      (dotimes (i num-pts)
	(setf (aref prod i) (cl:* (aref y1 i) (aref y2 i))))
      (make-sampled-function domain x-array prod))))

(defmethod map
    ((result-type (eql 'sampled-function)) oper (func sampled-function)
     &rest ignore)
  (declare (ignore ignore))
  (let* ((y (function-y func))
	 (new-y (make-array (list (length y)))))
    (dotimes (i (length y))
      (setf (aref new-y i) (funcall oper (aref y i))))
    (make-sampled-function (domain-of func) (function-x func) new-y)))

(defmethod minus ((func sampled-function))
  (map 'sampled-function #'cl:- func))

(defmethod times ((num number) (func sampled-function))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((num numeric) (func sampled-function))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((func sampled-function) (num number))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod times ((func sampled-function) (num numeric))
  (map 'sampled-function #'(lambda (x) (* x num)) func))

(defmethod quotient ((func sampled-function) (num number))
  (map 'sampled-function #'(lambda (x) (/ x num)) func))

(defmethod quotient ((func sampled-function) (num numeric))
  (map 'sampled-function #'(lambda (x) (/ x num)) func))

(defmethod conjugate ((func sampled-function))
  (map 'sampled-function #'cl:conjugate func))

(defmethod realpart ((func sampled-function))
  (map 'sampled-function #'cl:realpart func))

(defmethod imagpart ((func sampled-function))
  (map 'sampled-function #'cl:imagpart func))

(defmacro deriv-2point (x y index1 index2)
  `(/ (- (aref ,y ,index2) (aref ,y ,index1))
      (- (aref ,x ,index2) (aref ,x ,index1))))

(defmethod deriv ((func sampled-function) &rest vars)
  (declare (ignore vars))
  (let* ((x-array (function-x func))
	 (y-array (function-y func))
	 (num-pts (length x-array))
	 (y-prime (make-array (list num-pts))))
    (setf (aref y-prime 0) (deriv-2point x-array y-array 1 0))
    (do ((i 1 (+ i 1)))
	((= i (1- num-pts))
	 (setf (aref y-prime i) (deriv-2point x-array y-array (- i 1) i)))
      (setf (aref y-prime i)
	    (/ (+ (deriv-2point x-array y-array (- i 1) i)
		  (deriv-2point x-array y-array  i (+ i 1))) 2)))
    (make-sampled-function (domain-of func) x-array y-prime)))

(defmacro trapezoidal (x y i1 i2)
  `(/ (* (- (aref ,x ,i2) (aref ,x ,i1))
	  (+ (aref ,y ,i1) (aref ,y ,i2))) 2))

(defgeneric integral (function &key lower upper &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod integral ((func sampled-function)
		     &key lower upper &allow-other-keys)
  (cond ((and (null lower) (null upper))
	 (indef-integral func))
	(t (let* ((x (function-x func))
		  (y (function-y func))
		  (num-pts (length x))
		  (sum 0.0))
	     ;; FIXTHIS: The trapezoidal rule is used here because it
	     ;; seems a bit more accurate.  But its really stupid.
	     ;; Since we know how we are interpolating the interval
	     ;; between any pair of points we should use an exact
	     ;; integration scheme.
	     (do ((i1 0 i2)
		  (i2 1 (cl:1+ i2)))
		 ((or (cl:>= i2 num-pts) (cl:<= upper (aref x i1)))
		  sum)
		 (setq sum (+ sum (trapezoidal x y i1 i2))))
	     #+ignore 
	     (do ((i1 0 (+ 2 i1))
		  (i2 1 (+ 2 i2))
		  (i3 2 (+ 2 i3)))
		 ((or (cl:>= i3 num-pts) (cl:<= upper (aref x i1)))
		  sum)
	       (multiple-value-bind (w1 w2) (simpson x y i1 i2 i3)
		 (setq sum (+ sum w1 w2))))))))

;; FIXTHIS:  I don't trust this routine. 
(defun simpson (x y i1 i2 i3)
  (let* ((x1 (svref x i1))
	 (x2 (svref x i2))
	 (x3 (svref x i3))
	 (h (- x2 x1))
	 (k (- x3 x2))
	 (range (- x2 x1))
	 (denom (* h k range))
	 (ksq (* k k))
	 (hsq (* h h))

	 (y1 (svref y i1))
	 (y2 (svref y i2))
	 (y3 (svref y i3))

	 (a (/ (- (+ (* k y1) (* h y3)) (* range y2)) (* 3 denom)))
	 (b (/ (+ (* hsq (- y3 y2)) (* ksq (- y2 y1))) (* 2 denom)))
	 (c y2))
    (values (+ (- (* a hsq h) (* b hsq)) (* c h))
	    (+ (* a ksq k) (* b ksq) (* c k)))))

(defun indef-integral (func)
  (let* ((x (function-x func))
	 (y (function-y func))
	 (num-pts (length x))
	 (int-y (make-array (list num-pts)))
	 (sum 0.0))
    (setf (aref int-y 0) 0.0)
    (do ((i1 0 (+ 2 i1))
	 (i2 1 (+ 2 i2))
	 (i3 2 (+ 2 i3)))
	((cl:>= i3 num-pts)
	 (if (= i2 (1- num-pts))
	     (setf (aref int-y i2) (+ sum (trapezoidal x y i1 i2))))
	 (make-sampled-function (domain-of func) x int-y))
      (multiple-value-bind (w1 w2) (simpson x y i1 i2 i3)
	(setq sum (+ sum w1))
	(setf (aref int-y i2) sum)
	(setq sum (+ sum w2))
	(setf (aref int-y i3) sum)))))

(defmethod-sd inner-product ((x sampled-function) (y sampled-function))
  (let* ((fun (* x (conjugate y)))
	 (abscissa (function-x x))
	 (dim (array-dimension abscissa 0)))
    (integral fun :lower (aref abscissa 0)
	      :upper (aref abscissa (1- dim)))))
	    

#| Test programs |

(defun sine-waves (n-pts n)
  (let ((abscissa (loop for x below 1 by (cl:/ n-pts)
			collect x)))
    (loop for i below n
	  collect (make-sampled-function-1
		       abscissa
		       #'(lambda (x) (sin (* 2 pi (1+ i) x)))))))

;; The following should generate Legendre polynomials!
(defun polys (n-pts n)
  (let ((abscissa (loop for x upfrom -1 below 1 by (cl:/ 2.0 n-pts)
			collect x)))
    (loop for i below n
	  collect (make-sampled-function-1
		       abscissa
		       #'(lambda (x) (cl:expt x i))))))

;; This uses randomly selected points.
(defun rpolys (n-pts n)
  (let ((abscissa (sort
		   (append (list 1.0 -1.0)
			   (loop for i below (- n-pts 2)
				 collect (/ (float (- (cl:random 20000) 10000))
					    10000)))
		   #'cl:<)))
    (loop for i below n
	  collect (make-sampled-function-1 abscissa
					   #'(lambda (x) (cl:expt x i))))))

;; Each argument is supposed to be a vector.  The vectors are are
;; normalized and an orthorgonal basis is returned.
(defun gram-schmidt (&rest vects)
  (let* ((dim (length vects))
	 (space (apply #'vector vects)))
    (loop for k below dim
	  for vect = (aref space k)
	  do ;; Normalize vector
	     (setq vect (/ vect (sqrt (abs (inner-product vect vect)))))
	     (setf (aref space k) vect)
	     (loop for j upfrom (+ k 1) below dim
		   for vect2 = (aref space j)
		   do (setf (aref space j)
			    (- vect2 (* (inner-product vect vect2) vect)))))
    space))

(defmethod print-Mathematica
    ((func sampled-function) &key (smooth 4) (stream *standard-output*)) 
  (let* ((x-array (function-x func))
	 (y-array (function-y func)))

    (when (and (not (null smooth))
	       (not (cl:numberp smooth)))
      (setq smooth 4))

    (format stream "Graphics[{GrayLevel[0.0]")
    (loop for i upfrom 1 below (length x-array)
	  for x0 = (aref x-array 0) then x1
	  and y0 = (aref y-array 0) then y1
	  and x1 = (aref x-array i)
	  and y1 = (aref y-array i)
	  do  (if (null smooth)
		 (format stream ",~%Line[{{~10F, ~10F}, {~10F, ~10F}}]"
			 x0 y0 x1 y1)
	       (loop for j below smooth
		     with delta = (cl:/ (cl:- x1 x0) smooth)
		     and xp = x0 and yp = y0 and xn and yn
		     do (setq xn (cl:+ xp delta)
			      yn (evaluate-at func xn))
		     (format stream ",~%Line[{{~10F, ~10F}, {~10F, ~10F}}]"
			     xp yp xn yn)
		     (setq xp xn yp yn))))
    (format stream "}]")))

||#
;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			         Flat Meshing
;;; ===========================================================================
;;; (c) Copyright 1995 Cornell University

;;; mesh.lisp,v 1.11 1995/06/09 14:21:32 chew Exp

;;  Everything needed for flat meshing.

(in-package :weyli)

;;; DELETE (make::adjust-version-numbers Weyl "1.11")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Control Variables.

(defvar *delaunay* T)			; Each new triangle is checked.
(defvar *cross-edges* nil)              ; Can't cross constraint edges.

(defvar *mesh* nil)			; Holds the current mesh.
(defvar *space* nil)			; Holds the current mesh space.
(defvar *too-close-factor* 0.75)        ; Affects what gets deleted when edges
					; are split.  1.0 deletes too many.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Classes.

(defclass has-home-space ()
     ((home :initarg :home :reader home-of)))

;;  The home-space acts as the parameter space.
(defclass curved-simplex (has-home-space simplex) ())

(defgeneric home-of (simplex)
  (:documentation
   "The home of a noncurved simplex is determined by (the first of)
its vertices.")
  (:method ((simplex simplex)) (domain-of (first (vertices-of simplex)))))

(defclass triangulation (simplicial-complex)
     (;;  Most recent triangle inserted; used for beginning searches.
      (most-recent :initform nil :accessor %most-recent)))

(defclass c-triangulation (triangulation)
     (;;  Holds the constraint edges. (The main tool to query this
      ;;  structure is Constraint.)
      (constraints :initform (make-instance 'simplicial-complex)
		   :reader %constraints-of)))

(defclass CDT (c-triangulation) ())

(defclass named-simplicial-complex (simplicial-complex)
  (;;  Holds names.
   (name-table :initform (make-hash-table) :reader %name-table-of)
   (default-name :initform nil :accessor %default-name-of)))

(defclass mesh (cdt named-simplicial-complex has-home-space)
     (;;  Triangles waiting to be improved during Refine-Mesh.
      (pending-list :initform nil :accessor %pending-list-of)))

(defmethod initialize-instance :after ((mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (with-slots (constraints) mesh
	      (setf constraints (make-instance 'named-simplicial-complex))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Maintenance of Names.
(defgeneric name (simplex mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod name ((simplex simplex) (nsc named-simplicial-complex))
  (gethash (id-number-of simplex) (%name-table-of nsc)))

(defmethod name ((simplex simplex) (mesh mesh))
  (case (dimension-of simplex)
    ((0 1) (name simplex (%constraints-of mesh)))
    (2 (call-next-method))
    (otherwise (error "Illegal use of NAME. ~s" simplex))))    

(defgeneric %set-name (simplex mesh name)
  (:documentation
   "The purpose of this function is unkown."))

(defmethod %set-name ((simplex simplex) (nsc named-simplicial-complex) name)
  (if name
      (setf (gethash (id-number-of simplex) (%name-table-of nsc)) name)
      (remhash (id-number-of simplex) (%name-table-of nsc)))
  name)

(defmethod %set-name ((simplex simplex) (mesh mesh) name)
  (case (dimension-of simplex)
    ((0 1) (%set-name simplex (%constraints-of mesh) name))
    (2 (call-next-method))
    (otherwise (error "Illegal use of (SETF NAME). ~s" simplex))))

(defsetf name %set-name)

(defgeneric insert (simplex nsc &key name &allow-other-keys)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insert ((simplex simplex) (nsc named-simplicial-complex)
		   &key (name (%default-name-of nsc))
		   &allow-other-keys)
  (setf (name simplex nsc) name)
  (call-next-method))

(defgeneric delete-maximal-cell (simplex nsc)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod delete-maximal-cell ((simplex simplex)
				(nsc named-simplicial-complex))
  (setf (name simplex nsc) nil)
  (call-next-method))

(defgeneric all-names (nsc)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod all-names ((nsc named-simplicial-complex))
  (let ((names nil))
    (maphash #'(lambda (ignore name) (declare (ignore ignore))
		       (pushnew name names))
	     (%name-table-of nsc))
    names))

(defmethod all-names ((sc simplicial-complex))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Utilities.

;;  Rotate the given list to bring the specified position to the front
;;  (items are numbered 0, 1, 2,...).  If no position is specified
;;  then one item is rotated (the second item is then on the front).
(defun rotate-list (list &optional (position 1))
  (append (nthcdr position list) 
	  (butlast list (- (length list) position))))

;;  Rotate a list to bring the leftmost occurrence of the specified
;;  member to the front.  The test is eql unless altered using the
;;  keyword :test.  Results are undefined if the given item is not a
;;  member of the given list.
(defun member-rotate (item list &key (test #'eql) (key #'identity))
  (rotate-list list (position item list :test test :key key)))

;;  Convert a vector into a lisp complex number.
(defgeneric complexer (vector)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod complexer ((vector vector-space-element))
  (unless (= 2 (dimension-of (domain-of vector)))
    (error "Wrong length vector for conversion to complex number. ~s" vector))
  (cl:complex (convert-to-lisp-number (ref vector 0))
	      (convert-to-lisp-number (ref vector 1))))

(defgeneric coordinate-list (vector)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod coordinate-list ((vector vector-space-element))
  (loop for n below (dimension-of (domain-of vector))
	collect (convert-to-lisp-number (ref vector n))))

(defun sqr (item)
  (* item item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Queue (a list implementation of a standard queue).

;;  FIXTHIS: All this queue stuff should be somewhere else.  There are
;;  also implementations (with consistent interface) for stack,
;;  priority-queue, and random-queue.

(defclass queue ()
     ((front :initarg :front :accessor front :reader %contents)
      ;;  The last item in the queue.
      (back :initarg :back :accessor back)))

(defun make-queue (&key (initial-contents nil))
  (setf initial-contents (copy-list initial-contents))
  (make-instance 'queue :front initial-contents
		 :back (last initial-contents)))

(defgeneric clearq (queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod clearq ((queue queue))
  (setf (front queue) nil)
  (setf (back queue) nil))

(defgeneric insertq (item queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insertq (item (queue queue))
  (with-slots (front back) queue
     (cond (front			; Nonempty queue.
	    (setf (rest back) (list item))
	    (setf back (rest back)))
	   (t				; Empty queue.
	    (setf front (list item))	
	    (setf back front)))))

;;  If :delete is null then the item is not removed from the queue.
(defgeneric getq (queue &key delete)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod getq ((queue queue) &key (delete t))
  (with-slots (front) queue
	      (let ((item (first front)))
		(if delete (setf front (rest front)))
		item)))

(defgeneric emptyq? (queue)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod emptyq? ((queue queue))
  (not (front queue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Geometry.

;;  Given 3 complex numbers forming a triangle, return the complex
;;  number that is the circumcenter.  Works with lisp complex numbers,
;;  since the weyl versions have some problems.
(defun complex-circumcenter (a b c)
  ;;  Map a and b to 0 and 1.
  (let* ((bb (cl:- b a))
	 (cc (cl:/ (cl:- c a) bb))
	 (cx (cl:realpart cc))
	 (cy (cl:imagpart cc))
	 w)    
    (when (cl:= 0 cy) (error "Flat triangle. ~s ~s ~s" a b c))
    (setf w (cl:complex 0.5 (cl:/ (cl:+ (cl:* cx cx) (cl:* cy cy) (cl:- cx))
				  (cl:* 2.0 cy))))
    ;;  Map back.
    (cl:+ (cl:* w bb) a)))

;;  Determine the center of a circle described by two points on the
;;  circle and a radius.  The two points and the returned center are
;;  all complex numbers.  There can be zero, one, or two solutions to
;;  this problem.  If there is no solution then a warning is printed
;;  and the midpoint of the two vertices is returned.  A radius of 0
;;  is taken as a special case and no warning is issued.  If there are
;;  two solutions then normally the solution to the left of the line
;;  from a to b is returned.  The other solution is returned if the
;;  given radius is negative.  Uses Lisp complex numbers.
(defun circle-center (a b radius)
  ;;  Based on using complex arithmetic to map 0,2 to a,b.  c1 and c2
  ;;  represent the center in various coordinate frames.
  (let* ((bb (cl:* 0.5 (cl:- b a)))
	 (rad (cl:/ radius (cl:abs bb)))
	 (cy (cl:sqrt (cl:- 1 (cl:* rad rad))))
	 (c1 (cl:+ 1 cy))
	 ;;  Check for bad radius and alternate solution.
	 (c2 (cond 
	       ((cl:/= 0 (cl:realpart cy))
		(unless (cl:= 0 radius)
		  (warn "Radius too small; half circle assumed. ~s ~s" a b))
		1.0)
	       ((cl:plusp radius) c1)
	       (t (cl:conjugate c1)))))
    ;;  Switch back to original reference frame.
    (cl:+ a (cl:* bb c2))))

(defgeneric make-mean-point (points &key mean-space point-space)
  (:documentation
   "The purpose of this function is unknown."))

;;  Make a point that is the mean of the given points.  The mean-space
;;  is the domain in which the mean is calculated; the point-space is
;;  the domain in which the resulting point resides.  The two spaces
;;  are presumably related via coerce.
(defmethod make-mean-point ((points list) &key
			    (mean-space (domain-of (first points)))
			    (point-space (domain-of (first points))))
  (let* ((vectors (mapcar #'(lambda (p) (coerce p mean-space)) points))
	 (mean (/ (apply #'%plus vectors) (float (length points))))
	 (point (make-point point-space (coerce mean point-space))))
    (unless (eql point-space mean-space)
      (setf (coerce point mean-space) mean))
    point))

(defconstant %deg-over-rad (cl:/ 180.0 cl:pi))

;;  Compute the angle between two vectors.
(defgeneric angle (vertex triangle &rest args &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod angle ((a vector-space-element) (b vector-space-element)
		  &key (radians nil) (degrees (not radians)) &allow-other-keys)
  ;;  The realpart is needed because we sometimes get a small
  ;;  imaginary component due to numerical error.
  (let ((angle (cl:realpart (cl:acos (convert-to-lisp-number
				      (/ (dot-product a b)
					 (sqrt (* (dot-product a a)
						  (dot-product b b)))))))))
    (if degrees (cl:* %deg-over-rad angle)
	angle)))

;;  Create a parameter-space.  This is used mainly to create simplices
;;  that are curved in the target-space.  The map should take a
;;  vector-space-element and produce an element of the target-space.
;;
;;  If an inverse-map is given then that is used build the
;;  correspondence between the parameter-space and the target-space
;;  (but inverse-maps are usually unavailable).  The inverse-map
;;  should take an element of the target-space and produce a lisp
;;  vector.  The dimension of the parameter-space must be specified
;;  when an inverse-map is used.
;;
;;  If no inverse-map is available then the spaces are tied together
;;  via the correspondence between the parameter-vectors and the
;;  target-points (i.e., the result will be that the target-points can
;;  be coerced into the parameter-space where they have the
;;  coordinates given by the parameter vectors).  The
;;  parameter-vectors must be lisp vectors.  [At some point, we may
;;  want to check that the map actually takes the parameter-vectors
;;  into the target-points.]
;;
(defgeneric make-parameter-space (map-function target-space &key parameter-vectors
                                               target-points dimension inverse-map)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-parameter-space ((map function) (target-space vector-space)
				 &key parameter-vectors target-points
				 dimension inverse-map)
  (unless dimension
    (if parameter-vectors (setf dimension (length (first parameter-vectors)))
	(error "Cannot determine dimension in Make-Parameter-Space.")))
  (let ((parameter-space (make-euclidean-space dimension))) ; A new space.
    (make-homomorphism parameter-space map target-space)

    ;;  Establish the inverse correspondence between the spaces.
    (if inverse-map
	(make-homomorphism target-space
			   #'(lambda (e)
			       (make-element parameter-space
					     (funcall inverse-map e)))
			   parameter-space)
	(mapcar #'(lambda (vector point)
		    (setf (coerce point parameter-space)
			  (make-element parameter-space vector)))
		parameter-vectors target-points))
    parameter-space))

;;  Split the given simplex along the given face using the given
;;  splitting-point.  The default splitting-point is the mean-point of
;;  the face.  Results are undefined if the given face does not
;;  correspond to part of the simplex.  Note that the calculations for
;;  the splitting-point and the creation of new simplices takes place
;;  in the simplex's home coordinate system.
(defgeneric split (simplex where &rest args)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod split ((simplex simplex) (where (eql nil)) &key
		  (face (vertices-of simplex))
		  (splitting-point
		   (make-mean-point face :mean-space (home-of simplex))))
  (loop with points = (vertices-of simplex)
	with home = (home-of simplex)
	for v in face
	for new-set = (subst splitting-point v points)
	collect (make-instance (class-of simplex)
		     :vertices new-set :home home)))

;;  Split a simplex within a simplicial-complex.
(defmethod split ((simplex simplex) (where simplicial-complex) &rest args)
  (loop with simplices = (apply #'split simplex nil args)
	initially (delete-maximal-cell simplex where)
	for s in simplices do
	  (insert s where)
	finally (return simplices)))

;;  Splitting a list splits each thing in the list.
(defmethod split ((things list) where &rest ignore)
  (declare (ignore ignore))
  (loop for thing in things
	append (split thing where)))

;;  Size of the given simplex.  We use the length of the longest side.
(defgeneric simplex-size (simplex &optional space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod simplex-size ((simplex simplex) &optional (space (home-of simplex)))
  (loop with points = (vertices-of simplex)
        repeat (1- (length points))
        maximize (loop with p = (first points)
                       for other in (rest points)
                       maximize (convert-to-lisp-number
                                 (distance p other :space space)))
        do (setf points (rest points))))

;;  Returns :left, :right, or :on depending on position of third
;;  vertex in relation to ray from first to second vertex.  Some
;;  effort has been taken to ensure that this operation is safe in the
;;  sense that the answer is the same even when the three vertices are
;;  given in a different order.  Without this caution, you can run
;;  into problems (due to numerical error); for instance, a vertex
;;  that looks as if it's on the line between two triangles can be
;;  claimed to be :outside of each of them.
(defgeneric bend (space &rest points)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod bend ((space vector-space) &rest three-points)
  (unless (= 3 (length three-points))
    (error "Improper number of arguments to Bend; need space then 3 points."))
  (let* ((ordered (sort (copy-list three-points) #'cl:< :key #'id-number-of))
	 (a (coerce (first ordered) space))
	 (b (coerce (second ordered) space))
	 (c (coerce (third ordered) space))
	 (det (* (sign-of-permutation ordered three-points)
		 (+ (* (- (ref b 1) (ref a 1)) (- (ref c 0) (ref b 0)))
		    (* (- (ref a 0) (ref b 0)) (- (ref c 1) (ref b 1)))))))
    (cond ((> 0 det) :left)
	  ((< 0 det) :right)
	  (t :on))))

(defgeneric distance (vector1 vector2 &rest ignore)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod distance ((vectora vector-space-element)
		     (vectorb vector-space-element) &rest ignore)
  (declare (ignore ignore))
  (let ((diff (- vectorb vectora)))
    (sqrt (dot-product diff diff))))

(defmethod distance ((pointa point) (pointb point) &key (space nil))
  (unless space
    (error "Must specify space for Distance between points."))
  (call-next-method (coerce pointa space) (coerce pointb space)))

(defmethod distance ((lista list) (listb list) &rest ignore)
  (declare (ignore ignore))
  (unless (= (length lista) (length listb))
    (error "Cannot comput distance between lists of different lengths. ~s ~s"
	   lista listb))
  (loop for a in lista
        for b in listb
	sum (sqr (- b a)) into sum-of-squares
	finally (return (sqrt sum-of-squares))))	

;;  Returns T iff the given edges cross each other.  Returns true even
;;  when one vertex of one edge is :on the other edge.
(defgeneric edges-cross? (space edge1 edge2)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod edges-cross? ((space vector-space) (edge-a list) (edge-b list))
  (and (not (eql (bend space (first edge-a) (second edge-a) (first edge-b))
		 (bend space (first edge-a) (second edge-a) (second edge-b))))
       (not (eql (bend space (first edge-b) (second edge-b) (first edge-a))
		 (bend space (first edge-b) (second edge-b) (second edge-a))))
       ))

;;  A bounding-box is a pair (low high) of coordinate lists.  For
;;  instance, 2D points would produce: ((low-x low-y) (high-x
;;  high-y)).  Useful mostly for graphics.  Also used for mesh
;;  initialization.
(defgeneric bounding-box (point space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod bounding-box ((point point) (space vector-space))
  (let ((c (coordinate-list (coerce point space))))
    (list c c)))

(defmethod bounding-box ((list list) (space vector-space))
  (if (= 1 (length list))
      (bounding-box (first list) space)
      (let ((a (bounding-box (first list) space))
	    (b (bounding-box (rest list) space)))
	(list (mapcar #'cl:min (first a) (first b))
	      (mapcar #'cl:max (second a) (second b))))))

(defmethod bounding-box ((simplex simplex) (space vector-space))
  (bounding-box (vertices-of simplex) space))

(defmethod bounding-box ((sc simplicial-complex) (space vector-space))
  (let (old new)
    (map-over-cells (cell) sc
      (setf new (bounding-box cell space))
      (unless old (setf old new))
      (setf old (list (mapcar #'cl:min (first new) (first old))
		      (mapcar #'cl:max (second new) (second old)))))
    old))

;;  Report the measure of the given simplex in the given space.
;;  FIXTHIS: This should be a general measure function based on
;;  determinants so that it works in any dimension.
(defgeneric measure (simplex space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod measure ((simplex simplex) (space vector-space))
  (let ((vertices (vertices-of simplex)))
    (case (dimension-of simplex)
      (0 0)
      (1 (distance (first vertices) (second vertices) :space space))
      (2 (let* ((a (complexer (coerce (first vertices) space)))
		(b (complexer (coerce (second vertices) space)))
		(c (complexer (coerce (third vertices) space)))
		(bb (cl:- b a))
		(cc (cl:/ (cl:- c a) bb)))
	   (cl:* 0.5 (cl:imagpart cc) bb (cl:conjugate bb))))
      (otherwise (error "Measure not yet implemented for higher dimensions."))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Segments.

;;  Given endpoints of an arc together with other information about
;;  the arc's shape, return a segment corresponding to the arc.  The
;;  arc can go :thru a given vertex, can have a given :radius, or can
;;  have a given :center vertex.  A radius that is too small (e.g.,
;;  zero) creates a half circle.  Weird things can happen if the given
;;  center is impossible for the given endpoints or if more than one
;;  of these options is given.  Direction can be specified, either :cw
;;  or :ccw.  If direction is not specified then :ccw is the default.
;;  Uses Lisp complex numbers.
(defgeneric arc (point1 point2 space &key thru radius center clockwise cw
                        counterclockwise ccw direction)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod arc ((apoint point) (bpoint point) (space vector-space) &key
		thru radius center
		clockwise (cw clockwise)
		(counterclockwise (not cw)) (ccw counterclockwise)
		(direction (if ccw :ccw :cw)))
  ;;  Convert to complex.  A and b are endpoints of the arc; c is the center. 
  (let* ((a (complexer (coerce apoint space)))
	 (b (complexer (coerce bpoint space)))
	 (c (cond
	      ;;  If center was given, we do a simple conversion.
	      (center (complexer (coerce center space)))
	      ;;  If thru was given, we use the circumcenter.
	      (thru (complex-circumcenter a b (complexer (coerce thru space))))
	      ;;  If radius was given, we compute the center.
	      (radius (circle-center a b (convert-to-lisp-number radius)))))
	 theta-a theta-b generator)
    ;;  Swap the endpoints if not doing :ccw.
    (when (not (eql :ccw direction)) 
      (rotatef a b) (rotatef apoint bpoint))
    ;;  Determine angles and radius (the existing radius value could
    ;;  be nil, too small, or negative).
    (setf theta-a (cl:phase (cl:- a c)))
    (setf theta-b (cl:phase (cl:- b c)))
    (setf radius (cl:abs (cl:- a c)))
    ;;  Make sure direction is right.
    (unless (cl:< theta-a theta-b)
      (setf theta-b (cl:+ theta-b (cl:* 2 cl:pi))))
    ;;  Create the parametric function.
    (setf generator #'(lambda (theta)
			(let* ((ltheta (convert-to-lisp-number theta))
			       (d (cl:complex (cl:cos ltheta) (cl:sin ltheta)))
			       (transformed (cl:+ c (cl:* radius d))))
			  (make-element space (cl:realpart transformed)
					(cl:imagpart transformed)))))
    ;;  Create the segment.
    (make-instance 'curved-simplex :vertices (list apoint bpoint)
		   :home (make-parameter-space
			  #'(lambda (v) (funcall generator (ref v 0)))
			  space
			  :parameter-vectors (list (vector theta-a)
						   (vector theta-b))
			  :target-points (list apoint bpoint)))))

;;  Note that the generator here take a number (the parameter) and
;;  returns an element of the space.
(defgeneric make-curved-segment (space param1 endpoint1 param2 endpoint2 generator)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-curved-segment ((space vector-space)
				(a-param-value number) (a-endpoint point)
				(b-param-value number) (b-endpoint point)
				(generator function))
  (make-instance 'curved-simplex :vertices (list a-endpoint b-endpoint)
		 :home (make-parameter-space
			#'(lambda (v) (funcall generator (ref v 0)))
			space
			:parameter-vectors (list (vector a-param-value)
						 (vector b-param-value))
			:target-points (list a-endpoint b-endpoint))))

;;  Return the endpoint common to two segments.
(defun common-endpoint (segment-a segment-b)
  (first (intersection (vertices-of segment-a) (vertices-of segment-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Triangles.

;;  Report whether the given point is :inside, :outside, or :on the
;;  given triangle.  Works regardless of whether triangle is clockwise
;;  or counterclockwise.  Returns multiple values -- if the vertex is
;;  :on the triangle then an appropriate side or vertex is also
;;  returned.
(defgeneric point-vs-triangle (vertex triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod point-vs-triangle ((vertex point) (triangle simplex)
			      &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Point-vs-Triangle only works on triangles. ~s" triangle))
  (loop with on-side = nil
	with vertices = (vertices-of triangle)
	for va in vertices
	for vb in (rotate-list vertices)
	for bend = (bend space va vb vertex)
	collect bend into bends
	do (if (eql bend :on) (setf on-side (list va vb)))
	finally
     (cond
       ((and (member :left bends) (member :right bends)) (return :outside))
       (on-side (return (values :on (cond
				      ((0? (distance (first on-side) vertex
						     :space space))
				       (first on-side))
				      ((0? (distance (second on-side) vertex
						     :space space))
				       (second on-side))
				      (t on-side)))))
       (T (return :inside)))))
		    
;;  Return the (counterclockwise) oriented side opposite the given
;;  vertex.
(defgeneric ccw-side (vertex triangle)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod ccw-side ((vertex point) (triangle simplex))
  (let ((side (opposite vertex triangle)))
    (if (eql :left (apply #'bend (home-of triangle) vertex side))
	side (rotate-list side))))

;;  Given a set of adjacency triples of the form (left-ptr vertex
;;  right-ptr), return a list of triangles created by these triples.
;;  When triangles are created, the points are in counterclockwise
;;  order.  If a poison-vertex is given then no triangle that contains
;;  that vertex or pair of triangles that cover that vertex will be
;;  allowed until the very last triangle created.  This function takes
;;  linear time: Whenever a triangle is created, two new vertices must
;;  be inspected (they are pushed onto triples).  The sum
;;  3*(triangles-to-do) + |triples| always decreases.
(defun triangulate-triples (triples poison-vertex space triangle-class
				    &rest args)
  (loop	with left and right and triangle and relation
	;; T when we need to watch for pair of covering triangles.
	with on-flag = nil		
	with triangle-list         ;;  What we return.
	with triangles-to-do = (- (length triples) 2)
	;;  Choose the next vertex.
	while (and (> triangles-to-do 0) triples)
	for triple = (pop triples)
	for v = (second triple)
	;;   Use it if...  it hasn't been done (once underway, a
	;;   triple can appear more than once), it has a left
	;;   neighbor, it has a right neighbor, and the triangle bends
	;;   the right way.
	do (when (and v	
		      (first triple) (setq left (second (first triple)))
		      (third triple) (setq right (second (third triple)))
		      (eql :left (bend space left right v)))
	     (setf triangle (apply #'make-instance triangle-class
				   :vertices (list left right v)
				   :home space args))
	     ;;  Check for the poison-vertex.  Automatically ok if
	     ;;  there's no poison vertex or we're on the last
	     ;;  triangle.  Also, ok if this is first triangle that
	     ;;  poison-vertex is :on.
	     (setf relation (if poison-vertex
				(point-vs-triangle
				 poison-vertex triangle :space space)))
	     (when (or (null poison-vertex) (= triangles-to-do 1)
		       (eql :outside relation)
		       (and (eql :on relation) (not on-flag) (setq on-flag T)))
	       ;;  Save the triangle, cancel v, and update the
	       ;;  neighboring vertices.  The neighboring vertices
	       ;;  should be rechecked (put back on the list).
	       (decf triangles-to-do) (push triangle triangle-list)
	       (setf (second triple) nil)
	       (setf (third (first triple)) (third triple))
	       (setf (first (third triple)) (first triple))
	       (push (first triple) triples)
	       (push (third triple) triples)))
	finally (return triangle-list))) ; Return the list of triangles.

;;  Given a list of vertices that describe a star-shaped polygon in
;;  counterclockwise order and a star-source vertex, return a set of
;;  triangles that fills the star-shaped polygon.  Note that the star
;;  source is not used as a vertex in the triangulation; it's needed
;;  to make the triangulation algorithm efficient.  Planned use:
;;  retriangulate a hole in a triangulation after a single vertex has
;;  been eliminated.
(defgeneric star-triangulate (star-shape star-source space triangle-class &rest args)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod star-triangulate ((star-shape list) (star-source point)
			     (space vector-space) triangle-class 
			     &rest args)
  ;;  We convert the vertex list into a list of adjacency triples of
  ;;  the form (left v right).
  (let ((triples (mapcar #'(lambda (v) (list nil v nil)) star-shape)))
    (mapc #'(lambda (pred current succ) 
	      (setf (third current) pred) (setf (first current) succ))
	  triples (rotate-list triples) (rotate-list triples 2))
    ;;  Pass the triples to our triangulator.
    (apply #'triangulate-triples
	   triples star-source space triangle-class args)))

;;  Given a list of vertices that describe a flat polygon in
;;  counterclockwise order, return a set of triangles that fills the
;;  flat polygon.  The polygon must be illuminated by the base and all
;;  vertices must be to one side of the base.  The base is determined
;;  by the first and last vertices given.  Planned use: Triangulate
;;  the hole that appears when a constraint side is forced through a
;;  triangulation.  There is one hole on each side of such a
;;  constraint side.
(defgeneric flat-triangulate (flat-polygon space triangle-class &rest args)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flat-triangulate ((flat-polygon list) (space vector-space)
			     triangle-class &rest args)
  ;;  We convert the vertex list into a list of adjacency triples of
  ;;  the form (left v right)
  (let ((triples (mapcar #'(lambda (v) (list nil v nil)) flat-polygon)))
    (mapc #'(lambda (pred current succ)
	      (setf (third current) pred) (setf (first current) succ))
	  triples (rest triples) (rest (rest triples)))
    ;;  Pass the triples to our triangulator.
    (apply #'triangulate-triples triples nil space triangle-class args)))

;;  Return the circumcenter of the given triangle.
(defgeneric circumcenter (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod circumcenter ((triangle simplex) &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Circumcenter is implemented only for triangles. ~s" triangle))
  (let ((center (apply #'complex-circumcenter
		       (mapcar #'(lambda (v) (complexer (coerce v space)))
			       (vertices-of triangle)))))
    (make-point space (cl:realpart center) (cl:imagpart center))))

(defgeneric circumradius (triangle &key space)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod circumradius ((triangle simplex) &key (space (home-of triangle)))
  (distance (first (vertices-of triangle))
	    (circumcenter triangle :space space) :space space))

;;  Return the triangle's angle at the given vertex.
(defmethod angle ((vertex point) (triangle simplex)
		  &rest args &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Function Angle is implemented only for triangles. ~s" triangle))
  (let ((others (remove vertex (vertices-of triangle)))
	(v (coerce vertex space)))
    (apply #'angle (- (coerce (first others) space) v)
	   (- (coerce (second others) space) v) args)))

(defgeneric angles (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod angles ((triangle simplex) &key (space (home-of triangle)))
  (loop for v in (vertices-of triangle)
	collect (angle v triangle :space space)))

(defgeneric vertices-sorted-by-angle (triangle &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod vertices-sorted-by-angle ((triangle simplex) &key
				     (space (home-of triangle)))
  (loop with (a b c) = (mapcar #'(lambda (v) (coerce v space))
				 (vertices-of triangle))
	repeat 3
	for diff = (- c b)
	for size = (convert-to-lisp-number (dot-product diff diff))
	collect (cons size a) into pairs			   
	do (rotatef a b c)
	finally (return (mapcar #'rest (sort pairs #'cl:< :key #'first)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Triangulations.

(defmethod insert ((triangle simplex) (triangulation triangulation)
		   &rest ignore)
  (declare (ignore ignore)) 
  (unless (= 2 (dimension-of triangle))
    (error "Only triangles can be INSERTed into a triangulation. ~s" triangle))
  (setf (%most-recent triangulation) triangle)
  (call-next-method))

;;  Return a list of all triangles adjacent to the given vertices.
(defgeneric neighbors (vertices triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighbors ((vertices list) (triangulation triangulation))
  (case (length vertices)
    (1 (cofacets (cofacets (get-cell vertices triangulation) triangulation)
		 triangulation))
    (2 (cofacets (get-cell vertices triangulation) triangulation))))

;;  Report the neighbor of the given triangle across the given side.
(defgeneric neighbor (triangle side triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighbor ((triangle simplex) (side list)
		     (triangulation triangulation))
  (find triangle (cofacets (get-cell side triangulation) triangulation)
	:test-not #'eql))

;;  Return a list of all the triangles within a given region.  The
;;  region's boundaries are defined by the function Neighbor.  This
;;  method is not terribly efficient, but it probably won't be done
;;  all that often.
(defgeneric neighborhood (start triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod neighborhood ((start simplex) (triangulation triangulation))
  (unless (triangle? start)
    (error "Neighborhood must start at a triangle. ~s" start))
  (loop with stack = (list start)
        with mark-list = (list start)
        while stack			; Process triangles until we run out.
        for triangle = (pop stack)
        collect triangle
        ;;  Check each neighbor; if it's not on the stack then put it
        ;;  there and mark it.
        do (loop for side in (facets triangle triangulation)
		 for neighbor = (neighbor triangle (vertices-of side)
					  triangulation)
                 if (and neighbor (not (member neighbor mark-list)))
                   do (push neighbor stack)
                      (push neighbor mark-list))))

;;  Return a triangle base (a list of two points) that is between the
;;  given vertex and the corresponding triangle apex.  Triangle
;;  orientation does not matter (aside from not :flat).
(defgeneric near-base (triangle vertex &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod near-base ((triangle simplex) (vertex point)
		      &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Near-Base works only on triangles. ~s" triangle))
  (loop with (a b c) = (vertices-of triangle)
	with direction = (bend space a b c)
	repeat 3
	if (eql direction (bend space vertex c b))
	  return (list b c)
	do (rotatef a b c)))

;;  Given a triangle and a destination (a vertex), travel from the
;;  triangle to the destination.  We return the triangle that contains
;;  the destination if one exists.  Otherwise, we return multiple
;;  values: nil and the side where we fell off the triangulation.
;;  Travel is not necessarily along a straight line, although each
;;  triangle we visit is closer than the previous one.
(defgeneric directed-locate (triangle destination triangulation &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod directed-locate ((triangle simplex) (destination point)
			    (triangulation triangulation)
			    &key (space (home-of triangle)))
  (unless (triangle? triangle)
    (error "Directed-Locate must start at a triangle. ~s" triangle))
  ;;  Find a side of the triangle that points toward the destination.
  (loop for side = (near-base triangle destination :space space)
	;;  If no side found then we're done.
	do (unless side (return triangle))
	   ;;  Update triangle.  If no triangle, we've fallen off.
	   (setf triangle (neighbor triangle side triangulation))
	   (unless triangle (return (values nil side)))))

;;  Find the triangle that contains the given vertex.
(defgeneric locate (vertex triangulation)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod locate ((vertex point) (triangulation triangulation))
  ;;  Check first to see if it's already present in the triangulation...
  (or (first (neighbors (list vertex) triangulation))
      ;;  Then try a directed search...
      (let ((start (%most-recent triangulation)))
	(if (maximal-cell? start triangulation)
	    (directed-locate start vertex triangulation)))
      ;;  Finally, try looking at everything.
      (catch 'found
	(map-over-maximal-cells (triangle) triangulation
	   (if (not (eql :outside (point-vs-triangle vertex triangle)))
	       (throw 'found triangle)))
	nil)))

;;  An inefficient way to look at all triangles in a triangulation.
;;  Not used elsewhere, but handy for debugging.
(defgeneric triangles (triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod triangles ((triangulation triangulation))
  (let ((triangles nil))
    (map-over-maximal-cells (triangle) triangulation
			    (push triangle triangles))
    triangles))

;;  Report all vertices adjacent to given vertex (not necessarily in order).
(defgeneric adj-vertices (vertex triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod adj-vertices ((vertex point) (triangulation triangulation))
  (loop for edge in (cofacets (get-cell (list vertex) triangulation)
			      triangulation)
	for vertices = (vertices-of edge)
	collect (if (eql vertex (first vertices))
		    (second vertices)
		    (first vertices))))

;;  Returns T iff the given edge can be flipped without producing an
;;  inverted triangle.
(defgeneric flip-ok? (edge triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flip-ok? ((edge list) (triangulation triangulation))
  (let* ((triangles (neighbors edge triangulation))
	 (apexes (mapcar #'(lambda (tri) (first (opposite edge tri)))
			 triangles))
	 (space (home-of (first triangles))))
    (and (= 2 (length triangles))
	 (eql (apply #'bend space (first edge) apexes)
	      (apply #'bend space (second edge) (reverse apexes))))))

;;  Check if Delaunay property holds for the given edge.  The Delaunay
;;  property here is that the opposite angles sum to less than or
;;  equal to 180 degrees.  There are lots of choices on how to
;;  implement the Delaunay property (e.g., look at the circles, choose
;;  the smallest angle sum, etc.).  This one has the advantage that it
;;  also works for curved surface Delaunay triangulations.  At some
;;  point, it may be desirable to switch to a faster method.
(defgeneric delaunay? (edge triangulation &key space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod delaunay? ((edge list) (triangulation triangulation) &key space)
  (let* ((triangles (neighbors edge triangulation))
	 (t1 (first triangles))
	 (t2 (second triangles)))
    (unless space (setf space (home-of t1)))
    (or  (< (length triangles) 2)
	 (<= (+ (angle (first (opposite edge t1)) t1 :space space)
		(angle (first (opposite edge t2)) t2 :space space))
	     180.0))))

;;  Flip the given side of the triangulation.  In other words, the
;;  side implicitly defines a quadrilateral; replace the side with the
;;  other diagonal of the quadrilateral.  This function will go ahead
;;  and Flip even when inverted triangles are produced.  To avoid
;;  this, check beforehand using Flip-OK?.
(defgeneric flip (side triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod flip ((side list) (triangulation triangulation))
  (let* ((triangles (neighbors side triangulation))
	 (vert (first (opposite side (second triangles)))))
    (delete-maximal-cell (second triangles) triangulation)
    (split (first triangles) triangulation
	   :face side :splitting-point vert)))

;;  Split the given edge of the triangulation.  Expect strange results
;;  if the splitting-vertex is not on or near the edge.
(defgeneric split-edge (edge triangulation splitting-vertex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod split-edge ((edge list) (triangulation triangulation)
		       (splitting-vertex point))
  (loop with triangles = (neighbors edge triangulation)
	for triangle in triangles
	do (split triangle triangulation :face edge
		  :splitting-point splitting-vertex)))

;;  Force an edge between two existing vertices of the triangulation.
;;  In a sense, this is an extended version of Flip.
(defgeneric force-edge (edge triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod force-edge ((edge list) (triangulation triangulation))
  ;;  Don't do anything if the edge already exists.
  (unless (get-cell edge triangulation)
    ;;  We want to travel in a line across the triangulation, deleting
    ;;  the crossed triangles and accumulating the left- and
    ;;  right-chains of vertices that bound them.
    (loop with (start dest) = edge
	  with triangles = (neighbors (list start) triangulation)
	  with space and opposite
	  ;;  Determine the starting triangle; it must be one that
	  ;;  "points at" the dest.
	  with triangle
	    = (loop for tri in triangles
		    do (setf space (home-of tri))
		       (if (edges-cross? space (opposite start tri) edge)
			   (return tri)))
	  ;;  Vertices vl and vr are :left and :right (or :on) of the
	  ;;  edge, respectively.  (We are assuming that start is at
	  ;;  the bottom and dest is at the top.)
	  with (vr vl) = (ccw-side start triangle)
	  with left-chain = (list vl start)
	  with right-chain = (list vr start)
	  until (eql vr dest)		; Quit if dest is reached.
	  for side = (list vl vr)
	  for next-triangle = (neighbor triangle side triangulation)
	  ;;  Delete the old triangle and update (vl vr).
	  do (unless next-triangle
	       (error "Boundary (constraint) edges cross. ~s ~s" edge side))
	     (setf opposite (first (opposite side next-triangle)))
	     (delete-maximal-cell triangle triangulation)
	     (setf triangle next-triangle)
	     (case (bend space start dest opposite)
	       (:left (setf vl opposite) (push vl left-chain))
	       ((:right :on) (setf vr opposite) (push vr right-chain)))
	  finally       
       ;;  Get rid of the final triangle and create new triangles.
       (delete-maximal-cell triangle triangulation)
       (loop for tri in (append
			  (flat-triangulate (cons dest left-chain) space
					    (class-of triangle))
			  (flat-triangulate (reverse right-chain) space
					    (class-of triangle)))
	     do (insert tri triangulation)))))

;;  Remove a vertex and retriangulate.  The vertex must be completely
;;  surrounded by triangles for this to make sense.  Nil is returned
;;  if the operation fails.  Failure implies that no vertex is
;;  eliminated.
(defgeneric remove-vertex (vertex triangulation)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod remove-vertex ((vertex point) (triangulation triangulation))
  ;;  Determine surrounding vertices, in counterclockwise order.
  (loop with neighbors = (neighbors (list vertex) triangulation)
	with triangle = (first neighbors)
	with class = (class-of triangle)
	with space = (home-of triangle)
	with v = (first (ccw-side vertex triangle))
	repeat (length neighbors)
	for edge = (opposite v triangle)
	collect v into surrounding
	do (setf triangle (neighbor triangle edge triangulation))
	   (setf v (find vertex edge :test-not #'eql))
	   (if (null triangle) (return nil))
	finally
     ;;  Remove the old triangles and retriangulate.
     (loop for tri in neighbors do (delete-maximal-cell tri triangulation))
     (loop for tri in (star-triangulate surrounding vertex space class)
	   do (insert tri triangulation))
     (return T)))

;;  Place the given vertex into the triangulation and retriangulate.
;;  This is the main method for building a triangulation.
(defgeneric place (vertex triangulation &key triangle)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod place ((vertex point) (triangulation triangulation) &key
		  (triangle (locate vertex triangulation)))
  ;;  Do nothing if the vertex already appears in the triangulation.
  (unless (get-cell (list vertex) triangulation)
    (unless triangle
      (error "Unable to PLACE; no triangle found. ~s" vertex))
    (unless (member triangle triangulation)
      (error "Unable to PLACE; triangle has been deleted. ~s" triangle))
    (multiple-value-bind
	(relation hit) (point-vs-triangle vertex triangle)
      (case relation
	;;  Inside of triangle; this is the normal case.
	(:inside (split triangle triangulation :splitting-point vertex))
	;;  On triangle.  Here we have to be careful; we've landed on
	;;  the triangle's boundary.  This case should be rare.
	(:on (cond ((listp hit)
		    (split-edge hit triangulation vertex))
		   (t
		    (warn "Placed ~s atop existing vertex ~s." vertex hit)
		    (unless (remove-vertex hit triangulation)
		      (error "Cannot Remove ~s, so cannot Place ~s"
			     hit vertex))
		    (place vertex triangulation))))
	;;  Outside the triangle.  This case should never happen.
	(:outside
	 (error "Cannot PLACE; ~s is :outside ~s." vertex triangle))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Constrained-Triangulations (C-Triangulations).
;;
;;  A triangulation in which some edges (the constraint edges) and
;;  vertices (the constraint vertices) are special.  Constraint edges
;;  cannot be flipped (although they can be Split).  Edges adjacent to
;;  exactly one triangle also cannot be flipped and ideally these
;;  should be constraint edges although this is not enforced.
;;  Constraint segments act as barriers through the use of function
;;  Neighbor; this function normally returns a neighbor, but it cannot
;;  see past a constraint edge when *cross-edges* is nil.  Most
;;  triangulation operations are inherited unchanged, but some have
;;  new behaviors.

;;  Given a list of vertices, return the corresponding constraint.
;;  Return nil if there is no such constraint.  To make sense, the
;;  list should be of length 1 or 2.  This is the main tool for
;;  querying the constraints table of the c-triangulation.
(defgeneric constraint (vertices triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod constraint ((vertices list) (c-triangulation c-triangulation))
  (get-cell vertices (%constraints-of c-triangulation)))

;;  Modified Neighbor (uses *cross-edges*).
(defmethod neighbor ((triangle simplex) (side list)
		     (c-triangulation c-triangulation))
  (unless (and (not *cross-edges*) (constraint side c-triangulation))
    (call-next-method)))

(defmethod flip-ok? ((edge list) (c-triangulation c-triangulation))
  (unless (constraint edge c-triangulation)
    (call-next-method)))

(defmethod flip ((edge list) (c-triangulation c-triangulation))
  (if (constraint edge c-triangulation)
      (error "Cannot flip a constraint edge. ~s" edge)
      (call-next-method)))

(defmethod split-edge ((edge list) (c-triangulation c-triangulation)
		       (splitting-vertex point))
  (if (constraint edge c-triangulation)
      (error "Use Split to split a constraint edge. ~s" edge)
      (call-next-method)))

;;  Force an edge between existing vertices.  *Cross-edges* is set so
;;  that the operation fails if an attempt is made to force an edge
;;  through a constraint edge.
(defmethod force-edge ((edge list) (c-triangulation c-triangulation))
  (let ((*cross-edges* nil))
    (call-next-method)))

(defmethod remove-vertex ((vertex point) (c-triangulation c-triangulation))
  (unless (constraint (list vertex) c-triangulation)
    (call-next-method)))

;;  Place segment or single vertex into a c-triangulation as a
;;  constraint.  Note that Place acts differently when placing a
;;  dimension-zero simplex than when placing a point.
(defmethod place ((simplex simplex) (c-triangulation c-triangulation)
		  &rest ignore)
  (declare (ignore ignore))
  (when (> (dimension-of simplex) 1)
    (error "Only segments and vertices can be PLACEd. ~s" simplex))
  (insert simplex (%constraints-of c-triangulation)) ; Must do this first.
  (loop for v in (vertices-of simplex)
	do (place v c-triangulation))
  (when (segment? simplex)
    (force-edge (vertices-of simplex) c-triangulation)))

;;  Split triangle on one side of a constraint that is being split.
(defgeneric %split-constraint-one-side (edge triangle new-vertex concave triangulation)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod %split-constraint-one-side ((edge list) (triangle simplex)
				       (new-vertex point) concave
				       (c-triangulation c-triangulation))
  (cond       
    ;;  Difficult case: we are on the same side as the triangle's apex
    ;;  and the new-vertex is :outside the triangle.
    ((and concave (eql :outside (point-vs-triangle new-vertex triangle)))
     (let ((old-neighbors (neighbors edge c-triangulation))
	   (hit-tri (directed-locate triangle new-vertex c-triangulation)))
       (place new-vertex c-triangulation :triangle hit-tri)
       (force-edge (list new-vertex (first edge)) c-triangulation)
       (force-edge (list new-vertex (second edge)) c-triangulation)
       (loop with new = (first
			  (set-difference (neighbors edge c-triangulation)
			      old-neighbors))
	     for tri in (neighborhood (or new triangle) c-triangulation)
	     do (delete-maximal-cell tri c-triangulation))))
    ;;  Easy case: replace a single triangle with 2 new ones.
    (T (split triangle c-triangulation :face edge :splitting-point new-vertex))
    ))

;;  Allow the splitting of a constraint segment.  We return the two
;;  new segments.  Splitting a constraint is a pain; there doesn't
;;  seem to be any way to do it that is esthetically pleasing.
(defmethod split ((simplex simplex) (c-triangulation c-triangulation)
		  &rest args)
  (cond
    ((triangle? simplex) (call-next-method))
    ((segment? simplex)
     (loop with points = (vertices-of simplex)
	   with new-segments = (apply #'split simplex
				      (%constraints-of c-triangulation) args)
	   with new-vert = (apply #'common-endpoint new-segments)
	   ;;  We temporarily re-insert the segment.
	   initially (insert simplex (%constraints-of c-triangulation))

	   ;;  Do each side of the old segment.
	   for triangle in (neighbors points c-triangulation)
	   for space = (home-of triangle)
	   for apex = (first (opposite points triangle))
	   for verts = (vertices-of triangle)
	   for concave = (eql (apply #'bend space (subst new-vert apex verts))
			      (apply #'bend space verts))
	   do (%split-constraint-one-side points triangle new-vert
		     concave c-triangulation)
	   ;;  Finally, we remove the old segment as a constraint.
	   finally (delete-maximal-cell simplex
					(%constraints-of c-triangulation))
		   (return new-segments)))
    (T (error "Can only SPLIT segments and triangles. ~s" simplex))))
	   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  CDTs (Constrained Delaunay Triangulations).
;;
;;  A constrained-triangulation with the empty circle property.
;;  Delaunay properties are maintained via the function Insert; each
;;  newly inserted triangle is checked.  Delaunay checking can be
;;  turned off by setting *delaunay* to nil.  *Delaunay* is also used
;;  internally to decrease the amount of checking done when a new
;;  vertex is inserted via Place (this also prevents an infinite loop
;;  that can occur when 4 points are nearly cocircular).

;;  Constraint edge always satisifies Delaunay property.
(defmethod delaunay? ((edge list) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (if (constraint edge cdt) T (call-next-method)))

;;  Check Delaunay properties on insertion.
(defmethod insert ((triangle simplex) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (call-next-method)
  (cond
    ((typep *delaunay* 'point)
     (let ((side (opposite *delaunay* triangle)))
       (unless (delaunay? side cdt) (flip side cdt))))
    (*delaunay* (loop for edge in (facets triangle cdt)
		      for side = (vertices-of edge)
		      if (not (delaunay? side cdt))
			do (flip side cdt) (return)))))

;;  Limit delaunay checking when placing a new vertex.
(defmethod place ((vertex point) (cdt cdt) &rest ignore)
  (declare (ignore ignore))
  (let ((*delaunay* vertex))
    (call-next-method)))

;;  Limit delaunay checking.
(defmethod split-edge ((edge list) (cdt cdt) (splitting-vertex point))
  (let ((*delaunay* splitting-vertex))
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Meshes.
;;
;;  A mesh is basically a CDT with names and a home-space.
;;  Refine-Mesh can be used to improve a mesh.

(defmethod insert ((triangle simplex) (mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (call-next-method)
  ;;  Save new triangles for later processing.
  (when (member triangle mesh) (push triangle (%pending-list-of mesh))))

(defgeneric dimension-of (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod dimension-of ((mesh mesh))
  (dimension-of (home-of mesh)))

;;  When a new vertex is placed into a mesh, we want to avoid
;;  introducing new edges that are especially small, so we remove any
;;  vertices that are too close to the new vertex.  A proof shows that
;;  only adjacent vertices (those adjacent to the new vertex) need to
;;  be considered.  [There can be vertices closer that are not
;;  adjacent, but if such a vertex exists then there are already short
;;  edges in the vicinity anyway.]  The idea is to use this only for
;;  vertices that are forced upon us by being part of a boundary.
;;  Thus, we use this when we split boundary edges.  This also needs
;;  to be used if we insert boundary edges in among existing mesh
;;  points (the current version doesn't allow this).
(defgeneric %delete-too-close (vertex mesh too-close)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod %delete-too-close ((vertex point) (mesh mesh) too-close)
  (loop with space = (home-of mesh)
	for v in (adj-vertices vertex mesh)
	if (< (distance vertex v :space space) too-close)
	  do (remove-vertex v mesh)))

;;  The following 3 methods (remove-vertex,
;;  %split-constraint-one-side, and split) cover all the ways that new
;;  triangles or boundaries can be introduced into a mesh during
;;  refine-mesh.  These methods handle the necessary updating of
;;  names.
(defmethod remove-vertex ((vertex point) (mesh mesh))
  (setf (%default-name-of mesh)
	(name (first (neighbors (list vertex) mesh)) mesh))
  (call-next-method))

;;  Name updating.
(defmethod %split-constraint-one-side ((edge list) (triangle simplex)
				       (new-vertex point) concave
				       (mesh mesh))
  (setf (%default-name-of mesh) (name triangle mesh))
  (call-next-method))

;;  Name updating along with code to delete any vertices too-close to
;;  a new boundary vertex.
(defmethod split ((simplex simplex) (mesh mesh) &rest ignore)
  (declare (ignore ignore))
  (cond
    ((triangle? simplex)
     (setf (%default-name-of mesh) (name simplex mesh))
     (call-next-method))
    ((segment? simplex)
     (setf (%default-name-of (%constraints-of mesh)) (name simplex mesh))
     (let* ((new-segments (call-next-method))
	    (new-vert (apply #'common-endpoint new-segments))
	    (points (vertices-of simplex))
	    (space (home-of mesh))
	    (too-close (min (distance new-vert (first points) :space space)
			    (distance new-vert (second points) :space space))))
           ;;  We throw out vertices that are clearly too close, but
           ;;  the factor *too-close-factor* (constant less than 1)
           ;;  prevents us from throwing away too many -- at the cost
           ;;  of generating some edges that are possibly shorter than
           ;;  necessary.
       (%delete-too-close new-vert mesh (* *too-close-factor* too-close))
       new-segments))
    (T (call-next-method))))

;;  Place a boundary and set its name.
(defmethod place ((simplex simplex) (mesh mesh) &key name &allow-other-keys)
  (call-next-method)
  (when (and name (segment? simplex)) (setf (name simplex mesh) name)))

;;  Return the center of the given triangle.  What we mean by the
;;  center depends on the type of mesh that we are doing or on what
;;  approximation we are using for the circumcenter.  Here, we do a
;;  straightforward standard circumcenter.
(defgeneric triangle-center (triangle mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod triangle-center ((triangle simplex) (mesh mesh))
  (unless (eql (home-of triangle) (home-of mesh))
    (warn "Possible improper use of Triangle-Center."))
  (circumcenter triangle))

;;  Checks if given simplex is larger than the-bound.  The-bound can
;;  be nil or a number or a function of two numbers that returns a
;;  number.  The function is evaluated at the mean of the vertices of
;;  the simplex (calculated in space).  Nil effectively acts as
;;  infinity.
(defgeneric too-big? (simplex the-bound space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod too-big? ((simplex simplex) (the-bound T) (space vector-space))
  (let ((bound (cond
		 ((null the-bound) nil)
		 ((numberp the-bound) the-bound)
		 ((functionp the-bound)
		  (apply the-bound (coordinate-list
				    (make-mean-point (vertices-of simplex)
						     :mean-space space
						     :point-space space))))
		 (T (warn "Ignoring unknown bound ~s in Too-Big?" the-bound)
		    nil))))
    (if bound (< bound (simplex-size simplex space)))))

;;  Returns a grade for the given triangle showing whether the
;;  triangle needs to be improved.
(defgeneric grade (triangle mesh angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod grade ((triangle simplex) (mesh mesh)
		  (angle-bounds list) (size-list list))
  (let* ((space (home-of mesh))
	 (vertices (vertices-sorted-by-angle triangle :space space)))
    (cond
      ;;  Largest angle too large.
      ((and (second angle-bounds)
	    (> (angle (third vertices) triangle :space space)
	       (second angle-bounds)))
       :bad-shape)
      ;;  Smallest angle too small (and not a boundary angle).
      ((and (first angle-bounds)
	    (< (angle (first vertices) triangle :space space)
	       (first angle-bounds))
	    (or (not (constraint (opposite (second vertices) triangle) mesh))
		(not (constraint (opposite (third vertices) triangle) mesh))))
       :bad-shape)
      ;;  Size too big.
      ((too-big? triangle
		 (second (member (name triangle mesh) size-list)) space)
       :bad-size)
      ;;  Bounday size too big.
      ((loop for edge in (facets triangle mesh)
	     for segment = (constraint (vertices-of edge) mesh)
	     if (and segment
		     (too-big? segment (second (member (name segment mesh)
						       size-list)) space))
	       return T)
       :bad-segment)
      (T :good))))

;;  Improve the triangle.  Different techniques are used depending on
;;  whether the triangle is obtuse, near-right, or acute.  A proof
;;  shows this works and has the advantage of avoiding the calculation
;;  of circumcenters for obtuse triangles.
(defgeneric improve (triangle mesh)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod improve ((triangle simplex) (mesh mesh))
  (let* ((space (home-of mesh))
	 (vertex (third (vertices-sorted-by-angle triangle :space space)))
	 (max-angle (angle vertex triangle :space space))
	 (side (opposite vertex triangle))
	 (constraint (constraint side mesh)))
    (cond
      ;;  Acute triangle.  Place the circumcenter.
      ((< max-angle 89.9)
       (place (triangle-center triangle mesh) mesh :triangle triangle))
      
      ;;  The long side is a boundary.  Split the boundary.
      (constraint (split constraint mesh))

      ;;  Obtuse triangle.  Recursively split the neighboring triangle.
      ((> max-angle 90.1) (improve (neighbor triangle side mesh) mesh))

      ;;  Near-right triangle; hypotenuse is not a boundary.  Break
      ;;  the hypotenuse.
      (t (split-edge side mesh (make-mean-point side
						:mean-space (home-of triangle)
						:point-space space))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Initializations used by the Mesher Interface.

;;  Create a background-box for the mesh.  This is a box that contains
;;  all the boundaries of the mesh.  Used to initialize the mesh's
;;  CDT.
(defgeneric do-background-box (box border mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod do-background-box ((box list) (border number) (mesh mesh))
  (let* ((space (home-of mesh))
	 (low (mapcar #'(lambda (c) (cl:- c border)) (first box)))
	 (high (mapcar #'(lambda (c) (cl:+ c border)) (second box)))
	 (ll (apply #'make-point space low))
	 (hh (apply #'make-point space high))
	 (lh (make-point space (first low) (second high)))
	 (hl (make-point space (first high) (second low))))
    (insert (make-simplex ll hl hh) mesh)
    (insert (make-simplex ll hh lh) mesh)))

;;  Initialization needed in Name-Region.
(defgeneric build-cdt-from-boundaries (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod build-cdt-from-boundaries ((mesh mesh))
  ;;  Determine bounding-box and build containing rectangle.
  (loop with box =  (bounding-box (mapcar #'first (%pending-list-of mesh))
				  (home-of mesh))
	with boundaries = (reverse (%pending-list-of mesh))
	initially (setf (%pending-list-of mesh) nil)
		  (do-background-box box (* 0.1 (apply #'distance box)) mesh)
	for (boundary name) in boundaries
	do (place boundary mesh :name name)))

;;  Initialization needed in Refine-Mesh.
(defgeneric refine-mesh-prep (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod refine-mesh-prep ((mesh mesh))
  ;;  Make sure control variables are set properly.
  (setf *delaunay* T)
  (setf *cross-edges* nil)
  (setf *mesh* mesh)
  (setf *space* (home-of mesh))
  ;;  Place exsting triangles into pending-list in standard order
  ;;  (this ensures we get the same results on different runs).  Also,
  ;;  we get rid of unnamed triangles here.
  (let ((triangles nil))
    (map-over-maximal-cells (tri) mesh
      (if (name tri mesh)
	  (push tri triangles)
	  (delete-maximal-cell tri mesh)))
  (setf (%pending-list-of mesh) (sort triangles #'cl:> :key #'id-number-of))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Mesher Interface.
;;
;;  These are the functions that form the major part of the mesh
;;  interface.  All of them appear in the manual.
(defgeneric boundary-complex-of (mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod boundary-complex-of ((mesh mesh))
  (%constraints-of mesh))

;;  Create an empty mesh.
(defgeneric create-mesh (space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod create-mesh ((space euclidean-space))
  (setf *mesh* (make-instance 'mesh :home space)))

;;  Insert boundaries into the mesh during mesh-initialization.  Can
;;  insert a 0-dimensional simplex, a 1-dimensional simplex (a
;;  segment), or a simplicial-complex containing such simplices.
(defgeneric insert-boundary (simplex mesh &key name)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod insert-boundary ((simplex simplex) (mesh mesh) &key name)
  (when (%most-recent mesh)
    (error "Misuse of Insert-Boundary.  Use only during mesh initialization."))
  (when (< 1 (dimension-of simplex))
    (error "Illegal boundary simplex.  Only dimension 0 or 1 allowed. ~s"
	   simplex))
  (push (list simplex name) (%pending-list-of mesh)))

;;  Name a subregion of the mesh.  The first time this is called it
;;  builds the CDT of the boundaries.
(defgeneric name-region (name point mesh)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod name-region ((name symbol) (point point) (mesh mesh))
  ;;  Build the CDT on the first call.
  (unless (%most-recent mesh)
    (build-cdt-from-boundaries mesh))
  ;;  Region naming.
  (mapcar #'(lambda (tri) (setf (name tri mesh) name))
	  (neighborhood (locate point mesh) mesh))
  mesh)

;;  Check and improve each triangle of the mesh, including new ones.
(defgeneric refine-mesh (mesh &key angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod refine-mesh ((mesh mesh) &key (angle-bounds '(30)) (size-list nil))
  (refine-mesh-prep mesh)
  ;;  Pull triangles from the improvement queue until the queue is empty.
  (loop with triangle
	with improvement-queue = (make-queue)
     	do
     ;;  Load any pending triangles into the improvement queue.
     (loop for tri in (%pending-list-of mesh)
	   do (when (and (maximal-cell? tri mesh)
			 (not (eql (grade tri mesh angle-bounds size-list)
				   :good)))
		(insertq tri improvement-queue)))
     (setf (%pending-list-of mesh) nil)
     (when (emptyq? improvement-queue) (return))
     (setf triangle (getq improvement-queue :delete nil))
     (if (maximal-cell? triangle mesh)
	 (improve triangle mesh)
	 (getq improvement-queue :delete T))) ; Delete only when really gone.
  mesh)

(defmacro make-mesh ((space &rest keyargs &key size-list angle-bounds)
		     &body body)
  (declare (ignore size-list angle-bounds))
  (loop for clause in body
	for (type name . rest) = (if (listp clause) clause
				     (error "Improper clause in Make-Mesh: ~s"
					    clause))
	collect 
	(cond				; string= allows use across packages.
	  ((string= type "POINT")
	   `(setf (gethash ',name %pt-table) (make-point *space* ,@rest)))
	  ((string= type "REGION")
	   `(setf (gethash ',name %rg-table) (make-point *space* ,@rest)))
	  ((string= type "BOUNDARY")
	   `(%mm-boundary ,@(rest clause)))
	  (t (error "Unknown descriptor in Make-Mesh: ~s" clause)))
	into new-body
	finally
     (return `(let ((%pt-table (make-hash-table))
		    (%rg-table (make-hash-table)))
		(setf *space* ,space)
		(setf *mesh* (create-mesh *space*))
		(gethash nil %pt-table) ; Prevents a warning message.
		,@new-body
		(maphash #'(lambda (name point)
			     (name-region name point *mesh*)) %rg-table)
		(refine-mesh *mesh* ,@keyargs)
		*mesh*))))

(defmacro %mm-get-point (desc)
  (cond
    ((atom desc) `(or (gethash ',desc %pt-table)
		      (error "Unbound point name: ~s" ',desc)))
    ((string= (first desc) "PT") `(make-point *space* ,@(rest desc)))
    (t `(error "Unknown point descriptor: ~s" ',desc))))
		
(defmacro %mm-boundary (name options type . desc)
  (cond
    ((string= type "LINE")
     `(%boundary-line (list ,@(mapcar #'(lambda (x) `(%mm-get-point ,x)) desc))
		      ',name ,@options))
    ((string= type "ARC")
     (loop while desc
	   for (point arc-args) = desc
	   collect `(%mm-get-point ,point) into new-desc
	   collect
	   (if (listp arc-args)
	       (let ((part (or (member :thru arc-args)
			       (member :center arc-args))))
		 (when (second part)
		   (setf (second part) `(%mm-get-point ,(second part))))
		 `(list ,@arc-args))
	       `(error "Improperly formed arc options: ~s" ',arc-args))
	   into new-desc
	   do (setf desc (rest (rest desc)))
	   finally
	(return `(%boundary-arc (list ,@new-desc) ',name ,@options))))
    (t `(error "Unknown type in boundary descriptor: ~s" ',type))))

;;  Macro to handle the boundary construction.  Handles the boundary
;;  options :closed? and :split.  Also inserts the new boundaries into
;;  the mesh.  To use this, the body is expected to return a list of
;;  the boundary simplices.
(defmacro defun-boundary (function-name (point-list name) &body body)
  `(defun ,function-name (,point-list ,name &key (closed? nil) (split 0))
     (if closed? (setf ,point-list
		       (append ,point-list (list (first ,point-list)))))
     (loop with boundaries = (progn ,@body)
	   repeat split
	   do (setf boundaries (split boundaries nil))
	   finally
	(loop for b in boundaries
	      do (insert-boundary b *mesh* :name ,name)))))

(defun-boundary %boundary-line (points name)
  (loop for a in points
	for b in (rest points)
	collect (make-simplex a b)))

(defun-boundary %boundary-arc (point-arc-list name)
  (loop for (apoint arc-args bpoint) = point-arc-list
	while bpoint
	collect (if arc-args
		    (apply #'arc apoint bpoint *space* arc-args)
		    (make-simplex apoint bpoint))
	do (setf point-arc-list (rest (rest point-arc-list)))))

;;  Macro for building 2D meshes.  Creates a function that can create
;;  a mesh.  The function can take keyword arguments to control the
;;  mesh requirements.  Defaults for the keyword arguments can be
;;  specified within defmesh.
(defmacro defmesh (name (&key size-list angle-bounds) &body body)
  `(defun ,name (&key (size-list ,size-list)
		      (angle-bounds (or ,angle-bounds '(30))))
     (make-mesh ((get-euclidean-space 2)
		 :size-list size-list :angle-bounds angle-bounds)
		,@body)))

(defmesh circle (:size-list '(circle-boundary .1))
  (region inside 0 0)
  (point origin 0 0)
  (boundary circle-boundary (:closed? t) arc
	    (pt 1 0) (:center origin) (pt 0 1) (:center origin)
	    (pt -1 0) (:center origin) (pt 0 -1) (:center origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  File I/O.
(defgeneric make-mesh-from-file (stream &key angle-bounds size-list)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod make-mesh-from-file ((stream stream) &key angle-bounds size-list)
  (unless (string= (read stream) "MESHREQUEST")
    (error "File should start with token `MeshRequest'."))
  (loop with space and boundary-complex and region-points
	and file-angle-bounds and file-size-list
	with vertex-table = (make-hash-table)
	for token = (read stream nil '%EOF%)
	until (string= token "%EOF%")
	do
     (cond
       ((string= token "VERTEXSET")
	(when space
	  (error "Multiple VertexSets in file."))
	(setf space (get-euclidean-space (read stream)))
	(setf *space* space)
	(read-vertex-set stream vertex-table space))
       ((string= token "SIMPLICIALCOMPLEX")
	(unless space
	  (error "VertexSet must be defined before SimplicialComplex."))
	(setf boundary-complex
	      (read-boundary-simplicial-complex stream vertex-table)))
       ((string= token "REGIONS")
	(setf region-points (read-region-points stream space)))
       ((string= token "ANGLEBOUNDS")
	(setf file-angle-bounds (list (read stream) (read stream))))
       ((string= token "SIZETABLE")
	(setf file-size-list (read-size-table stream)))
       (t
	(error "Confused in file; cannot understand this token: ~s" token)))
	finally
     (let ((mesh (create-mesh space)))
       (map-over-maximal-cells (boundary) boundary-complex
	 (insert-boundary boundary mesh
			  :name (name boundary boundary-complex)))
       (loop for (name point) in region-points
	     do (name-region name point mesh))
       (refine-mesh mesh :angle-bounds (or angle-bounds file-angle-bounds)
		    :size-list (or size-list file-size-list))
       (return mesh))))

(defgeneric read-point (stream space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-point ((stream stream) (space domain))
  (apply #'make-point space (loop repeat (dimension-of space)
				  collect (read stream))))

(defgeneric read-vertex-set (stream vertex-table space)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-vertex-set ((stream stream) (vertex-table hash-table)
			    (space domain))
  (loop with numvertices = (read stream)
	for i below numvertices
	do (setf (gethash i vertex-table) (read-point stream space))))
    
;;  If a complex is passed in via keyword then new simplices are added
;;  to that complex.
(defgeneric read-simplicial-complex (stream vertex-table &key complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-simplicial-complex
    ((stream stream) (vertex-table hash-table)
     &key (complex (make-instance 'named-simplicial-complex)))
  (loop with simp-dimension = (read stream)
	with numsimps = (read stream)
	with current-name = nil and count = 0
	until (>= count numsimps)
	for next = (read stream)
	if (typep next 'symbol) do
	  (setf current-name next)
	else do
	  (insert (apply #'make-simplex (gethash next vertex-table)
			 (loop repeat simp-dimension
			       collect (gethash (read stream) vertex-table)))
		  complex :name current-name)
	  (incf count))
  complex)

;;  Special code for boundaries so that arcs can be used.  This is
;;  basically a hack that is here until we get a chance to put in
;;  general code for curved objects.
(defgeneric read-boundary-simplicial-complex (stream vertex-table &key complex)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-boundary-simplicial-complex
    ((stream stream) (vertex-table hash-table)
     &key (complex (make-instance 'named-simplicial-complex)))
  (loop with simp-dimension = (read stream)
	with numsimps = (read stream)
	with current-name = nil and count = 0
	until (>= count numsimps)
	initially (unless (and (numberp simp-dimension) (= simp-dimension 1))
		    (error "Use 1 for boundary SimplicialComplex, not ~s."
			   simp-dimension))
	for next = (read stream)
	if (typep next 'symbol) do
	  (setf current-name next)
	else do
	  (let* ((arc-args (read stream))
		 (other arc-args))
	    (if (listp arc-args)
		(setf other (read stream))
		(setf arc-args nil))
	    (when arc-args
	      (let ((code (string (first arc-args))))
		(unless (= 2 (length arc-args))
		  (error "Badly formed arc descriptor: ~s" arc-args))
		(setf (first arc-args)
		      (case (aref code 0)
			(#\C :center)
			(#\T :thru)
			(#\R :radius)
			(otherwise (error "Unrecognized arc descriptor: ~s"
					  arc-args))))
		(unless (numberp (second arc-args))
		  (error "Second element should be a number: ~s" arc-args))
		(unless (eql (first arc-args) :radius)
		  (setf (second arc-args)
			(gethash (second arc-args) vertex-table)))
		;;  Any more than single character implies clockwise.
		(if (> (length code) 1)
		    (setf arc-args (append arc-args '(:cw t))))))
	    (setf next (gethash next vertex-table))
	    (setf other (gethash other vertex-table))
	    (insert (if arc-args
			(apply #'arc next other *space* arc-args)
			(make-simplex next other))
		    complex :name current-name)
	    (incf count)))
  complex)

(defgeneric read-region-points (stream space)
  (:documentation
   "The purspose of this function is unknown."))

(defmethod read-region-points ((stream stream) (space domain))
  (loop with numpoints = (read stream)
	repeat numpoints
	for name = (read stream)
	for point = (read-point stream space)
	collect (list name point)
	do (unless (typep name 'symbol)
	     (error "Bad name for Region: ~s ~s" name point))))

(defgeneric read-size-table (stream)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod read-size-table ((stream stream))
  (loop with numentries = (read stream)
	repeat numentries
	for name = (read stream)
	for size = (read stream)
	collect name
	collect size
	do (unless (and (typep name 'symbol) (typep size 'number))
	     (error "Improper entry for SizeTable: ~s ~s" name size))))

(defgeneric fwrite (tuple stream &rest ignore)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod fwrite ((tuple tuple) stream &rest ignore)
  (declare (ignore ignore))
  (apply #'format stream "~s~@{ ~s~}~%"
	 (loop for i below (dimension-of (domain-of tuple))
	       collect (ref tuple i))))

;;  Write the vertex set of anything for which we can do
;;  map-over-cells.  The vertex-table is returned.
(defgeneric write-vertex-set (thing stream)
  (:documentation
   "The purpose of this function is unknown."))

(defmethod write-vertex-set ((thing t) stream)
  (let ((count 0)
	(space nil)
	(vertex-table (make-hash-table)))
    (map-over-cells (vertex 0) thing
	 (setf vertex (first (vertices-of vertex)))
	 (incf count)
	 (if space
	     (unless (eql space (domain-of vertex))
	       (error "Mismatched vertex domains in ~s." thing))
	     (setf space (domain-of vertex))))
    (format stream "VertexSet ~s ~s~%" (dimension-of space) count)
    (setf count 0)
    (map-over-cells (vertex 0) thing
	 (setf vertex (first (vertices-of vertex)))
	 (fwrite vertex stream)
	 (setf (gethash (id-number-of vertex) vertex-table) count)
	 (incf count))
    vertex-table))

(defmethod fwrite ((simplex simplex) stream &key
		   (vertex-table (write-vertex-set simplex stream)))
  (apply #'format stream "~s~@{ ~s~}~%"
	 (loop for v in (vertices-of simplex)
	       collect (gethash (id-number-of v) vertex-table))))

(defmethod fwrite ((sc simplicial-complex) stream &key
		   (vertex-table (write-vertex-set sc stream)))
  (let ((count 0)
	(max-dimension nil)
	(names (all-names sc)))
    (map-over-maximal-cells (cell) sc
      (incf count)
      (if max-dimension
	  (unless (= max-dimension (dimension-of cell))
	    (error "Inconsistent maximal-cell dimensions in ~s." sc))
	  (setf max-dimension (dimension-of cell))))
    (format stream "SimplicialComplex ~s ~s~%" max-dimension count)
    (if names
	(loop for name in names do
	  (format stream "~s~%" name)
	  (map-over-cells (cell max-dimension) sc
	      (when (eql name (name cell sc))
		(fwrite cell stream :vertex-table vertex-table))))
	(map-over-cells (cell max-dimension) sc
	    (fwrite cell stream :vertex-table vertex-table)))))

(defmethod fwrite ((mesh mesh) stream &rest ignore)
  (declare (ignore ignore))
  (format stream "Mesh~%")
  (let ((vertex-table (write-vertex-set mesh stream)))
    (fwrite (%constraints-of mesh) stream :vertex-table vertex-table)
    (call-next-method mesh stream :vertex-table vertex-table)))

;;  User interface.
(defgeneric write-mesh (mesh stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod write-mesh ((mesh mesh) stream)
  (unless stream (setf stream *standard-output*))
  (fwrite mesh stream))

(defgeneric read-mesh (stream)
  (:documentation
   "The purpose of this method is unknown."))

(defmethod read-mesh ((stream stream))
  (let ((token (read stream))
	(vertex-table (make-hash-table))
	(space nil)
	(mesh nil))
    (when (string= token "MESH") (setf token (read stream)))
    ;;  Vertex set.
    (unless (string= token "VERTEXSET")
      (error "Expected VertexSet instead of ~s." token))
    (setf space (get-euclidean-space (read stream)))
    (setf *space* space)
    (setf mesh (create-mesh space))
    (read-vertex-set stream vertex-table space)
    ;;  Boundary complex.
    (setf token (read stream))
    (unless (string= token "SIMPLICIALCOMPLEX")
      (error "Expected boundary SimplicialComplex instead of ~s." token))
    (read-simplicial-complex stream vertex-table
			     :complex (%constraints-of mesh))
    ;;  Mesh complex.
    (setf token (read stream))
    (unless (string= token "SIMPLICIALCOMPLEX")
      (error "Expected SimplicialComplex instead of ~s." token))
    (read-simplicial-complex stream vertex-table :complex mesh)
    ;;  End of file.
    (setf token (read stream nil '%eof%))
    (unless (eql token '%eof%)
      (warn "Unexpected data instead of end-of-file. ~s" token))
    mesh))  

