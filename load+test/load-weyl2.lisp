; some silence ...
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)
(declaim (sb-ext:muffle-conditions style-warning))

(defparameter group1 (list 
"closer-mop/closer-mop-packages"
"closer-mop/closer-mop-shared"
"closer-mop/closer-sbcl"))

(defparameter group2 (list 
"weyl/packages"
"weyl/lisp-support"
"weyl/domain-support"))
 
(defparameter group3 (list  
"weyl/classes/algebraic-domains"
"weyl/classes/space-classes"
"weyl/classes/general-classes"))
 
(defparameter group4 (list 
"weyl/avl"
"weyl/lisp-numbers"
"weyl/sets"
"weyl/morphisms"
"weyl/quotient-fields"
"weyl/general" 
"weyl/fourier"
"weyl/functions"
"weyl/direct-sums"))

(defparameter group5 (list 
"weyl/numbers/bigfloat"
"weyl/numbers/numbers"
"weyl/numbers/gfp"))

(defparameter group6 (list 
"weyl/polynomials/poly-tools"
"weyl/polynomials/mpolynomial"
"weyl/polynomials/upolynomial"
"weyl/polynomials/epolynomial"
"weyl/polynomials/sparsegcd"
"weyl/polynomials/grobner"))
    
(defparameter group7 (list     
"weyl/tpower"
"weyl/taylor"
"weyl/rational-functions"
"weyl/differential-domains"
"weyl/algebraic-extension"))
   
(defparameter group8 (list 
"weyl/vector-spaces/vector"
"weyl/vector-spaces/projective-space"
"weyl/vector-spaces/quaternions"))
 
(defparameter group9 (list  
"weyl/matrix"
"weyl/topology"
"weyl/funct-spaces"
"weyl/mesh"))

;; (weyl/new) <-- Sat 1 Mar 22:09:56 CET 2025/KFP
(defparameter group10 (list 
"weyl/new/weyl-infix"
"weyl/new/ge-support"
"weyl/new/ge-latex"))

(defparameter group11 (list 
"load+test/user-manual"))


;; some files have to be compiled to get full functionality.
;(compile-file "../weyl/numbers/numbers.lisp")   ;; get-rational-...
;(compile-file "../weyl/tpower.lisp")            ;; taylor

;; we also load MKs user-manual here.
;(load "user-manual")

(defparameter parent-of-weyl-folder "../")

(defun load-group (g)
  (loop for f in g do 
    (load (format nil "~A~A" parent-of-weyl-folder f))))
    
(defun compile-group (g)
  (loop for f in g do 
    (compile-file (format nil "~A~A" parent-of-weyl-folder f))))
    
(defun document-group (g)
  (loop for f in g do 
    (progn (format t "~A~%" f)
      (ignore-errors 
        (create-user-manual 
           (format nil "~A~A.lisp" parent-of-weyl-folder f))))))

(defparameter all (concatenate 'list
  group1 group2 group3 group4 group5 group6 group7 
         group8 group9 group10 group11))

(load-group all)
(compile-group all)

(document-group group2)

;(defmethod perform :after ((op load-op) (comp (eql (find-system "weyl"))))
;  "Initialize and reset the contexts."

(pushnew :weyl *features*)
(funcall (intern "INITIALIZE-CONTEXTS" :weyli))
(funcall (intern "RESET-DOMAINS" :weyli))
