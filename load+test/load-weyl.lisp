; some silence ...
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)
(declaim (sb-ext:muffle-conditions style-warning))


(load "../closer-mop/closer-mop-packages")
(load "../closer-mop/closer-mop-shared")
(load "../closer-mop/closer-sbcl")

(load "../weyl/packages")
(load "../weyl/lisp-support")
(load "../weyl/domain-support")
   
(load "../weyl/classes/algebraic-domains")
(load "../weyl/classes/space-classes")
(load "../weyl/classes/general-classes")
 
(load "../weyl/avl")
(load "../weyl/lisp-numbers")
(load "../weyl/sets")
(load "../weyl/morphisms")
(load "../weyl/quotient-fields")
(load "../weyl/general" )
(load "../weyl/fourier")
(load "../weyl/functions")
(load "../weyl/direct-sums")


(load "../weyl/numbers/bigfloat")
(load "../weyl/numbers/numbers")
(load "../weyl/numbers/gfp")



(load "../weyl/polynomials/poly-tools")
(load "../weyl/polynomials/mpolynomial")
(load "../weyl/polynomials/upolynomial")
(load "../weyl/polynomials/epolynomial")
(load "../weyl/polynomials/sparsegcd")
(load "../weyl/polynomials/grobner")
     
(load "../weyl/tpower")
(load "../weyl/taylor")
(load "../weyl/rational-functions")
(load "../weyl/differential-domains")
(load "../weyl/algebraic-extension")
   
   
(load "../weyl/vector-spaces/vector")
(load "../weyl/vector-spaces/projective-space")
(load "../weyl/vector-spaces/quaternions")
     
(load "../weyl/matrix")
(load "../weyl/topology")
(load "../weyl/funct-spaces")
(load "../weyl/mesh")

;; (weyl/new) <-- Sat 1 Mar 22:09:56 CET 2025/KFP
(load "../weyl/new/weyl-infix")
(load "../weyl/new/ge-support")
(load "../weyl/new/latex")
(load "../weyl/new/display")
(load "../weyl/new/weyl-user-manual")
(load "../weyl/new/make-docs")
;(load "../weyl/new/ge-simplify")


;; some files have to be compiled to get full functionality.
(compile-file "../weyl/numbers/numbers.lisp")   ;; get-rational-...
(compile-file "../weyl/tpower.lisp")            ;; taylor

;; we also load MKs user-manual here.
;;(load "user-manual") replaced by new/weyl-user-manual
;; fixed a format bug ...


;(defmethod perform :after ((op load-op) (comp (eql (find-system "weyl"))))
;  "Initialize and reset the contexts."

(pushnew :weyl *features*)
(funcall (intern "INITIALIZE-CONTEXTS" :weyli))
(funcall (intern "RESET-DOMAINS" :weyli))
