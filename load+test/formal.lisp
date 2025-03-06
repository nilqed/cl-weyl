(ql:quickload :weyl)

(in-package :weyl)
(weyl::ge-vars '(i j k n a x))
(defvar lb1 (eqn= i 0))
(defvar ub1 n)
(defvar ai (add-subscripts a i))
(defvar body1 (* ai (expt x i)))


(defpackage formal-objects
  ;(:nicknames fobj :FORMAL-OBJ)
  (:use common-lisp)  
  ;(:import-from "WEYL") 
  (:export "latex")) 


(in-package :formal-objects)

(defclass sum ()
  ((lb   :initarg :lb    :initform nil :accessor sum-lb)
   (ub   :initarg :ub    :initform nil :accessor sum-ub)
   (body :initarg :body  :initform nil :accessor sum-body)))
   

(defmethod latex ((s sum) &key (pre "") (post ""))
  (let ((lb (sum-lb s))
        (ub (sum-ub s))
        (body (sum-body s)))
    (format nil "~A\\sum_{~A}^{~A} {~A} ~A" pre lb ub body post)))

;weyl::lb1
;i = 0
;(describe weyl::lb1)

(defvar sum1 (make-instance 'sum  :lb weyl::lb1 
                                  :ub weyl::ub1
                                  :body weyl::body1))
                                  
#|                            
* (in-package :formal-objects)
#<PACKAGE "FORMAL-OBJECTS">
* sum1
#<SUM {10030B4EF3}>
* (describe sum1)
#<SUM {10030B4EF3}>
  [standard-object]

Slots with :INSTANCE allocation:
  LB                             = i = 0
  UB                             = n
  BODY                           = x^i a(i)
*    

(describe weyl::lb1)
i = 0
  [standard-object]

Slots with :INSTANCE allocation:
  DOMAIN                         = #<Domain: GENERAL-EXPRESSIONS>
  SIMPLIFIED?                    = NIL
  LHS                            = i
  RHS                            = 0
*
(cl-user::Type-of  weyl::lb1)
WEYLI::GE-EQN=



* (in-package :formal-objectS)
#<PACKAGE "FORMAL-OBJECTS">
* sum1
#<SUM {10031B0713}>
* (latex sum1)
"\\sum_{i = 0}^{n} {x^i a(i)} "
* (latex sum1 :pre "$$" :post "$$")
"$$\\sum_{i = 0}^{n} {x^i a(i)} $$"
*

|#

   