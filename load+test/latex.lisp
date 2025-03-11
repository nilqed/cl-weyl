(ql:quickload :weyl)
(in-package :weyl)

(defun wtype (obj) (cl::type-of obj))

#|
-------------
WEYLI CLASSES
-------------

RATIONAL-INTEGER
WEYLI::GE-ATOM
WEYLI::GE-NARY
WEYLI::GE-VARIABLE
WEYLI::GE-TIMES
WEYLI::GE-PLUS
WEYLI::GE-EXPT
WEYLI::GE-FUNCTION
WEYLI::GE-FUNCTION-DERIV
WEYLI::GE-APPLICATION
WEYLI::GE-EQUATION
WEYLI::GE-EQUATION=
WEYLI::GE-EQUATION>
WEYLI::GE-EQUATION>=
WEYLI::GE-FOURIER


WEYLI::GENERAL-EXPRESSIONS
WEYLI::GENERAL-EXPRESSION
WEYLI::ABSTRACT-FUNCTION
WEYLI::APPLICABLE-FUNCTION
WEYLI::UNIVERSAL-QUANTIFIED-SET


|#

(defgeneric display-latex (object &key &allow-other-keys)
  (:documentation "Return a LaTeX representation of object."))



(defmethod display-latex ((x weyli::ge-variable) &key (pre "") (post ""))
  (let* ((lr (get-variable-property *general* x 'latex-repr)))
    (if lr (format nil "~A{~A}~A" pre lr post)
      (format nil "~A{~A}~A" pre (string-of x) post))))
#|
* (weyl::ge-var p)
P
* (weyli::latex p)
"{p}"
* (weyli::latex p :pre "$" :post "$")
"${p}$"
*
* (weyli::set-variable-property *general* p 'latex-repr "//pi")
"//pi"
We have to define the latex method in :weyl and not in :weyli, otherwise
there is a problem regarding the property list.
|#

(defmethod display-latex ((x weyli::ge-expt) &key (pre "") (post ""))
  (let ((bas (base-of x)) (exp (exponent-of x)))
    (format nil "{~A{~A}^{~A}~A}" 
     pre 
      (case (wtype bas)
        (weyli::rational-integer (display-latex bas))
        (weyli::ge-variable      (display-latex bas))
        (weyli::ge-expt          (display-latex bas))
        (weyli::ge-application   (display-latex bas))
        (otherwise (display-latex bas :pre "(" :post ")")))
      (case (wtype exp)
        (weyli::rational-integer (display-latex exp))
        (weyli::ge-variable      (display-latex exp))
        (weyli::ge-expt          (display-latex exp))
        (weyli::ge-application   (display-latex exp))
        (otherwise (display-latex exp :pre "(" :post ")")))        
     post)))

(defmethod display-latex ((x weyli::ge-plus) &key (pre "") (post ""))
  (format nil "~A~{{~A}~^ + ~}~A" 
    pre (mapcar #'display-latex (terms-of x)) post))


;;;;;;
(ge-var p)
(weyli::set-variable-property *general* p 'latex-repr "\\pi")
(ge-var q)
(defvar l1 (display-latex (expt p q)))
(ge-var r)
(defvar l2 (display-latex (+ p q r)))
(defvar l3 (display-latex (+ p q r (expt p (+ q r)))))

(defun disp6 (obj &key (pt "11pt") (fg "Green") (bg "Black") 
             (res "150") (size "bbox") (off "-1.0cm,-2.0cm"))
"Display a object as rendered LaTeX code in a terminal that supports sixel
 graphics (e.g. xterm, mlterm and some others)."
  (progn 
    (cl-user::latex-to-sixel 
       (display-latex obj :pre "$$" :post "$$") 
           :fg "Blue" :bg "'rgb 1.0 1.0 1.0'" ) T)) 
  
 (disp6 (expt r (+ p q )))  
