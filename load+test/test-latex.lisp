(ql:quickload :weyl)
;(load "load-weyl")

(in-package :weyl)

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
;;;;;;
(ge-var p)
(weyli::set-variable-property *general* p 'latex-repr "\\pi")
(ge-var q)
(defvar l1 (latex (expt p q)))
(ge-var r)
(defvar l2 (latex (+ p q r)))
(defvar l3 (latex (+ p q r (expt p (+ q r)))))


  
 (display6 (expt r (+ p q )))  
 (display6 (expt (+ q r) (+ p q)))
 (display6 (expt (+ q r 2) (+ 33 p q)))
 (display6 (+-> "p+q^(2+r)"))
 (display6 (+-> "(p+r*q)^(p+q^(2+r))")) 
 (display6 (sin (* p p q 2)))
 (display6 (+-> "cos(sin(p)/q+cos(r^p))"))
 
(defvar eq1 (make-instance 'weyli::ge-eqn= :lhs (+ p q) :rhs (expt p q)))
(display6 eq1)

(defvar eq2 (make-instance 'weyli::ge-eqn> :lhs (+ p q) :rhs (expt p q)))
(display6 eq2)

(defvar eq3 (make-instance 'weyli::ge-eqn>= :lhs (+ p q) :rhs (expt p q)))
(display6 eq3)

