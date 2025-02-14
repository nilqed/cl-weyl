

;;; This is just a scratchpad /\/\/\ 
;;; v Thu 13 Feb 22:39:29 CET 2025 



;;; load weyl from sources (just a few seconds)
(load "load-weyl")

; (sb-ext::disable-debugger)
(declaim (sb-ext:muffle-conditions style-warning))
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)


(in-package :weyl)

;;; quick exit
(defun bye () (cl-user::quit))  

;;; defines one general variable
(defmacro ge-var (v) 
  `(defvar ,v (coerce ',v *general*)))
  
(macroexpand '(ge-var r))

(defun eval-str (s)
  (eval (read-from-string s)))

;;; defines general variables from a list 
(defun ge-vars (vl)
  (loop for i in vl
    do (eval-str (format nil "(ge-var ~a)" i))))

(ge-vars '(x0 x1 x2 x3))   ;;; some variables
(ge-vars '(x y z p q r))

(defvar ge1 (deriv (expt p q) q))
(defvar ge2 (* x1 (expt x2 2) (expt x3 3) (sin x1)))

#|
;;; Infix
;(load "../infix/infix")  
(load "../infix/weyl-infix")

;(defvar ix1 infix::'#I(p^^2+q^^2))
; (COMMON-LISP:+ (COMMON-LISP:EXPT INFIX::P 2) (COMMON-LISP:EXPT INFIX::Q 2))
; not yet what we expect ... remove package INFIX, rename to weyl-infix.lisp.

; (infix::string->prefix "p+q")
 ;(COMMON-LISP:+ P Q)


(defvar dgex (deriv (eval '#I(p^2 + q^2 - p))  p))
;* dgex
;-1 + 2 p
;*

(defvar gex '#I(p^2 + q^2 - p))
;GEX
;*

(cl-user::type-of  gex)
;CONS
;*

(cl-user::type-of  (eval gex))
;WEYLI::GE-PLUS
;*

;;;;;;; after load:
;;  * (in-package :weyl)
;;  #<PACKAGE "WEYL">
;;  * gex
;;  (+ (EXPT P 2) (EXPT Q 2) (- P))
;;  * dgex
;; -1 + 2 p
;; *
;;


'(eval #I(p^2 + q^2 - p^sin(x1) ))
;; (EVAL (+ (EXPT P 2) (EXPT Q 2) (- (EXPT P (SIN X1)))))

(eval #I(p^2 + q^2 - p^sin(x1) ))
;;q^2 - p^(sin(x1)) + p^2

(deriv (eval #I(p^2 + q^2 - p^sin(x1))) x1)
;; -1 (log(p)) p^(sin(x1)) (cos(x1))

(deriv (eval #I(p^2 + q^2 - p^sin(x1))) x1 p)
;; p^(-1 + sin(x1)) (cos(x1)) - ((sin(x1)) p^(-1 + sin(x1)) (log(p)) (cos(x1)))

(deriv (eval #I(p^2 + q^2 - p^sin(x1))) q )
;; 2 q

|#


#|
 
(load "../infix/infix-weyl")

(defvar dgex (deriv (eval '#I(p^^2 + q^^2 - p))  p))
;* dgex
;-1 + 2 p
;*

(defvar gex '#I(p^^2 + q^^2 - p))
;GEX
;*

(cl-user::type-of  gex)
;CONS
;*

(cl-user::type-of  (eval gex))
;WEYLI::GE-PLUS
;*

;;;;;;; after load:
;;  * (in-package :weyl)
;;  #<PACKAGE "WEYL">
;;  * gex
;;  (+ (EXPT P 2) (EXPT Q 2) (- P))
;;  * dgex
;; -1 + 2 p
;; *
;;


'(eval #I(p^^2 + q^^2 - p^^sin(x1) ))
;; (EVAL (+ (EXPT P 2) (EXPT Q 2) (- (EXPT P (SIN X1)))))

(eval #I(p^^2 + q^^2 - p^^sin(x1) ))
;;q^2 - p^(sin(x1)) + p^2

(deriv (eval #I(p^^2 + q^^2 - p^^sin(x1))) x1)
;; -1 (log(p)) p^(sin(x1)) (cos(x1))

(deriv (eval #I(p^^2 + q^^2 - p^^sin(x1))) x1 p)
;; p^(-1 + sin(x1)) (cos(x1)) - ((sin(x1)) p^(-1 + sin(x1)) (log(p)) (cos(x1)))

(deriv (eval #I(p^^2 + q^^2 - p^^sin(x1))) q )
;; 2 q

(ge-vars '(a b c d e f g))

|#


(load "../infix/weyl-infix")
(infix:test-infix)

; '#I(p+q^2^w)
; (COMMON-LISP:+ P (COMMON-LISP:EXPT Q (COMMON-LISP:EXPT 2 W)))
; * '#I(p+q)
; (COMMON-LISP:+ P Q)
; * (eval '#I(p+q))

;; how to get rid of the COMMON-LISP: tags ???
;; dirty but useful :-)
(defvar ix1  '#I(p+q))
(eval (read-from-string  (format nil "~A" ix1)))

(defun ifx (str)
  (eval (read-from-string (format nil "~A" (infix:string->prefix str)))))

(ifx "p+q")
(ifx "p*q^2")
(deriv (ifx "p*q^2*sin(x1*x2)") x2 x1)

(ifx "sin(q)^2+cos(q)^2")  ;; unable to =1 
(deriv (ifx "sin(q)^2+cos(q)^2") q)  ;; unable =0, even expand does not help!

(ifx "p*q-q*p") ; =0 at least 



(defun wtype (obj) (cl::type-of obj))

(defun slot-names (cls)
  (mapcar #'sb-mop::slot-definition-name 
    (sb-mop:class-slots (sb-mop::find-class cls ))))

(defun slot-iargs (cls)
  (mapcar #'sb-mop::slot-definition-initargs 
    (sb-mop:class-slots (sb-mop::find-class cls))))
    
(defun slot-info (obj &key (prt t))
  (let* ((tobj (cl-user::type-of obj))
         (sn (slot-names tobj))
         (sv (map 'list (lambda (x) (slot-value obj x)) sn))
         (sa (slot-iargs tobj)))
            (format prt "Obj:Type : ~a : ~a ~%" obj tobj)
            (format prt "Names ...: ~{~a~^, ~} ~%" sn)
            (format prt "Values ..: ~{~a~^, ~} ~%" sv)  
            (format prt "InitArgs : ~{~a~^, ~} ~%~%" sa)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar t1  (wtype p))                  ;;; WEYLI::GE-VARIABLE
(defvar t2  (wtype (ifx "p*q")))        ;;; WEYLI::GE-TIMES
(defvar t3  (wtype (ifx "p/q")))        ;;; WEYLI::GE-TIMES 
(defvar t4  (wtype (ifx "p+q")))        ;;; WEYLI::GE-PLUS
(defvar t5  (wtype (ifx "p-q")))        ;;; WEYLI::GE-PLUS
(defvar t6  (wtype (ifx "p^q")))        ;;; WEYLI::GE-EXPT
(defvar t7  (wtype (ifx "-p")))         ;;; WEYLI::GE-TIMES 
(defvar t8  (wtype (ifx "sin(p)")))     ;;; WEYLI::GE-APPLICATION 
(defvar t9  (wtype (ifx "p^2+q*p")))    ;;; WEYLI::GE-PLUS
(defvar t10 (wtype (ifx "q-q")))        ;;; RATIONAL-INTEGER
(defvar t11 (wtype (ifx "q/q")))        ;;; RATIONAL-INTEGER

(slot-info p)
; Obj:Type : p : GE-VARIABLE
; Names ...: PROPERTY-LIST, DOMAIN, SIMPLIFIED?, SYMBOL, STRING
; Values ..: NIL, #<Domain: GENERAL-EXPRESSIONS>, NIL, P, p
; InitArgs : NIL, (DOMAIN), NIL, (SYMBOL), (STRING)

(slot-info (ifx "p*q"))
; Obj:Type : q p : GE-TIMES
; Names ...: DOMAIN, SIMPLIFIED?, TERMS
; Values ..: #<Domain: GENERAL-EXPRESSIONS>, NIL, (q p)
; InitArgs : (DOMAIN), NIL, (TERMS)

(slot-info (ifx "p^q"))
; Obj:Type : p^q : GE-EXPT
; Names ...: DOMAIN, SIMPLIFIED?, BASE, EXP
; Values ..: #<Domain: GENERAL-EXPRESSIONS>, NIL, p, q
; InitArgs : (DOMAIN), NIL, (BASE), (EXP)

(slot-info (ifx "sin(p)"))
; Obj:Type : sin(p) : GE-APPLICATION
; Names ...: DOMAIN, SIMPLIFIED?, FUNCT, ARGS
; Values ..: #<Domain: GENERAL-EXPRESSIONS>, NIL, sin, (p)
; InitArgs : (DOMAIN), NIL, (FUNCT), (ARGS)


(slot-value (ifx "sin(p)") 'funct)         ;;; sin  (in weyl, also in weyli)
(slot-value (ifx "sin(p)") 'weyli::domain) ;;; tag is necessary
(slot-value (ifx "sin(p)") 'weyli::args)   ;;; (q) , dito!

(defun ltx (x)
  (case (wtype x) 
    ('rational-integer       (ltx-ratint x))
    ('weyli::ge-variable     (ltx-var x))
    ('weyli::ge-expt         (ltx-expt x))
    ('weyli::ge-plus         (ltx-plus x))
    ('weyli::ge-times        (ltx-times x))
    ('weyli::ge-application  (ltx-app x))
    (otherwise "END"))) 


(defun ltx-ratint (x)
  (format nil "{~A}" (slot-value x 'weyli::value)))  

(defun ltx-var (x) ;;TODO: subscripts
  (format nil "{~A}" (slot-value x 'weyli::string)))
  
(defun ltx-expt (x)
  (format nil "{{~A}^{~A}}" 
    (ltx (slot-value x 'weyli::base))
    (ltx (slot-value x 'weyli::exp))))
    
(defun ltx-plus (x)
  (format nil "~{{~a}~^ + ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))

(defun ltx-times (x)
  (format nil "~{{~a}~^ \\, ~}" (mapcar 'ltx (slot-value x 'weyli::terms))))
  
(defun ltx-app (x)
  (format nil "~a( ~{{~a}~^ , ~})" 
    (slot-value x 'weyli::funct)
    (mapcar 'ltx (slot-value x 'weyli::args))))
  
(defun test-ltx ()
  (list (ltx (+ p q))
        (ltx (* p q))
        (ltx (* p (expt q 2) ))
        (ltx (* p (expt q 2) (sin x1) ))))






