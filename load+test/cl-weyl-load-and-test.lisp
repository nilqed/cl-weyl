
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

;;; Infix
;(load "../xref/infix")  
(load "../xref/weyl-infix")

;(defvar ix1 infix::'#I(p^^2+q^^2))
; (COMMON-LISP:+ (COMMON-LISP:EXPT INFIX::P 2) (COMMON-LISP:EXPT INFIX::Q 2))
; not yet what we expect ... remove package INFIX, rename to weyl-infix.lisp.

; (infix::string->prefix "p+q")
 ;(COMMON-LISP:+ P Q)


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

;;;;;;;
;;  * (in-package :weyl)
;;  #<PACKAGE "WEYL">
;;  * gex
;;  (+ (EXPT P 2) (EXPT Q 2) (- P))
;;  * dgex
;; -1 + 2 p
;; *
;;


 