;;; test-weyl-new.lisp -- Sat 1 Mar 22:17:55 CET 2025/KFP
;;; load weyl from sources with additions in  folder: weyl/new
(load "load-weyl")

;; (sb-ext::disable-debugger)
(declaim (sb-ext:muffle-conditions style-warning))
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)


(in-package :weyl)

;; quick exit
(defun bye () (cl-user::quit))

;; define some variables
(ge-vars '(p q r s u v))
(ge-vars '(x_0 x_1 x_2 x_3 _x4))

(defvar gvs (list-ge-vars))
(mapcar #'ge-variable? gvs) ;--> all T
(= (length gvs) 13)

;; type of ge-expressions
(defun wtype (obj) (cl::type-of obj))


;; test infix reader
(+-> "p+q")
(+-> "p*q^2")
(deriv (+-> "p*q^2*sin(x_1*x_2)") x_2 x_1)
(+-> "sin(q)^2+cos(q)^2")  
(deriv (+-> "sin(q)^2+cos(q)^2") q)  
(+-> "p*q-q*p") 

(defvar t1  (wtype p))                  ;;; WEYLI::GE-VARIABLE
(defvar t2  (wtype (+-> "p*q")))        ;;; WEYLI::GE-TIMES
(defvar t3  (wtype (+-> "p/q")))        ;;; WEYLI::GE-TIMES
(defvar t4  (wtype (+-> "p+q")))        ;;; WEYLI::GE-PLUS
(defvar t5  (wtype (+-> "p-q")))        ;;; WEYLI::GE-PLUS
(defvar t6  (wtype (+-> "p^q")))        ;;; WEYLI::GE-EXPT
(defvar t7  (wtype (+-> "-p")))         ;;; WEYLI::GE-TIMES
(defvar t8  (wtype (+-> "sin(p)")))     ;;; WEYLI::GE-APPLICATION
(defvar t9  (wtype (+-> "p^2+q*p")))    ;;; WEYLI::GE-PLUS
(defvar t10 (wtype (+-> "q-q")))        ;;; RATIONAL-INTEGER
(defvar t11 (wtype (+-> "q/q")))        ;;; RATIONAL-INTEGER








