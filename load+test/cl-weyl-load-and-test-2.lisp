;;; load weyl from sources (just a few seconds)
(load "load-weyl")
;(compile-file "../weyl/numbers/numbers.lisp")  ;; necessary to create numbers
; already done in load-weyl

; (sb-ext::disable-debugger)
(declaim (sb-ext:muffle-conditions style-warning))
(defun debug-ignore (c h) (declare (ignore h)) (print c) (abort))
(setf *debugger-hook* #'debug-ignore)


(in-package :weyl)

;;; quick exit
(defun bye () (cl-user::quit))  

(def-ge-vars p q r u v w x y z)

(defvar ge1 (+-> "p^(q+x)-(r/w)^y+u*v^2*w^z"))
