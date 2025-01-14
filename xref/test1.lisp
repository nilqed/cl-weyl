(require :sb-introspect)
(ql::quickload :weyl)

(declaim (sb-ext:muffle-conditions cl:warning))

(in-package :weyli)
(defun bye () (cl-user::quit))

(reset-domains)
(setf R *general*)

(setf u (coerce 'u R))
(setf v (coerce 'v R))
(setf w (coerce 'w R))

(ge-variable? v)
(add-subscripts u '(1 2 'a))
(get-variable-property R u 'key)

(declare-dependencies u v w)
(depends-on? u v)

(setf x (+ 2 (* u v)))
(different-kernels x '(v))
(different-kernels x (list v)) ;;; recognize the diff

(setf xx (simplify (- x x)))
(simplify xx)  ;; lol
(expand xx) ;; ok

(deriv x 'v)
(deriv (* x x) 'v)
(expand (deriv (* x x) 'v))

(expand xx) ;; -> 0 but not simplify ;)

#|
(sb-introspect::who-calls  'weyli:ge-variable? )
(sb-introspect::who-calls  'weyli:expand )
(sb-introspect::who-calls  'weyli:simplify )

 (sb-introspect::function-lambda-list  'weyli:deriv)
  (sb-introspect::function-type   'weyli:deriv)
  (sb-introspect::function-type   'weyli:expand)
  
  (sb-introspect::find-function-callers  'weyli:expand)

|#

(let ((count 0)) (permute '(a b c d) (p) (print p) (incf count))
    (format t "~%~D permutations total. ~%" count))

(weyli:partition (l 10) (print l))












