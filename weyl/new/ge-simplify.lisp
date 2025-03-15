(ql:quickload :weyl)
(in-package :weyl)

(ge-vars '(x y z p q r a b c))
(defvar ge1 (+-> "(x+y)^3+2*p/(1-q^2)+a^r"))
(defvar ge2 (+ (expt (+ x y) 3) (* 2 p (expt (- 1 (expt q 2)) -1)) (expt a r)))



;* ge1
;(y + x)^3 + 2 (1 - q^2)^-1 p + a^r
;* (expand ge1)
;3 y x^2 + y^3 + 3 y^2 x + x^3 + a^r

(print (ge-equal ge1 ge2))

(defvar g12 (- (expand ge1) ge2))
(defvar sg12 (simplify g12))

(defvar tg12 (terms-of g12))

;(loop for s in tg12 do
;  (format t "term: ~45A   type: ~A ~%" s (wtype s)))
  
(defun print-terms (ge &key (ll 65))
  "Print a table of the terms of the instance."
  (format t "~%Terms of: ~A~%" (wtype ge))
  (format t "~v@{~A~:*~}~%" ll "=")
  (format t "~3A ~45A ~A ~%" "#" "Term" "Type")
  (format t "~v@{~A~:*~}~%" ll "-")   
  (let ((terms (terms-of ge))
        (nr 0))
          (loop for s in terms do
             (format t "~3A ~45A ~A ~%" (incf nr) s (wtype s))))
  (format t "~v@{~A~:*~}~%" ll "-"))
 
 
(defun ge-plus-or-minus? (obj)
  (let ((wt (wtype obj)))
    (or (eql wt 'weyli::ge-plus) (eql wt 'weyli::ge-times))))
 
; doesn't work in SBCL 
;(defmethod num-of-terms ((ge (satisfies ge-plus-or-minus?)))
;  (length (terms-of ge)))
  
(defmethod num-of-terms ((ge weyli::ge-plus)) (length (terms-of ge)))
(defmethod num-of-terms ((ge weyli::ge-times)) (length (terms-of ge)))

(defmethod get-term ((ge weyli::ge-plus) k)
  (if (<= k (num-of-terms ge)) (nth (- k 1) (terms-of ge)) nil))
  
(defmethod get-term ((ge weyli::ge-times) k)
  (if (<= k (num-of-terms ge)) (nth (- k 1) (terms-of ge)) nil))

;(WTYPE (get-term g12 1))
;WEYLI::GE-TIMES
;*
;(expand (get-term g12 1)) -> DOES NOT EXPAND -1*(...)

;(print-terms (get-term g12 1))

;Terms of: GE-TIMES
;=================================================================
;#   Term                                          Type
;-----------------------------------------------------------------
;1   -1                                            RATIONAL-INTEGER
;2   (y + x)^3 + 2 (1 - q^2)^-1 p + a^r            GE-PLUS
;-----------------------------------------------------------------
;NIL
;*

(weyli::expand-product (get-term g12 1))
; (-1 (y + x)^3 -2 (1 - q^2)^-1 p -1 a^r)
; ok

(defun simp (expr)
  (simplify (expand expr)))

