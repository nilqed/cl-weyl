(ql:quickload :weyl)
(in-package :weyl)

;;;; More Simplification Tools
;;;; 
;;;; Weyl provides 'simplify' and 'expand' so far, however, there is room for
;;;; improvement. Certain terms in ge-plus still are unexpanded ge-times 
;;;; which ought be expanded in ge-plus terms. We need some normal form, and
;;;; a method 'normalize' to test equality as good as possible (ge-equal tests
;;;; only syntactical equivalence ??).
;;;;
;;;; Weyl predicates: 
;;;;   ge-plus?, ge-times?, ge-expt?, ge-variable?
;;;;   ge-application?, ge-function? 
;;;;   ge-eqn=?, ge-eqn>?, ge-eqn>=?
;;;;   number?, weyli::real?, minus?, 0?, 1? 
;;;;
;;;;
;;;; Weyl accessors:
;;;;   terms-of
;;;;   base-of, exponent-of 
;;;;   funct-of, args-of
;;;;   lhs-of, rhs-of
;;;;   weyli::integer-value
;;;;   domain-of  | (describe  'weyli::domain-element)
;;;;
;;;;
;;;; NEW: (print-terms obj), obj=GE-PLUS or GE-TIMES
;;;; Example:
;;;; (ge-vars '(x y z p q r a b c))
;;;; (defvar ge1 (+-> "(x+y)^3+2*p/(1-q^2)+a^r"))
;;;; (defvar ge2 (+ (expt (+ x y) 3) 
;;;;             (* 2 p (expt (- 1 (expt q 2)) -1)) (expt a r)))
;;;; Notice that ge1 = ge2, i.e. (ge-equal ge1 ge2) -> T 
;;;; 
;;;; *
;;;; (print-terms ge1)
;;;;
;;;; Terms of: GE-PLUS
;;;; =================================================================
;;;; #   Term                                          Type
;;;; -----------------------------------------------------------------
;;;; 1   (y + x)^3                                     GE-EXPT
;;;; 2   2 (1 - q^2)^-1 p                              GE-TIMES
;;;; 3   a^r                                           GE-EXPT
;;;; -----------------------------------------------------------------
;;;; NIL
;;;;
;;;; when we expand ...
;;;;
;;;; *
;;;; (print-terms (expand ge1))
;;;;
;;;; Terms of: GE-PLUS
;;;; =================================================================
;;;; #   Term                                          Type
;;;; -----------------------------------------------------------------
;;;; 1   3 y x^2                                       GE-TIMES
;;;; 2   2 (1 - q^2)^-1 p                              GE-TIMES
;;;; 3   y^3                                           GE-EXPT
;;;; 4   3 y^2 x                                       GE-TIMES
;;;; 5   x^3                                           GE-EXPT
;;;; 6   a^r                                           GE-EXPT
;;;; -----------------------------------------------------------------
;;;; NIL
;;;;
;;;; Now we define g12 which is obviouly ZERO!
;;;;
;;;; * (defvar g12 (simplify (- (expand ge1) ge2)))
;;;; G12
;;;; * (print-terms g12)
;;;;
;;;; Terms of: GE-PLUS
;;;; =================================================================
;;;; #   Term                                          Type
;;;; -----------------------------------------------------------------
;;;; 1   -1 ((y + x)^3 + 2 (1 - q^2)^-1 p + a^r)       GE-TIMES
;;;; 2   3 y x^2                                       GE-TIMES
;;;; 3   2 (1 - q^2)^-1 p                              GE-TIMES
;;;; 4   y^3                                           GE-EXPT
;;;; 5   3 y^2 x                                       GE-TIMES
;;;; 6   x^3                                           GE-EXPT
;;;; 7   a^r                                           GE-EXPT
;;;; -----------------------------------------------------------------
;;;; NIL
;;;;
;;;; The problem is that the first term is not expanded, that is 
;;;; 'simplify' does not check whether ther are terms in GE-PLUS
;;;; that should be expanded further like the GE-TIMES term #1.
;;;;
;;;; At least, another 'expand' will do the job:
;;;; (expand g12)
;;;; 0
;;;; *
;;;;
;;;; Other examples are
;;;;   (defvar xy  (+-> "(x^2-y^2)-(x-y)*(x+y)"))
;;;;   (defvar x/y (+-> "(x^2-y^2)/((x-y)*(x+y))"))
;;;; while expand eventually yield 0 in the former case, neither simplify nor
;;;; expand will do any further simplification in the latter.
;;;;
;;;; The reason can again be seen with print-terms:
;;;;
;;;; *
;;;; (print-terms x/y)
;;;;
;;;; Terms of: GE-TIMES
;;;; =================================================================
;;;; #   Term                                          Type
;;;; -----------------------------------------------------------------
;;;; 1   -1 y^2 + x^2                                  GE-PLUS
;;;; 2   ((-1 y + x) (y + x))^-1                       GE-EXPT
;;;; -----------------------------------------------------------------
;;;; NIL
;;;;
;;;; The second term (GE-EXPT) should be combined, so that #1 and #2 are
;;;; recognized as inverses of each other.
;;;;
;;;; NEW: 
;;;;   num-of-terms obj  
;;;;   get-term obj #n
;;;;
;;;;


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
  (if (not (or (ge-plus? ge) (ge-times? ge))) 
    (return-from print-terms (wtype ge)))
  (format t "~%Terms of: ~A~%" (wtype ge))
  (format t "~v@{~A~:*~}~%" ll "=")
  (format t "~3A ~45A ~A ~%" "#" "Term" "Type")
  (format t "~v@{~A~:*~}~%" ll "-")   
  (let ((terms (terms-of ge))
        (nr 0))
          (loop for s in terms do
             (format t "~3A ~45A ~A ~%" (incf nr) s (wtype s))))
  (format t "~v@{~A~:*~}~%" ll "-"))
 
 

(defmethod num-of-terms ((ge weyli::ge-plus))  
  (length (terms-of ge)))
  
(defmethod num-of-terms ((ge weyli::ge-times)) 
  (length (terms-of ge)))

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

; (expt 2 p) --> No way to raise 2 to the p power
(make-ge-expt *general* 2 p)  ; ok

;* (ge-expt? (expt 2 p))
;T


(defmethod normalize ((x weyli::ge-variable)) x)
(defmethod normalize ((x weyli::rational-integer)) x)

(defmethod normalize ((x weyli::ge-expt)) x)

;(defmethod normalize ((x weyli::ge-times))
;  (let ((tx (terms-of x)))
;    (loop for s in tx collect
;      (typecase s
;        (weyli::ge-plus   )
  
;(defmethod collect-expts-with-same-exp ((x weyli::ge-times))
;  (let ((el nil) (tx (terms-of x)))
;    (loop for s in tx do
;      (if (ge-expt? s)
;        (push s el)))
;          (if (null el) x 
;             (let ((a (car el)) (b (cdr el)))
          
; Example: (defvar xy (* (expt 2 p) (expt x q) (expt 3 p) (expt y q)))
; (pick-exps (terms-of ab)) --> ((q . y) (q . x) (p . 3) (p . 2))     
(defun pick-exps (x)
  "Input: x a list of ge-expt." 
  (loop for s in x collect (cons (exponent-of s) (base-of s))))
  
(defun combine-picks (x)
  "Input: x a list of conses (exp . base)."
  (if (null x) (return-from combine-picks))
  (let* ((a (pop x))
         (r (loop for s in x collect 
           (if (eql (car a) (car s)) 
             (progn (delete s x :test #'equal) s)))))
           (push a r)
           (nconc (list r) (combine-picks x))))

