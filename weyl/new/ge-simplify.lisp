(ql:quickload :weyl)
(in-package :weyl)

(ge-vars '(x y z p q r a b c))
(defvar ge1 (+-> "(x+y)^3+2*p/(1-q^2)+a^r"))
(defvar ge2 (+ (expt (+ x y) 3) (* 2 p (expt (- 1 (expt q 2)) -1)) (expt a r)))



;* ge1
;(y + x)^3 + 2 (1 - q^2)^-1 p + a^r
;* (expand ge1)
;3 y x^2 + y^3 + 3 y^2 x + x^3 + a^r
; where is 2*p/(1-q^2) ????
;(ge-equal ge1 ge2)
; T
;* (ge-equal ge1 (expand ge2))
;NIL

; severe bug ********
;(expand (+-> "1/(1-q^2)"))
; 0
; 
;(expand (+-> "2*p/(1-q^2)"))
;0

; (wtype  (+-> "1/(1-q^2)"))
; WEYLI::GE-EXPT
; ==> (defmethod expand ((exp ge-expt)) l. 1545
; 
#|
(defmethod expand ((exp ge-expt))
  (let ((base (base-of exp))
	(exponent (exponent-of exp)))
    (cond ((and (ge-plus? base)
		(typep exponent 'rational-integer))
	   (simp-plus-terms (domain-of exp)
                            (expand-binomial-form (terms-of base)
                                                  (integer-value exponent))))
	  ((ge-times? base)
	   (expand (simp-times-terms (domain-of exp)
                                     (loop for term in (terms-of base)
                                           collect (expand (expt term exponent))))))
	  (t exp))))
      
|#

(defvar gx1 (+-> "1/(1-q^2)"))
(defvar egx1 (exponent-of gx1))
;-1
(defvar bgx1 (base-of gx1))
;1 - q^2

(ge-plus? bgx1)
;T

(typep egx1 'rational-integer)
;T

;(simp-plus-terms (domain-of exp)
;    (expand-binomial-form (terms-of base)
;        (integer-value exponent)))

(weyli::integer-value egx1)
;-1

(weyli::expand-binomial-form (terms-of bgx1) (weyli::integer-value egx1))
; NIL --- that's bad

(weyli::domain-of gx1)
;#<Domain: GENERAL-EXPRESSIONS>

(weyli::simp-plus-terms (weyli::domain-of gx1) nil)
; 0

;;; error :: (typep exponent 'rational-integer)) 
;;; exponent must be positive, better >=2 of course ....
