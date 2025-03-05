(ql:quickload :weyl)

(in-package :weyl)
;#<PACKAGE "WEYL">

(ge-vars '(p q r x y z  u v w x_0 x_1 x_2 x_3 x_4))
;NIL

(list-ge-vars)
;(x_4 x_3 x_2 x_1 x_0 w v u z y x r q p v.1 x)

(defvar t0  (coerce 't *general*))  ;; cannot use ge-var / t special of course

(defvar ge1 (+-> "p*q^t0-sin(cos(p+u)*r)"))
;GE1

ge1
; q^t p - (sin((cos(u + p)) r))

; (display6 ge1) 

(deriv ge1 t)
; (log(q)) q^t p

;(display6 (deriv ge1 t))

(deriv ge1 p)
; q^t + (sin(u + p)) r (cos((cos(u + p)) r))


(deriv ge1 p r u)
; (cos(u + p)) (cos((cos(u + p)) r)) + (sin(u + p))^2 r (sin((cos(u + p)) r))
; + (sin(u + p))^2 (sin((cos(u + p)) r)) r + (sin(u + p))^2 r^2 
; (cos((cos(u + p)) r)) (cos(u + p)) - ((cos(u + p))^2 (sin((cos(u + p)) r)) r)


(latex (deriv ge1 p r u))
;"$${{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {\\operatorname{cos}( 
;{{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {{r}}})}} + 
;{{{{\\operatorname{sin}( {{{u}} + {{p}}})}^{{2}}}} \\, {{r}} \\, 
;{\\operatorname{sin}( {{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {{r}}})}} + 
;{{{{\\operatorname{sin}( {{{u}} + {{p}}})}^{{2}}}} \\, {\\operatorname{sin}( 
;{{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {{r}}})} \\, {{r}}} + 
;{{{{\\operatorname{sin}( {{{u}} + {{p}}})}^{{2}}}} \\, {{{{r}}^{{2}}}} \\,
;{\\operatorname{cos}( {{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {{r}}})} 
;\\, {\\operatorname{cos}( {{{u}} + {{p}}})}} + {{{-1}} \\, 
;{{{\\operatorname{cos}( {{{u}} + {{p}}})}^{{2}}}} \\, {\\operatorname{sin}( 
;{{\\operatorname{cos}( {{{u}} + {{p}}})} \\, {{r}}})} \\, {{r}}}$$"


(describe p)
;p
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  PROPERTY-LIST                  = NIL
;  DOMAIN                         = #<Domain: GENERAL-EXPRESSIONS>
;  SIMPLIFIED?                    = NIL
;  SYMBOL                         = P
;  STRING                         = "p"


(describe ge1)
;q^t #1=p - (sin((cos(u + #1#)) r))
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  DOMAIN                         = #<Domain: GENERAL-EXPRESSIONS>
;  SIMPLIFIED?                    = NIL
;  TERMS                          = (q^t #1=p -1 (sin((cos(u + #1#)) r)))


(latex p)
;"$${p}$$"

(weyli::set-variable-property *general* p  'latex-repr "\\pi" )
;"\\pi"

(get-variable-property *general* p  'latex-repr )
;"\\pi"

(latex p)
;"$${\\pi}$$"

;(display6 p) ; ok


(defvar pp (add-subscripts p q r))
;PP
(print (getf pp :subscripts))
; (q r)


; (display6 pp)
(latex  pp)
; "$${p(q,r)}$$"  ;; todo

(weyli::ge-variables *general*)
;(p(q,r) t tt x_4 x_3 x_2 x_1 x_0 w v u z y x r q p v.1 x)




(substitute p q (* p q))
;p^2

(substitute p q (+ p q))
; 2 p

(substitute 4 q (+ p q))
;4 + p

(substitute x_1  q (+ p (sin (cos q)) ))
;p + sin(cos(x_1))

;;; make-app-function (todo: wrong in manual: make-applicable-function)
(defvar f1 (weyli::make-app-function '(u v) (+ (* 'u 'v) (* 'u 'u 'u))))
;F1

f1 
;(lambda (v.1 v.2) v.1^3 + v.2 v.1)

;; NOTICE!
(deriv f1 0) ; --> (lambda (v.1 v.2) v.2 + 3 v.1^2)
;(lambda (v.1 v.2) v.2 + 3 v.1^2)

(deriv f1 1)
;(lambda (v.1 v.2) v.1)

(cl-user::type-of f1)
;WEYLI::APPLICABLE-FUNCTION


(apply f1 '(p q)) 
;p(q,r)^3 + q p(q,r)

(apply (deriv f1 0) '(p q)) 
; q + 3 p(q,r)^2


(documentation 'weyli::make-ge-variable 'function)
;"Create a variable in a domain."

(documentation 'weyli::coerce 'function)
;"Coerce the element into the domain."

(documentation 'weyli::expand 'function)
;"Replaces all products of sums in exp by sums of products."


 ;(cl-user::quit)
 
 ;;; Wed 5 Mar 22:37:25 CET 2025



