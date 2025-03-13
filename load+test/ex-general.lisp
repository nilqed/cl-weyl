(ql:quickload :weyl)
(in-package :weyl)

;;;;
;;;; General Expressions
;;;;

; Most sophisticated computations using Weyl take place within one of the more 
; specialized domains discussed in the later chapters. Generally, these domains 
; deal with algebraic structures like groups, rings and elds. Unfortunately, 
; none of these structures can deal with all types of mathematical objects. For 
; instance, while vectors can be elements of vector spaces, there is no 
; algebraic domain that contains both vectors and polynomial. Similarly, there 
; is no algebraic domain that can deal with special functions, summations, 
; products etc. What is required is a domain that can represent mathematical 
; objects syntactically. This is dealt with in Weyl by the general-expression 
; domain. General expressions are intended to be exible enough to represent any 
; mathematical expression that one might write on a piece of paper. On paper 
; one can write the two distinct expressions a(b + c) and ab + ac, even though 
; as polynomials they are equivalent. Within the polynomial domain discussed in 
; Chapter 9, these two expressions are indistinguishable. However, as general 
; expressions they are distinct objects that can be di erentiated. While, 
; general expressions are extremely exible, they are not necessarily very 
; efficient. As a consequence, for large scale computations it is usually 
; preferable to use one of the special purpose domains described in the later 
; sections. While some computations are purely algebraic and need never 
; reference general expressions, most engineering and scienti c computations 
; need a place to store information about the dimensions of variables, defining
; relationships and so on. These purposes are also served well by the 
; general expression structures.


;;;
;;; Variables (6.2)
;;;

;; Originally, general variables in Weyl are declared by using 'coerce' or
;; the method 'make-ge-variable' (general.lisp).

(defvar x (coerce 'x *general*))
;=> X

(defvar y (make-ge-variable *general* 'y))
;=> Y

;; We may also use the new macro 'ge-var' defined in ge-support.lisp:
(ge-var z)
;=> Z

;; or the function 'ge-vars' applied to a list (iterates ge-var):
(ge-vars '(p q r)) 
;=> NIL

;;
;; Check if symbol is a ge-variable:
;;
(ge-variable? x)
;=> T

(ge-variable? r)
;=> T

; unfortunately quoting is useless and not quoting results in an error if 
; it is not defined at all (fix?)
(ge-variable? 'r)
;=> NIL

;;
;; Listing all variables currently active in domain *general*
;; 
(weyli::ge-variables *general*)
;=> (r q p z y x v.1 x)
 
; there is a function 'list-ge-vars' in ge-support.lisp which does the same:
(list-ge-vars)
;=> (r q p z y x v.1 x)

; the variables v.1 and x are always present, but this x is not ge-equal to
; a later defined x (hmm, strange ... todo)...

;;
;; Adding subscripts to a variable (yields a new variable!)
;;
(ge-vars '(u v w))
;=> NIL

(defvar u12 (add-subscripts u 1 2))
;=> U12

u12
;=> u(1,2)

(defvar u_vw (add-subscripts u v w))
;=> U_VW

u_vw
;=> u(v,w)

(defvar u_ab (add-subscripts u 'a 'b))
;=> U_AB

; note the difference between using variables or symbols as indices. 
u_ab
;=> u(A,B)

(list-ge-vars)
; => (u(A,B) u(v,w) u(1,2) w v u r q p z y x v.1 x)


(describe u_vw)
;u(v,w)
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  PROPERTY-LIST                  = (:SUBSCRIPTS (v w))
;  DOMAIN                         = #<Domain: GENERAL-EXPRESSIONS>
;  SIMPLIFIED?                    = NIL
;  SYMBOL                         = U
;  STRING                         = "u(v,w)"

(describe u_ab)
;u(A,B)
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  PROPERTY-LIST                  = (:SUBSCRIPTS (A B))
;  DOMAIN                         = #<Domain: GENERAL-EXPRESSIONS>
;  SIMPLIFIED?                    = NIL
;  SYMBOL                         = U
;  STRING                         = "u(A,B)"

(documentation 'add-subscripts 'function)
;"Creates a new variable, which has the subscripts indicated. If the
; variable already has subscripts, then the new subscripts are appended to
; the ones already present."

;;
;; Variable properties
;;

; All variables have property lists which can be used to store important 
; information about them. This information is not attached to the variables, 
; but to the domain from which the variables arise.

; get-variable-property domain var key                            [Function]
; Returns the key property of var. A return value of nil indicates that the 
; was no key property associated with var . New properties may be established 
; by using setf on this form.

(get-variable-property *general* u12 :subscripts)
;=> NIL

; This shows that the propety-list displayed by describe is not the one
; where get-variable-property is looking for. As explained above, the infor
; is fetched from the domain property list.

(setf (get-variable-property *general* u12 'mykey) 999)
;=> 999

(get-variable-property *general* u12 'mykey)
;=> 999

; A (describe u12) shows that 'mykey is not stored in the instance.


;;
;; Dependencies
;;

; Dependencies of one variable on another can be declared using 
; declare-dependencies:

; declare-dependencies kernel &rest vars [Function]
;
; This indicates that kernel depends upon each of the variables in vars.

(declare-dependencies z x y)
;=> (y x)



; depends-on? exp &rest vars                                         [Function]
;
; This predicate can be applied to any expression, not just to variables. It 
; returns t if the exp depends on all of the variables in vars, otherwise it 
; returns nil. The expression can also be a list, in which case nil is returned
; only if every element of exp is free of vars.

(depends-on?  z x)
;=> T

(depends-on?  z x y)
;=> T

(depends-on?  z u)
;=> NIL

; But notice that it is enough when one variable matches! This is 
; quite unfortunate (fix?)
(depends-on?  z u x)
;=> T

; Apparently we also can get the dependencies from the domain property list:
; (not documented in manual).
(get-variable-property *general* z :dependencies)
;=>(y x)


; different-kernels exp list-of-kernels                             [Function]
; Returns a list of the kernels in exp that are different from those in 
; list-of-kernels .

;(different-kernels (* z x y) '(x y))
;=> (x y z X Y)

; not yet clear to me what's for ...

;;;
;;; Operators (6.4)
;;;
(ge-vars '(a b c))
;=> NIL

;a+b+c
(+ a b c)
;=> c + b + a

;a+b
(+ a b)
;=> b + a

; -a
(- a)
;=> -1 a

; a*b
(* a b)
;=> b a

; a/b
(/ a b)
;=> b^-1 a

; a^b
(expt a b)
;=> a^b

; According to the manual:
; In addition the standard special functions are supported log sin cos tan cot 
; sec csc sinh cosh tanh coth sech csch

; Actually we have:
weyli::*global-functions*
;=>(acosh asinh tanh cosh sinh acos asin tan cos sin log imagpart realpart abs)

; that is cot, sec, csc, coth, sech, csch are not defined.

(log p)
;=> log(p)
(sin p)
;=> sin(p)
(cos p)
;=> cos(p)
(tan p)
;=> tan(p)

;(cot p)
;(sec p)
;(csc p)

(sinh p)
;=> sinh(p)
(cosh p)
;=> cosh(p)
(tanh p)
;=>tanh(p)

;(coth p)
;(sech p)
;(csch p)

(deriv (tanh p) p)
;=> (cosh(p))^-2

;;;
;;; Tools for General Expressions (6.5)
;;;

; Weyl provides a number of programming tools that make writing symbolic 
; programs simpler. This section discusses some of these tools.


;;
;; Display tools (6.5.1)
;;

; print-object exp stream                                           [Function]
;
; This method is provided for all CLOS instances. It is used whenever an 
; object is printed using princ or a related function. In Weyl, a print-object
; method is provided for classes of objects to make the objects more readable 
; when debugging or when doing simple computations. The printed form produced 
; by print-object cannot be read to produce the object again (as can be done 
; with lists and some other Lisp expressions. 

(print-object (expt p q) nil)
;=> p^q
;=> "q"

;display expr &optional (stream *standard-output*)                  [Function]
;
; Prints the expression expr onto stream. If stream a graphics stream then a 
; two dimension display will be used (not yet implemented), otherwise some 
; textual display will be used.

(display (expt p q) nil)
;=> p^q
;=> p^q

(display (expt p q) t)
;=> p^qp^q

;;
;; Simplification Tools (6.5.2)
;;

; simplify expr                                                     [Function]
;
; Performs simple simpli cations of expr, 0 + x ! x and so on.




