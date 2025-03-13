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
; Performs simple simplifications of expr, 0 + x ! x and so on.

; +-> "(p+q)*(p-q)"
(defvar ge1 (* (+ p q) (- p q)))
;=> GE1

; +-> "p^2 - q^2"
(defvar ge2 (- (expt p 2) (expt q 2)))
;=> GE2

ge1
;=> (-1 q + p) (q + p)

ge2
;=> -1 q^2 + p^2

(simplify (- ge1 ge2))
;=> (-1 q + p) (q + p) - (-1 q^2 + p^2)

; We see that the simplifier is far from perfect ;-) and needs some
; attention.

(simplify (- 1 (/ ge1 ge2)))
; 1 - ((-1 q + p) (-1 q^2 + p^2)^-1 (q + p))

;even ...
(simplify (- (expand ge1) ge2))
;=> -1 q^2 + p^2 - (-1 q^2 + p^2)

;(ge-equal (simplify (- ge1 ge2)) 0)
;=> NIL :-((

;eventually ...
 (expand (- (expand ge1) ge2))
 ;=> 0

;
; expand exp                                                        [Function]
;
; Replaces all products of sums in exp by sums of products.

(expand (* (+ a b c) (- p q) (+ r (expt v u))))
;=> -1 r q c - (r q b) - (r q a) + r p c + r p b + r p a - (v^u q c) - 
;   (v^u q b) - (v^u q a) + v^u p c + v^u p b + v^u p a

;
; ge-equal x y                                                      [Function]
;
; Returns T if x and y are syntactically identical general expressions.

(ge-equal (/ x y) (* x (expt  y -1)))
; => T

; one must be careful here about the meaning of "syntactically"...

;
; ge-great x y                                                      [Function]
;
; To speed up operations like simplification of expressions, an order is 
; placed on all expressions in the general representation. This ordering is 
; provided by the function ge-great.

; The function WEYL::GE-GREAT is undefined.
; we have to use weyli::

(weyli::ge-great q p)
;=> 0

;
; def-ge-operator operator &rest keyword-expr-pairs                 [Function]
;
; When a new operator is introduced this de ner should be used. It allows one 
; to define the simplifier, display and equality tester functions.

; ???? 

;
; deriv exp var &rest vars                                          [Function]
;
; Computes the derivative of exp with respect to var and simplifies the results.
; This is done for var and each element of var . Thus the second derivative 
; of exp with respect to T could be computed by: (deriv exp 't 't).

(defvar sc (+ (expt (sin p) 2) (expt (cos p) 2)))
;=> SC

(deriv sc p)
;=>2 (sin(p)) (cos(p)) - (2 (cos(p)) (sin(p)))

(simplify (deriv sc p))
;=> 2 (sin(p)) (cos(p)) - (2 (cos(p)) (sin(p)))
; well, advertising 'simplifying' is quite exaggerated ;)


(expand (deriv sc p ))
;=> -2 (cos(p)) (sin(p)) + 2 (sin(p)) (cos(p))

(deriv (tanh p) p p p)
;=> -2 (cosh(p))^-2 + 6 (cosh(p))^-4 (sinh(p))^2

(deriv (expt q (tan p)) p)
;=> -1 (log(q)) q^(tan(p)) (cos(p))^-2

(deriv (log (cos p))  p)
;=> -1 (sin(p)) (cos(p))^-1

(deriv (expt (log (sin (* (cos p) p))) p) p)
;=> (log(log(sin((cos(p)) p)))) (log(sin((cos(p)) p)))^p + (-1 (sin(p)) p + 
;    cos(p)) p (log(sin((cos(p)) p)))^(-1 + p) (cos((cos(p)) p)) 
;    (sin((cos(p)) p))^-1

;;
;; Functions (6.6)
;;
; There are three different types of functions as illustrated in Figure 6.3, 
; sampled-functions, ge-functions and applicable-functions. Each of these 
; represent functions about which different aspects are known. To represent 
; *well-known* functions, like sine, cosine and f (in the expressions f (x)),
; we use instances of the class ge-function. We are given only these functions
; names. Sometimes we know more about the functions, like their derivative or
; their expansion as a power series. This additional information is placed on 
; their property lists.

; applicable-functions are functions for which we have a program for computing
; their values. They are essentially lambda-expressions. Finally, 
; sampled-functions are functions about which we know only their graph. That 
; is, we are given the values of the function at certain points and mechanisms
; for interpolating those values. Each of these types of functions are 
; described in more detail in the following sections.
; Each class that inherits from abstract-function includes a slot that 
; indicates the number of arguments an instance of this class (i.e., functions)
; accepts. This information can be accessed using the method nargs-of.

;
; nargs-of abstract-function                                        [Function]
;
; Returns the number of arguments accepted by abstract-function.


; Each of these types of functions can be applied to arguments to get the value 
; of the function at that point. This can be done by either of the following 
; two functions, which are extensions of the usual Lisp ones (and continue to 
; work with Lisp arguments).

;
;funcall fun arg1 arg2 : : : argn                                   [Function]
;
; Apply fun to the specified arguments. If the number of arguments provided 
; does not match the number of arguments of the function, then an error is 
; signaled.

;
;apply fun arg1 arg2 : : : argn list                                [Function]
;
; Apply fun to the k arguments specified and the elements of list. If the 
; number of arguments of the function di er from k plus the length of list 
; then an error is signaled.


;;
;; GE Functions and Applications (6.6.1)
;;

; The most commonly used type of function in Weyl is ge-function. These are 
; functions with *well-known* names. The easiest way to generate examples of
; ge-functions is to invoke the Lisp function with the same name. For instance,
;   > (setq appl (sin 'x))
;   sin(x)
; This expression sin x is not a ge-function, but a ge-application. It consists
; of two pieces, a function and an argument list. These pieces can be extracted
; using the functions funct-of and args-of.

(defvar appl (sin 'x))
;=> APPL

;
; funct-of application                                              [Function]
; 
; Returns the ge-function in the functional position of application.


;
; args-of application                                               [Function]
;
; Returns a list of the arguments of application.

; For instance, using appl created above, we have
;   > (funct-of appl)
;   sin
;   > (args-of appl)
;  (x)

(funct-of appl)
;=> sin

(args-of appl)
;=> (x)

; The ge-function can be used with funcall and apply to create new applications.
;   > (funcall (funct-of appl) (expt 'y 2))
;   sin(y^2)
;   > (apply (funct-of appl) (list 'y))
;   sin(y)

(funcall (funct-of appl) (expt 'y 2))
;=> sin(y^2)

(apply (funct-of appl) (list 'y))
;=> sin(y)


; Like all classes that inherit from ge-function, instances of 
; abstract-function handle the nargs-of method.
;   > (nargs-of (funct-of appl))

(nargs-of (funct-of appl))
;=> 1

; In addition to information about the number of arguments accepted by a 
; function, we can also indicate the function's derivative. This is done 
; using the following function: 

;
; declare-derivative func args &body body [Function]
;
; This form is used to de ne the derivative of func. The body of this form 
; can do any computation it wants on the arguments of func. For instance, 
; the derivative of sin and cosine are de ned as follows:
;     (declare-derivative sin (x) var
;       (* (deriv x var) (cos x)))
;
;     (declare-derivative cos (x) var
;       (* (- (deriv x var)) (sin x)))
;
; With these definitions, which are already in Weyl, derivatives of general 
; expressions involving sine and cosine can be performed:
;
;  > (deriv (* (sin 'x) (cos 'x)) 'x)
;  (cos(x))^2 - (sin(x))^2

(deriv (* (sin 'x) (cos 'x)) 'x)
;=> (cos(x))^2 - (sin(x))^2

;;
;; Applicable functions (6.6.2) 
;;

; Applicable functions are a mechanism to enable manipulation of anonymous 
; functions. In languages like Scheme and Common Lisp they correspond to 
; lambda-expressions. One of the useful features of Weyl is that one can 
; perform arithmetic operations on functions. This often forces us to produce
; applicable functions. For instance,
;
;   > (+ (funct-of (sin 'x)) (funct-of (cos 'x)))
;   (lambda (v.1) sin(v.1) + cos(v.1))
;
; To explicitly create an applicable function, the function 
; make-applicable-function is used.

;
; make-applicable-function args body                                [Function]
;
; Create an applicable function. The variables used in args are replaced by 
; newly generated variables, and the uses of the variables in body are 
; similarly replaced.

; Like other functions, one can differentiate applicable functions.

;
; deriv function integer1 integer2 ...                              [Function]
;
; Returns the function obtained by taking the positional derivative of function
; with respect to the given positions.
; For example:

; ** WRONG in manual: make-applicable-function does not exist, but
; weyli::make-app-function in functions.lisp
(defvar f1 (weyli::make-app-function '(u v) (+ (* 'u 'v) (* 'u 'u 'u))))
;=> F1

f1
;=> (lambda (v.1 v.2) v.1^3 + v.2 v.1)

(deriv f1 1)
;=> (lambda (v.1 v.2) v.1)

(deriv f1 0)
;=> (lambda (v.1 v.2) v.2 + 3 v.1^2)

(deriv f1 1 0)
;=> (lambda (v.1 v.2) 1)

; The example given in the manual:
(defvar f0 (make-app-function '(x y) (+ (* 'x 'y) (* 'y 'y))))
;=> F0

f0
;=> (lambda (v.1 v.2) v.2^2 + v.2 v.1)

(deriv f0 1)
;=> (lambda (v.1 v.2) 2 v.2 + v.1)

;(deriv f0 2)
;=> 
;debugger invoked on a SIMPLE-ERROR @7B5FACF1B4CD in thread
;#<THREAD "main thread" RUNNING {10048C8253}>:
;  Cannot take deriv of (lambda (v.1 v.2) v.2^2 + v.2 v.1) and 2
; however

(deriv f0 0)
;(lambda (v.1 v.2) v.2)

; Therefore, contrary to the manual, derivs are zero based now. Hmm, maybe
; we should change this.

;;
;; Sampled Functions (6.6.3)
;;

; nothing in manual, tbw.


;;
;; Substitution
;;

;
; substitute value var expr &rest ignore
;
; Substitutes value for each occurrence of var in expr. If value 
; is a list, it is interpreted as a set of values to be substituted in 
; parallel for the variables in var. The values being substituted must 
; be either elements of the domain of polynomial or its coefficient domain.
; still valid? To review ...

(documentation 'substitute 'function)
;=> "Extend the common lisp SUBSTITUTE function."

(substitute q p (sin p))
;=> sin(q)

(substitute (expt x q)  p (sin p))
;=> sin(x^q)

(substitute (+ a b)  p (expt y  (* p p)))
;=> y^(b + a)^2


;No way to raise 2 to the p power
;(expt (coerce 2 *general*) p)

(substitute 2  p (expt y  (* p p)))
;=> y^4

;(substitute 2  y (expt y  (* p p)))
;No way to raise 2 to the p^2 power

;* (expt 'e 2)
;e^2
;* (expt 'e (* p (log 2)))
;e^(0.6931472 p)
;* (expt 'e (* p (log (coerce 2 *general*))))
;e^(p (log(2)))
;*
;(defvar tt (expt 'e (* p (log (coerce 2 *general*)))))
;TT
;* tt
;e^(p (log(2)))
;* (deriv tt p)
;(log(e)) e^(p (log(2))) (log(2))
;*

;;
;; Equations
;;

(defvar eq1 (make-instance 'weyli::ge-eqn= :lhs (+ p q) :rhs (expt p q)))
;=> EQ1
EQ1
;=> q + p = p^q

(slot-value eq1 'weyli::lhs)
;=> q + p

(wtype eq1)
;=> WEYLI::GE-EQN=

(defvar eq2 (make-instance 'weyli::ge-eqn> :lhs (+ p q) :rhs (expt p q)))
;=> EQ2
eq2
;=> q + p > p^q

(lhs-of eq2)
;=> q + p

(defvar eq3 (make-instance 'weyli::ge-eqn>= :lhs (+ p q) :rhs (expt p q)))
;=> EQ3
eq3
;=> q + p >= p^q

(rhs-of eq3)
;=> p^q

(describe (wtype eq1))
;WEYLI::GE-EQN=
;  [symbol]
;
;GE-EQN= names the standard-class #<STANDARD-CLASS WEYLI::GE-EQN=>:
;  Class precedence-list: WEYLI::GE-EQN=, WEYLI::GE-EQUATION,
;                         WEYLI::GENERAL-EXPRESSION,
;                         WEYLI::GE-OR-NUMERIC, WEYLI::DOMAIN-ELEMENT,
;                         STANDARD-OBJECT, SB-PCL::SLOT-OBJECT, T
;  Direct superclasses: WEYLI::GE-EQUATION
;  No subclasses.
;  No direct slots.
;*

(describe 'WEYLI::GE-EQUATION)
;WEYLI::GE-EQUATION
;  [symbol]
;
;GE-EQUATION names the standard-class #<STANDARD-CLASS WEYLI::GE-EQUATION>:
;  Direct superclasses: WEYLI::GENERAL-EXPRESSION
;  Direct subclasses: WEYLI::GE-EQN>=, WEYLI::GE-EQN>, WEYLI::GE-EQN=
;  Not yet finalized.
;  Direct slots:
;    WEYLI::LHS
;      Initargs: :LHS
;      Readers: LHS-OF
;      Writers: (SETF LHS-OF)
;    WEYLI::RHS
;      Initargs: :RHS
;      Readers: RHS-OF
;      Writers: (SETF RHS-OF)
;*


; Actually there are constructors in general.lisp for equations:

(weyli::make-ge-eqn=  *general* (+ p q) (expt p q))
;=> q + p = p^q

(weyli::make-ge-eqn>  *general* (+ p q) (expt p q))
;=> q + p > p^q

(weyli::make-ge-eqn>=  *general* (+ p q) (expt p q))
;=> q + p >= p^q

; special for *general* in general.lisp as well (somehow redundant ...??)
; However, here we do not have to use prefix weyli:: 
(eqn>=  (+ p q) (expt p q))
;=> q + p >= p^q

(eqn=  (+ p q) (expt p q))
;=> q + p = p^q








;;
;; GE-FUNCTION
;;

;; create a function f of two variables in domain *general*
(defvar f (weyli::make-function *general* 'f 2))
(wtype f) ;-> WEYLI::GE-FUNCTION
(describe f) ; ....
(derivs-of f) ;-> nil

(weyli::make-ge-funct *general* 'g)
;-> g()

(defvar h (weyli::make-ge-funct *general* 'h p  ))
; -> H = h(p)

(deriv h p)
;h_{0}(p)
(deriv h p p q)
;0
(deriv h p p )
;h_{00}(p)





