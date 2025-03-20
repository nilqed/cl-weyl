(ql:quickload :weyl)
(in-package :weyl)

;;;; Basics (4)
;;;;
;;;; This chapter describes the basic tools used by Weyl to model the algebraic
;;;; structures of mathematics. The fundamental di erence between Weyl and most
;;;; other systems is that in addition to providing a representation for domain
;;;; elements like polynomials, Weyl also provides representations for the 
;;;; algebraic domains of which the polynomials are elements. In Section 4.2 we
;;;; discuss domains and domain elements. Section 4.3 discusses morphisms, 
;;;; which are used to define the relationships between the elements of two 
;;;; domains. Section 4.4 illustrates how to make use of these relationships.
;;;; Section 4.5 presents the hierarchy of domains being used in Weyl. Weyl 
;;;; uses the Common Lisp Object System quite heavily. We begin this chapter 
;;;; with a brief introduction to CLOS, focusing on those points that are 
;;;; relevant to Weyl.

;;; Common Lisp Object System (4.1)
;;; Weyl is based on the Common Lisp Object System (CLOS) [1] programming model. 
;;; Since this model of object-oriented programming differs significantly from 
;;; the more common, message-based paradigm used in Smalltalk and C++, we give
;;; a short introduction here [see the manual, Section 4.1].

(defclass complex-number ()
  ((a :initarg :real :accessor real-part)
   (b :initarg :imag :accessor imag-part)))
   
;;; Domains (4.2 )
;;; The objects that are usually manipulated in algebraic computation systems
;;; (integers, polynomials, algebraic functions, etc.) are called 
;;; `domain elements` in Weyl. They are elements of algebraic objects called 
;;; domains . Examples of domains are the ring of (rational) integers Z, the 
;;; ring of polynomials in x, y and z with integer coefficients, Z[x; y; z ], 
;;; and the field of Gaussian numbers, Q[i].
;;; [more in the manual, Section 4.2]

(defvar ZZ (get-rational-integers))
(defvar QQ (get-rational-numbers))
(defvar RR (get-real-numbers))

(describe rr)

; R
;  [standard-object]
;
; Slots with :INSTANCE allocation:
;  PROPERTY-LIST                  = (:COMPLETE-SET T :ORDERED-DOMAIN T :INTEGRAL-DOMAIN T)
;  OPERATION-TABLE                = #<HASH-TABLE :TEST EQL :COUNT 18 {100481C333}>
;  SUPER-DOMAINS                  = NIL
;  MORPHISMS-FROM                 = NIL
;  MORPHISMS-TO                   = (Q->R)
;  PRINT-FUNCTION                 = WEYLI::REAL-NUMBERS-PRINT-OBJECT
;  COEFFICIENT-DOMAIN             = NIL


(defvar Q-mes (get-polynomial-ring QQ '(m e s)))
Q-MES
; => Q[m, e, s]



;; Subdomains and Superdomains (4.2.1)
;;
;; In some circumstances it is useful to be able to create a domain that is a
;; subdomain of a larger domain. For instance, the positive integers are 
;; viewed as a subdomain of the rational integers. The elements of an ideal 
;; of ring form an additive group that is a subdomain of the original ring.
;;
;;   super-domains-of domain  [Generic]
;;
;; Returns the superdomains that contain domain.
;;
;; At the moment, nothing uses this mechanism. The superdomain concept is 
;; intended for dealing with deductive questions like: Are there any zero 
;; divisors in the set of positive integers? Although the set of positive 
;; integers is not an integral domain, it is a subsemigroup of an integral 
;; domain and thus none of the positive integers is a zero divisor. Similarly,
;; it is intended that an ideal of a ring R will be implemented as an R-module
;; that is also a subdomain of R.

(weyli::super-domains-of ZZ)
; => NIL

;;; Morphisms (4.3)
;;;
;;; Morphisms are maps between domains. They are rst-class objects in Weyl and 
;;; can manipulated like domains and domain elements. In particular, two 
;;; morphisms can be composed using the operation compose. Morphisms are 
;;; created using the function make-morphism, which takes three arguments.
;;;
;;;   make-morphism domain mapping range &key (replace? T)  [Generic]
;;;
;;; This function creates a morphism from domain to range. Mapping is a 
;;; function of one argument that takes an element of domain and returns an 
;;; element of range . If replace? is true, then any existing morphisms between
;;; domain and range are deleted before the new morphism is created.
;;;
;;; All morphisms created are remembered in the \*morphisms\* data structure. 
;;; (Initially, this is just a list.) To nd any existing homomorphisms we can
;;; use the function get-morphisms.
;;;
;;;   get-morphisms &key domain range                              [Function]
;;;
;;; When given no arguments this function returns a list of all the morphisms 
;;; Weyl currently knows about in this context. When the domain is provided, 
;;; it returns all morphisms from domain to anywhere. Similarly, when range 
;;; is provided, only those morphisms that map into range are returned. When 
;;; both range and domain are given, then only those morphisms from domain to
;;; range are returned
;;;

(get-morphisms)
; => (Z->Q Z->R Q->R Z->Q[m, e, s] Q->Q[m, e, s])

(get-morphisms :domain *general*)
; => NIL

(get-morphisms :domain QQ)
; => (Q->R Q->Q[m, e, s])

;;; *** NOTE: neither *morphisms* nor weyli::*morphisms* are defined.

;;; Classes have been provided to indicate that more is known about the map 
;;; than that is a simple morphism. In particular, a morphism can be a 
;;; homomorphism, an injection, a surjection, an isomorphism and an 
;;; automorphism. At the moment nothing takes advantage of this information.


;;; Coercions (4.4)
;;;
;;; Explicit coercions are performed by the function coerce:
;;;
;;;    coerce element domain  [Generic]
;;;
;;; Coerce finds an element of domain that corresponds with element. This is 
;;; done using one of two methods. First, there may be a `canonical` coercion, 
;;; which is one that is defined via explicit `coerce` methods. These methods 
;;; take care of mapping Lisp expressions, like numbers and atoms, into Weyl 
;;; domains. If there are no canonical coercion methods then coerce checks to 
;;; see if there is a unique morphism between element's domain and domain. If 
;;; so, this morphism is used to map element to domain. If there is more than
;;; one morphism then an error is signaled. If the the switch *allow-coercions*
;;; is set to false (nil) then the canonical maps are the only ones that will 
;;; be used by coerce. However, if the user sets *allow-coercions* to T then 
;;; by creating a homomorphism between two domains, the set of canonical
;;; maps between domains can be extended. If coerce cannot nd any other 
;;; predefined mapping between the domain of element and domain it then s
;;; earches the set of all defined homomorphisms. If there exists a canonical 
;;; homomorphism between the two domains then it is used to map element into 
;;; domain . If there does not exist a canonical homomorphism but there is 
;;; exactly one homomorphism between the two domains, then it is used.

(defvar q2/3 (coerce (/ 2 3) QQ))
; =>Q2/3

q2/3
; => 2/3

(describe q2/3)

; 2/3
;  [standard-object]
;
; Slots with :INSTANCE allocation:
;  DOMAIN                         = Q
;  NUMERATOR                      = 2
;  DENOMINATOR                    = 3
;*

;;; As a general rule, Weyl does not provide for implicit coercions of 
;;; arguments to functions. Thus we assume that in the expression (+ a b) the 
;;; domains of a and b are the same. If this is not the case, the user must 
;;; explicitly insert a coercion. The reason for this is to deal with problems
;;; such as would arise if, for example, a were 1=2 and b were x. The domain of
;;; a is Q, while the domain of b is Z[x]. The sum cannot be represented in 
;;; either domain, and in fact we have the choice of embedding it in either 
;;; Q[x] or Z(x). The wrong choice could be lead to very inefficient algorithms.
;;; However, we do make one exception. There is assumed to be a canonical 
;;; homomorphism of the rational integers into every domain. If only one of 
;;; the arguments to one of the four basic arithmetic operations (+, -, * and /)
;;; is an element of Z, then it is automatically coerced into the domain of
;;; the other argument.



;;; Hierarchy of Domains (4.5)
;;;
;;; The root of the domain hierarchy is the class domain. It provides a number 
;;; of utility routines for managing the set of operations and axioms associated
;;; with a domain. However, all algebraic domains are built on the higher level
;;; class set. The basic algebraic domains are given in Figure 4.2. The elements
;;; of a domain of class set can be compared using the binary operator 
;;; weyli::binary=. However, users will find it more convenient to use the 
;;; macro =, which converts several arguments into a sequence of calls to 
;;; weyli::binary=. That is
;;;
;;;    (= a b c) ==> (and (weyli::binary= a b)(weyli::binary= b c)).
;;;
;;;
;;;    weyli::binary= x y                                          [Generic]
;;;
;;; Test to see if x and y are equal. Equality is meant in the mathematical 
;;; sense, so two vectors are = if their components are =.
;;;

(weyli::binary= (coerce 10 ZZ) (coerce 10 QQ))
; => NIL

(= (coerce 10 ZZ) (coerce 10 QQ))
; => NIL

(= (coerce 10 RR) (coerce 10 RR))
; ==> T


(= (coerce (/ 5 2) RR) (coerce 2.5  RR))
; => NIL

(describe (coerce (/ 5 2) RR))

; 5/2
;  [standard-object]
;
; Slots with :INSTANCE allocation:
;  DOMAIN                         = R
;  NUMERATOR                      = 5
;  DENOMINATOR                    = 2

(describe (coerce 2.5  RR))

; 2.5
;  [standard-object]
;
; Slots with :INSTANCE allocation:
;  DOMAIN                         = R
;  VALUE                          = 2.5
; *




;; Semigroups, Monoids and Groups (4.5.1)
;;
;; This section discusses domains that have one operation. That is, they consist
;; of a set S and a binary operation (+) such that for two elements a and b in 
;; S, a (+) b is an element of S . The simplest interesting class of such 
;; domains is a semigroup, where (+) is assumed to be associative. That is, for
;; a, b and c elements of S , 
;;    (a (+) b) (+) c = a (+) (b (+) c).
;; In Weyl, domains that are semigroups include the class semigroup. In 
;; addition, the operation of a semigroup is assumed to be weyli::times [4]. As 
;; with weyli::binary=, there is a macro * that simplifies its use. 
;; For instance,
;;
;;   (* a b c) = (weyli::times (weyli::times a b) c).
;;
;; In addition, repeated multiplication can be indicated by expt. That is,
;;
;;  (expt a 3) = (weyli::times (weyli::times a a) a).
;;
;; although it may be computed more efficiently for elements of some domains.
::
;;
;;    weyli::times x y                                               [Generic]
;;
;; Returns the product of two elements of a semigroup.
;;
;; If one wants to compute the product of all the elements of a lists, a 
;; convenient idiom is to `apply` the `*` function to the list. This is **not** 
;; possible with the Weyl `*` operator since it is a macro, not a function. 
;; As a work around for this problem, Weyl also provides a function for
;; multiplication caled `%times`. Semantically, this function is the same as 
;; the `*` macro, but the code that would be generated if `%times` were used 
;; in place of `*` is not as efficient [5].
;;
;;    * &rest args
;;    %times &rest args  [Generic]
;;
;; Monoid's are semigroups that contain a multiplicative identity. This element
;; is called one and may be accessed by applying the function one to the domain.
;;
;;    one semigroup     [Generic]
;;
;; Returns the multiplicative identity of monoid.
;;
;;   1? elt   [Generic]
;;
;; Returns true if its argument is the multiplicative identity and false otherwise.

;; In addition, the expt function is extended so if its second argument is 0, 
;; it returns the multiplicative identity.

;; The elements of a group have multiplicative inverses which can be determined
;; using recip
;;
;;    recip elt    [Generic]
;;
;; Returns the multiplicative inverse of elt .


; [4] This is a limitation of the current implementation of Weyl and will be 
;     fixed shortly.
; [5] The need for this function can be eliminated by compiler macros, but this
;     solution cannot currently be implemented in a portable manner.


;; For efficiency reasons it often valuable to have a binary operation that 
;; multiplies an element by the multiplicative inverse of the second argument. 
;; This operation is called weyli::quotient. The macro `/` can be used to 
;; simplify its use:
;;
;;      (/ a) = (recip a)
;;      (/ a b) = (weyli::quotient a b)
;;      (/ a b c) = (weyli::quotient (weyli::quotient a b) c)
;;
;;
;; In a group, the `expt` operation is extended to apply to negative second 
;; arguments also. In this case
;;
;;      (expt x ?n) = (expt (recip x) n)
;;
;;
;;   expt a n  [Generic]
;;
;; Multiplies a by itself n times. If the domain of a just a semigroup, then n 
;; must be a positive Lisp integer. When the domain of a is a monoid, then n 
;; can be equal to zero. If the domain of a is a group then n can be negative.

;; A binary operation (+) on a semigroup S is commutative if for every pair of 
;; elements in a and b in S , a (+) b = b (+) a. The domains abelian-semigroup, 
;; abelian-monoid and abelian-group are similar to the domains semigroup, monoid
;; and group except the binary operation is plus instead of times.
;; This is the biggest distinction between the domain structure of Weyl and the
;; domains of modern algebra.[6]
;;
;;    + &rest args
;;    %plus &rest args
;;
;;    weyli::plus x y   [Function]
;;
;; Returns the sum of x and y , which are elements of an abelian-semigroup. 
;; weyli::plus is a commutative binary operation.

;; The multiple argument version of weyli::plus is +, which is what should be 
;; used in all applications.
;;
;;
;;    zero abelian-semigroup    [Function]
;;
;;  Returns the additive identity of an abelian semigroup.
;;
;;    zero? elt
;;    0? elt                   [Function]
;; 
;; Returns true if its argument is the additive identity and false otherwise.

;; The additive inverse of an element of an abelian-group can be obtained using
;; weyli::minus. As the multiplicative case, there is a binary operation, 
;; weyli::difference and a macro - that makes these operations easier to use.

;;      (- a) = (weyli::minus a)
;;      (- a b) = (weyli::difference a b)
;;      (- a b c) = (weyli::difference (weyli::difference a b) c)

; [6] It would be better if any operation could be declared to be commutative.


; Figure 4.3: Different Ring Notations


;; The function `%difference` is provided for use when a multi-argument version
;; of the `-` should be passed to another function. The same caveats about 
;; efficiency apply here.
;;   Notice that algebraically we consider all of these domains to possess 
;; only one operation weyli::times, or weyli::plus in the abelian case. The 
;; other operations are either abbreviations, e.g., expt, or are constructive 
;; versions of existential axioms. For instance, a monoid M obeys the axiom: 
;; There exists an element e in M such that all for elements a of M , 
;; e - a = a - e = a. The function one just returns the element e.

;; Rings (4.5.2)
;;

(wtype q-mes)
; => WEYLI::MULTIVARIATE-POLYNOMIAL-RING


(describe Q-mes)

; Q[m, e, s]
;  [standard-object]
;
; Slots with :INSTANCE allocation:
;  PROPERTY-LIST                  = (:INTEGRAL-DOMAIN T)
;  OPERATION-TABLE                = #<HASH-TABLE :TEST EQL :COUNT 17 {10015DC6E3}>
;  SUPER-DOMAINS                  = NIL
;  MORPHISMS-FROM                 = NIL
;  MORPHISMS-TO                   = (#1=Q->#1#[m, e, s])
;  PRINT-FUNCTION                 = WEYLI::POLYNOMIAL-RING-PRINT-OBJECT
;  ZERO                           = 0
;  ONE                            = 1
;  VARIABLES                      = (m e s)
;  VARIABLE-HASH-TABLE            = ((m 0) (e 1) (s 2))
;  VARIABLE-TABLE                 = #2A((m 0) (e 0) (s 0))



;; Modules (4.5.3)
;;
;;    coefficient-domain-of module   [Function]
;;
;; Returns the coefficient domain of module.

(coefficient-domain-of q-mes)
; => Q


;; Properties (4.5.4)
;;






