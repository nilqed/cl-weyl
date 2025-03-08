(ql:quickload :weyl)
(in-package :weyl) 

; TUPLES (3.3)
; Instances of the class weyli::tuple are simply one-dimensional vectors.
; The initialization value can be either a list or a Lisp vector.
(defvar tup (make-instance 'weyli::tuple :values '(1 2 3)))

(cl-user::type-of tup)
(describe tup)


; (length  tup) not working as advertised in manual
 (slot-value tup 'weyli::value)
 (length (slot-value tup 'weyli::value))
 
;The generic function ref is used in place of the Common Lisp functions 
;aref or svref.
; >> ref sequence &rest indices   [Macro]
;Accesses the indicated elements of sequence. Tuples are one-dimensional 
;arrays so only one index is allowed. The indexing scheme is zero based, 
;so the rst element of the tuple has index 0, the second 1 and so on.
(ref tup 0)
(ref tup 1)


;It is sometimes useful to be able to convert a tuple into a list of 
;its elements.
; >> list-of-elements tuple  [Method]
;Returns a list of the elements of tuple. For example,
(list-of-elements tup)


;The following functions extend the Common Lisp mapping functions to work with 
;tuples as well as the usual sequences of Common Lisp.
; >> map type function tuple &rest sequences  [Method]
;The number of arguments of function is expected to be one more than the number
;of elements in sequences. function is applied to each element of tuple and the 
;corresponding elements of each of the elements of sequences. For instance:
(map 'tuple #'cons tup tup)


;Algebraic objects in Weyl have a slot that indicates the algebraic domain of 
;which the object is an element. When creating an instance of such an object 
;it is necessary to indicate this domain. If the sequence returned by map 
;requires this information then the domain will be extracted from the tuple. 
;If it is necessary to explicitly specify the domain of the resulting tuple, 
;the following function may be used:
; >> map-with-domain type domain function tuple &rest sequences  [Method]
;Similar to map but the domain of the resulting tuple will be domain.

; got no example working


;Arithmetic with Lists (3.4)
;These routines use the Weyl arithmetic operations, and thus can be used both 
;for arrays and lists that contain Lisp numbers and those those that contain 
;Weyl's mathematical objects.

; >> list-inner-product list1 list2 [Method]
;Computes the sum of the pairwise product the elements of list1 and list2.
;GONE!
;
; >> list-expt base-list expt-list [Method]
;Returns a list of consisting of the elements of base-list raised to the power 
;of the corre-sponding element of expt-list. 
;GONE!
;
; >> array-times array1 array1 [Method]
;Checks to make sure the arguments of the of the right dimensions and then 
;computes a matrix product storing the result in a new, appropriately sized 
;matrix. Found in "matrix.lisp" ; requires WEYLI:: prefix!

;AVL Trees (3.5, avl.lisp)

(defvar tree (make-instance 'weyli::avl-tree))
;TREE
;;; lots of mysteries ....

;* tree
;#<AVL tree: 0 elts>
;* (cl-user::type-of tree)
;WEYLI::AVL-TREE
(describe  tree)

(defvar node1 (make-instance 'weyli::avl-node :left 'A :right 'B))
;NODE1

(defvar node2 (make-instance 'weyli::avl-node :left 'C :right 'D))
;NODE2

(describe node1)
;#<error printing a WEYLI::AVL-NODE: #<UNBOUND-SLOT KEY {1003F0C153}>>
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  DOMAIN                         = #<unbound slot>
;  KEY                            = #<unbound slot>
;  LEFT                           = A
;  RIGHT                          = B
;  BALANCE                        = 0
;*

(defvar node3 (make-instance 'weyli::avl-node :key 'n3  :left 'A :right 'B))
; insert still not working

(weyli::empty? tree)
;T

;; seems to be incomplete ... ever worked??


; BASICS (4)
;There are always two functions for obtaining a domain. The one that begins 
;weyli::make- always creates a new domain. The one beginning with get- tries 
;to find an isomorphic domain that already exists. Thus (get-rational-integers)
;always returns the same rational integer domain, while (make-rational-integers)
;always creates a new instance of the rational integers.

(get-rational-integers)
;Z

(weyli::make-rational-integers)
;Z

;Subdomains and Superdomains (4.2.1)
;Returns the superdomains that contain domain.
;At the moment, nothing uses this mechanism. The superdomain concept is 
;intended for dealing with deductive questions like: Are there any zero 
;divisors in the set of positive integers?

(weyli::super-domains-of *general* )
;NIL

;Morphisms (4.3)
;Morphisms are maps between domains. They are rst-class objects in Weyl and 
;can manipulated like domains and domain elements. In particular, two morphisms 
;can be composed using the operation compose. Morphisms are created using the 
;function make-morphism, which takes three arguments.
;
; >> make-morphism domain mapping range &key (replace? T) [Generic]
;
;This function creates a morphism from domain to range. Mapping is a function 
;of one argument that takes an element of domain and returns an element of 
;range . If replace? is true, then any existing morphisms between domain and 
;range are deleted before the new morphism is created.
;All morphisms created are remembered in the *morphisms* data structure. 
;(Initially, this is just a list.) To nd any existing homomorphisms we can 
;use the function get-morphisms:
; 
; >> get-morphisms &key domain range  [Function]
; 
;When given no arguments this function returns a list of all the morphisms 
;Weyl currently knows about in this context. When the domain is provided, it 
;returns all morphisms from domain to anywhere. Similarly, when range is 
;provided, only those morphisms that map into range are returned. When both 
;range and domain are given, then only those morphisms from domain to range 
;are returned.

(weyli::make-morphism *general* (lambda (x) x) *general*)
;#<Domain: GENERAL-EXPRESSIONS>->#<Domain: GENERAL-EXPRESSIONS>

(get-morphisms)
;NIL

; (get-morphisms :domain *general*) not working as with :domain


;4.4 Coercions (see Weyl Manual for details)
;Explicit coercions are performed by the function coerce:
;
; >> coerce element domain [Generic]
;Coerce finds an element of domain that corresponds with element.

;As a general rule, Weyl does not provide for implicit coercions of arguments 
;to functions. Thus we assume that in the expression (+ a b) the domains of 
;a and b are the same. If this is not the case, the user must explicitly 
;insert a coercion.

(coerce  2345 (get-rational-integers))
;2345

(describe (coerce  2345 (get-rational-integers)))
;2345
;  [standard-object]
;
;Slots with :INSTANCE allocation:
;  DOMAIN                         = Z
;  VALUE                          = 2345
;*

(describe 'rational-integer)
;WEYLI:RATIONAL-INTEGER
;  [symbol]
;
;RATIONAL-INTEGER names the standard-class #<STANDARD-CLASS WEYLI:RATIONAL-INTEGER>:
;  Class precedence-list: RATIONAL-INTEGER, WEYLI::NUMERIC,
;                         WEYLI::GE-OR-NUMERIC, WEYLI::DOMAIN-ELEMENT,
;                         STANDARD-OBJECT, SB-PCL::SLOT-OBJECT, T
;  Direct superclasses: WEYLI::NUMERIC
;  No subclasses.
;  Direct slots:
;    WEYLI::VALUE
;      Initargs: :VALUE
;      Readers: WEYLI::INTEGER-VALUE



;4.5 Hierarchy of Domains

(defvar w2345  (coerce  2345 (get-rational-integers)))
;W2345

w2345
;2345

(weyli::binary= w2345 2345)
;T

(= w2345 2345)
;T

; although 
; (describe 2345)
; 2345
; [fixnum]


;4.5.1 Semigroups, Monoids and Groups


;Scalar Domains
;Almost all domains that occur in mathematics can be constructed from the 
;rational integers, Z.

;There are several di erent ways to determine if an object is a scalar.
;   (typep obj 'cl:number) a Lisp number,
;   (typep obj 'numeric) a Weyl number,
;   (number? obj)
;either a Lisp number or a Weyl number.

(typep 2345 'cl:number)
;T
(typep w2345 'cl:number)
;F

(typep w2345 'weyli::numeric)
;T

(typep 2345 'weyli::numeric)
;NIL

(number? w2345)
;T
(number? 2345)
;T


;;;;;;;;;;;;;;;;;
;;  ARRAY is a bad type specifier for sequences.
(defvar tsd1 (weyli::get-tpower-series-domain (get-rational-integers) 'x))
(defvar tsd2 (get-tpower-series-domain (get-rational-numbers) 'x))
(taylor (sin 'x) tsd2  8)

(ge-var p) ;; not standard
(defvar tsdp (get-tpower-series-domain (get-rational-numbers) p))
(taylor (sin p) tsdp 4) 
