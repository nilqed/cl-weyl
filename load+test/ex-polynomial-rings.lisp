(ql:quickload :weyl)
(in-package :weyl)


#|
Polynomial Rings
================
Polynomial rings are domains which consist of polynomials in some number of 
variables over a coefficient domain, e.g. :math:`\mathbb{Z}[x]`, 
:math:`\mathbb{R}[x]`, :math:`\mathbb{Q}(t)[x]`. In these three cases the 
coefficient domains are the rational integers :math:`\mathbb{Z}`, 
the real numbers :math:`\mathbb{R}` and powerseries in t. Polynomial rings are 
created using the function get-polynomial-ring.

.. function:: get-polynomial-ring coefficient-domain list-of-variables  [Function]

   Coefficient-domain must be a ring. List-of-variables is a list of arbitrary 
   Lisp objects, each of which represents a generator of the polynomial ring.
   
.. code-block:: lisp

    (defvar Z (get-rational-integers))
    => Z
    
    (defvar Z[xyz] (get-polynomial-ring Z '(x y z)))
    => Z[xyz]
    
    Z[XYZ]
    => Z[x, y, z]


.. function:: transcendence-degree domain1 domain2    [Function]

   This function checks to see that domain2 is a subring domain1, and if so 
   returns the transcendence degree of domain1 over domain2.


Information About Variables (9.1)
---------------------------------
The variables of a polynomial ring have three di erent representations: as a 
polynomial, as symbol, or as a variable index . The simplest representation is 
as a symbol. These are the symbols passed to get-polynomial-ring and are the 
only mechanism the user has to in uence the external printed representation of 
the variable. Internal to the polynomial package, there are (Lisp) integers 
associated with variables. These integers are used as indices into tables and 
to indicate the ordering of variables in multivariate polynomials. Finally, 
a variable can be represented as a polynomial. Regardless of the internal 
representation used for polynomials, there is an integer associated with each 
variable of the polynomial. These numbers are used as an index into all data 
structures that hold additional information about the variable. The index 
corresponding to a variable is obtained by the generic function variable-index. 
The variable corresponding to an order number is variable-symbol.

.. function:: variable-index domain variable             [Function]

   Returns the variable index corresponding to variable. Variable can be either 
   a symbol or a polynomial.


.. function:: variable-symbol domain variable    [Function]

   Returns the variable symbol corresponding to variable. Variable can be 
   either an integer or a polynomial.

There is a property list associated with each variable in a polynomial ring. 
This property list is ring specific and not global. The ring property list 
is accessed using the generic function get-variable-property. Properties 
can modified using setf, as with normal property lists.

.. function:: get-variable-property domain variable property   [Function]

   Returns a property property of variable .

.. code-block:: lisp

    * (describe Z[xyz])
    Z[x, y, z]
      [standard-object]

    Slots with :INSTANCE allocation:
      PROPERTY-LIST                  = (:INTEGRAL-DOMAIN T)
      OPERATION-TABLE                = #<HASH-TABLE :TEST EQL :COUNT 17 {10032F3DE3}>
      SUPER-DOMAINS                  = NIL
      MORPHISMS-FROM                 = NIL
      MORPHISMS-TO                   = (#1=Z->#1#[x, y, z])
      PRINT-FUNCTION                 = WEYLI::POLYNOMIAL-RING-PRINT-OBJECT
      ZERO                           = 0
      ONE                            = 1
      VARIABLES                      = (x y z)
      VARIABLE-HASH-TABLE            = ((x 0) (y 1) (z 2))
      VARIABLE-TABLE                 = #2A((x 0) (y 0) (z 0))
      COEFFICIENT-DOMAIN             = Z
    *


.. code-block:: lisp

    * (describe 'get-variable-property)
    WEYLI:GET-VARIABLE-PROPERTY
      [symbol]

    GET-VARIABLE-PROPERTY names a generic function:
      Lambda-list: (DOMAIN VARIABLE KEY)
      Argument precedence order: (DOMAIN VARIABLE KEY)
      Derived type: (FUNCTION (T T T) *)
      Documentation:
        There is a property list associated with each variable in a polynomial
        ring. This property list is ring specific and not global. The ring
        property list is accessed using the generic function get-variable-property.
        Properties can modified using setf, as with normal property lists.
      Method-combination: STANDARD
      Methods:
        (GET-VARIABLE-PROPERTY (VARIABLE-HASH-TABLE T T))
        (GET-VARIABLE-PROPERTY (DOMAIN GE-VARIABLE T))
          Documentation:
            Returns a property property of variable.
      Source file: /home/kfp/quicklisp/local-projects/weyl/general.lisp

    (SETF GET-VARIABLE-PROPERTY) has setf-expansion: WEYLI::SET-VARIABLE-PROPERTY
    *

In contrast with normal mathematical usage, the polynomial rings of Weyl can be 
modified in a limited fashion after they have been created. In particular, it 
is possible to add variables to a polynomial ring, but removing variables is 
not allowed. The variables are added to the end of the ring's list of variables
so that they will be less \main" than any of the existing variables, and thus
only minimal changes to existing polynomial representations will be needed.

.. function:: add-new-variable ring var        [Function]

   Var is a variable in the general representation. This function modifies 
   ring to have an additional variable.

The behavior of this routine is illustrated by the following examples. First, 
we create a polynomial ring with two variables, and then add a third variable 
to it.

.. code-block:: lisp

    (defvar  r (get-polynomial-ring (get-rational-numbers) '(x y)))
    => R

    (add-new-variable r 'z)
    => Q[x, y, z]

Now try adding a more complex expression to the ring. In this case, 
add-new-variable determines that w and sin(z+x) are the only new kernels and 
adds them.

.. code-block:: lisp

    (add-new-variable r (expt (+ 'x 'w (sin (+ 'x 'z))) 3))
    => Q[x, y, z, w, sin(z + x)]


Polynomial Arithmetic (9.2)
---------------------------
The simplest way to create a polynomial is to coerce a general expression into 
a polynomial ring. For instance, if r is the ring Q[x,y], as de ned above, 
we could proceed as follows. First, we create a polynomial in x and y as 
general expressions.

.. code-block:: lisp

    (expt (+ 'x 'y) 3)
    => (y + x)^3

Notice that the expressions is not expanded. Next, we coerce the expression into the polynomial
ring r.

.. code-block:: lisp

    (coerce (expt (+ 'x 'y) 3) r)
    => x^3 + 3 y x^2 + 3 y^2 x + y^3

This time the expression is expanded. When represented as an element of a 
polynomial ring, as opposed to being general expressions, polynomials are 
represented using a canonical representation (as described in Section 9.5). 
This canonical representation expresses polynomials as a sum of terms. 
This is easily seen in the following example.

.. code-block:: lisp

    (defvar  p (+ (* (- 'x 'y) (+ 'x 'y)) (expt 'y 2)))
    => P
    
    p
    => (-1 y + x) (y + x) + y^2

General expressions can represent polynomials as products of sums, and thus 
do not have a canonical representation for the polynomial. When the expression 
is coerced into a polynomial ring, the products are expanded to produce a 
simple form for the expression.

.. code-block:: lisp

    (coerce p r)
    => x^2
    
Polynomial Operators (9.3)
--------------------------

.. function:: scalar? polynomial  [Function]

   The argument of this function is expected to be an element of a polynomial 
   domain. If the argument is an element of the coefficient field this function 
   returns T otherwise it returns nil.
   
.. code-block:: lisp

    


|#

(defvar Z (get-rational-integers))
(defvar Z[xyz] (get-polynomial-ring Z '(x y z)))

(variable-index Z[xyz] 'x)
;0
(variable-index Z[xyz] 'y)
;1
(variable-index Z[xyz] 'z)
;2
(variable-index Z[xyz] 'u)
;NIL

(weyli::variable-symbol  Z[xyz] 0)
;x

(weyli::variable-symbol  Z[xyz] 1)
;y

(weyli::variable-symbol  Z[xyz] 2)
;z

;;;(weyli::variable-symbol  Z[xyz] 3) error ----

;;; something wrong: Array dimensions are not equal
;;; only second time it works ????
;;; (WEYLI::%COPY-ARRAY-CONTENTS* #2A((x 0) (y 0)) #2A((0 0) (0 0) (0 0) (0 0)))
;;; source: (ERROR "Array dimensions are not equal.")

(defvar  r (get-polynomial-ring (get-rational-numbers) '(x y)))
r
(add-new-variable r 'z)

(add-new-variable r 't)
;Q[x, y, z, t]




