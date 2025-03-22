Sums, Products and Quotients of Domains
=======================================

Let A be category of domains, e.g., groups, rings, elds, etc. Given two elements
of A, say A and B, there are often ways of combining them to produce another 
element of A. This chapter discusses several of those techniques.

Direct Sums (7.1)
-----------------
Given two domains A and B , their direct sum can be viewed as the set of all 
pairs of elements in A and B, where arithmetic operations are performed 
component-wise. We denote the direct sum of A and B by A (+) B . If A, B and C 
are domains, then

.. math::

    A \oplus (B \oplus C ) = (A \oplus B) \oplus C = A \oplus B \oplus C,

so multiple direct sums are well defined. Direct sums are created using the 
function get-direct-sum:

.. function:: get-direct-sum domain_1 ... domain_n         [Function]

   Creates a domain that is the direct sum of domain_1 ... domain_n. If any 
   of the domain_i are themselves direct sums, then their components are 
   included  as if they had been explicitly specified.

The algebraic structure of the object returned by get-direct-sum depends upon 
the type of the objects of which it is composed. The following table illustrates 
the options.


+---------+---------+---------+
|    A    |     B   | A (+) B |      
+=========+=========+=========+
| Ring    | Ring    | Ring    | 
+---------+---------+---------+
| Field   | Field   | Ring    |          
+---------+---------+----+----+
| Group   | Ring    | Group   |          
+---------+---------+----+----+
| Abelian | Abelian | Abelian |          
+---------+---------+---------+

The dimension of a direct sum is the number of its components:

.. function:: dimension direct-sum  [Function]

   Returns the number of components of the direct sum.

To illustrate this the direct sum we can compute the direct sum of the real 
numbers, the rational numbers and the rational integers as follows:
::

    > (setq direct-sum (get-direct-sum (get-real-numbers)
                                       (get-rational-numbers)
                                       (get-rational-integers))
    R (+) Q (+) Z

The symbol used to indicate a direct sum when only ascii characters are 
available is (+). This domain has dimension 3 and it is a ring as one might 
expect:
::

    > (dimension direct-sum)
    3
    > (typep direct-sum 'ring)
    T
    > (typep direct-sum 'field)
   NIL
   
   
Even though some of the components of the direct sum domain are fields, the 
direct sum itself is a ring. Individual elements of the direct sum can be 
extracted using the routine ref:
::

    > (ref direct-sum 0)
    R
    > (ref direct-sum 2)
    Z


.. function:: ref direct-sum i  [Function]  

   Returns the i-th element (zero based) of direct-sum. direct-sum can either 
   be a direct sum domain or an element of a direct sum domain.

Elements of a direct sum domains are created using the following function:

.. function:: make-element direct-sum element_1 ... element_n  [Function]

   Creates an element of direct-sum . The number of elements provided must 
   match the dimension of direct-sum . Each of the elements is coerced into 
   the domain of their associated component of the direct-sum before the 
   element is created.

The following illustrates the use of elements of a direct sum.
::

    > (setq x (make-element direct-sum 1 2 3))
    1 (+) 2 (+) 3
    > (+ x x)
    2 (+) 4 (+) 6
    > (* 3 x)
    3 (+) 6 (+) 9

As with direct sum domains, the dimension of an element of a direct sum 
domain can be computed using dimension, and individual components can be 
determined using ref.
::

    > (dimension x)
    3
    > (loop for i below (dimension x)
          do (format t "~%Component ~D: ~S, domain: ~S"
                     i (ref x i) (domain-of (ref x i))))

    Component 0: 1, domain R
    Component 1: 2, domain Q
    Component 2: 3, domain Z


Free Modules (7.2)
------------------

M is a free R-module if M is both a free abelian group and an R-module. In Weyl, 
elements of free modules are represented as n-tuples of elements of the 
coefficient domain R. Thus, we are only able to deal with finite dimensional 
free modules. If n is the rank of M as a free abelian group, then as a 
free R-module, M is isomorphic to the direct sum of n copies of R. 
Closely related to the concept of a free module is that of a vector space. 
A vector space is a free module whose coefficient domain is a field. Additional 
information about operations on elements of vector spaces can be found in 
Section 8.1.

The basic routine for creating a free module is get-free-module.

.. function:: get-free-module domain rank  [Function]

   Creates a free module of dimension rank where the elements' components are 
   all elements of domain. domain must be a ring. If domain is a field the 
   domain returned will be a vector space.
   
If one expects the coefficient domain to be a field, and thus the affine module 
will actually be a vector space, then the routine get-vector-space should be 
used instead of get-free-module. This routine explicitly checks that the 
coefficient domain is a field and signals an error if it is not a field.

Once a free R-module has been created, it is often useful to refer to the 
domain R itself. This can be done using the routine coefficient-domain-of. 
The dimension of the free module can be obtained using dimension.

.. function:: coefficient-domain-of domain   [Function]

   Returns the domain of he coefficients of domain.

.. function:: dimension domain    [Function]

   This method is de ned for free modules but the value returned is not specified. 
   (Actually it should be infinity.)

Elements of a free module can be created using the function make-element. 
The routine make-point calls dimensions, so that code that uses free modules as 
vector spaces can be written more euphoniously.

.. function:: make-element domain value &rest values  [Function]

   Make an element of the module domain, whose rst component is value, etc. 
   If value is a Lisp vector or one dimensional array, then elements of that 
   array are used as the components of the free module element.

Use of these routines is illustrated below [Add something here {RZ]

.. function:: ref vector i                 [Function]

   Returns the i-th element (zero based) of vector.       

.. function:: inner-product u v     [Function]

   Computes the inner (dot product) of the two vectors u and v. 
   If :math:`u = (u_1 ... u_k)` and :math:`v = (v_1 ... v_k)` then
   :math:`(u,v) = u_1 v_1 + u_2 v_2 + ... + u_k v_k`.
  
  
Tensor Products (7.3)
---------------------
tbw

Rings of Fractions (7.4)
------------------------
Given an arbitrary ring R, we can construct a new ring, called the quotient 
ring of R, whose elements are pairs of elements in R subject to the following 
equivalence relation. If (a, b) and (c, d) are elements of a quotient ring, 
then they are equal if and only if ad = bc. The sum and product of two elements 
of the quotient ring are de ned as follows

.. math::
                \begin{eqnarray}
                (a, b) + (c, d) & = & (a d + b c, bd), \\
                (a, b) \times (c, d) & = & (ac, bd).
                \end{eqnarray}

If the ring R is an integral domain then the quotient ring is actually a field.

More generally, let S be a multiplicatively closed subset of R. The 
localization of R with respect to a multiplicative subset of R, S , written 
S^(-1) R, is the ring of pairs (a, s), where a is an element
of R and s is an element of S .

.. function:: make-ring-of-fractions domain &optional (multiplicative-set domain) [Function]

   Constructs a quotient ring from domain. If a second argument is provided, 
   then this make-quotient-ring returns a ring representing the localization of 
   domain with respect to multiplicative-set. Otherwise, the quotient ring of 
   domain is returned. If domain is an integral domain, then the ring returned 
   will be a field.
   
.. function:: make-quotient-field domain   [Function]

   This generic function constructs the quotient eld of domain . The domain 
   returned by this operation will be a field. If domain is itself a field, then 
   it will be returned without any modification. If domain is a gcd domain then 
   operations with the elements of the resulting quotient field will reduce 
   their answers to lowest terms by dividing out the common gcd of the resulting 
   numerator and divisor.

Two special cases are handled specially by make-quotient. If the argument domain 
is either the rational integers or a polynomial ring then special domains of 
the rational numbers or rational functions are used. This is for efficiency 
reasons. Hopefully, a more general solution can be found in the near future.

Two operations can be used to create elements of a quotient field: 
make-quotient-element and quotient-reduce. The operation make-quotient-element 
creates a quotient element from the numerator and denominator domains. 
Quotient-reduce does the same thing but removes the common GCD
from the numerator and denominator rst.
(Ed: Need to think about things like localizations here.)

.. function:: get-quotient-field ring  [Function]

   Returns a field which is the quotient field of ring . In some cases, this 
   is special cased to return return something more efficient than the general 
   quotient field objects.
   
.. function:: weyli::qf-ring qf      [Function]

   Returns the ring from which the quotient eld qf was built.
   
.. function:: numerator q                          [Function]

   Returns the numerator of q .

.. function:: denominator q                  [Function]

   Returns the denominator of q .

.. function:: quotient-reduce domain numerator &optional denominator [Function]

   Numerator and denominator are assumed to be elements of base ring of domain . 
   quotient-reduce creates and quotient element in domain from numerator and 
   denominator . If denominator is not provided, the multiplicative unit of 
   domain is used.

.. function:: with-numerator-and-denominator (num den) q &body body [Function]

   Creates a new lexical environment where the variables num and den are bound 
   to the numerator and denominator of q .
   
   
Factor Domains (7.5)
--------------------
Let B be a subgroup of A. One can divide the elements of A into equivalence 
classes as follows: Two elements of A are in the same equivalence class if their quotient is an element of B . The
equivalence classes of A with respect to B form a group, called the factor 
group of A by B . Using this construction one can form factor modules of one 
module by a submodule. If A is a ring and B is an ideal of A, then the factor 
module of A by B is a ring, called the factor ring of A by B . Weyl provides 
four classes for dealing with factor domains, as shown in Figure 7.1. The factor
domain (group, module or ring) of A and B is written A=B . The components of 
a factor domain may be accessed using the following routines.

.. function:: factor-numer-of factor-domain  [Function]

   Returns the numerator of a factor domain.

.. function:: factor-denom-of factor-domain   [Function]

   Returns the \denominator of a factor domain.


