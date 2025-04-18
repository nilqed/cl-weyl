.. (ql:quickload :weyl)
.. (in-package :weyl)

Truncated Power Series
======================
Formal power series are in nite degree polynomials, where the arithmetic 
operations are de ned formally using the same rules as a polynomial arithmetic. 
Thus

.. math::

  f(x) = \sum_{0\leq i} a_i\,x^i = a_0 + a_1 x + a_2 x^2 + \ldots,
  
is a power series, and we have

.. math::

  (\sum_{0\leq i} a_i\,x^i) (\sum_{0\leq i} b_i\,x^i)  =
        \sum_{0\leq i} (\sum_{0\leq j \leq i} a_i\,b_{i-j}) \,x^i.

Some operations are possible with formal power series that are not possible with 
polynomials. For instance, if the leading coefficient of a formal power series 
is invertible then the formal power series has a reciprocal.

Rings of formal power series are denoted using a similar notation to that used 
for polynomials except that the brackets are doubled. Thus, Z[[x]] and Q[t][[x]] 
represent rings of formal power series in x whose coefficients are rational 
integers and polynomials with rational number coefficients.

The quotient field of a ring of formal power series consists of formal power 
series whose leading term can have negative order. For example, we have

.. math::

    (x+x^2+\ldots)^{-1} = \frac{1}{x}-1+\ldots.
    
The field of formal power series is denoted using doubled parenthesis similar 
adaptation to the notation for the eld of rational functions. Thus, R((x)) is 
the field of formal power series with real number coefficients.

In mathematics one also deals with regular power series, which are formal power 
series that are convergent in some sense. Thus, while

.. math::

     1+ 2 x + 3! x^2 + 4! x^3 + \ldots,
     
is a perfectly reasonable formal power series, but it is not convergent for any 
value of x. When one represents physical quantities, convergent power series 
are usually more interesting, since they can actually be evaluated. However, 
arithmetic operations with power series may shrink their radius of convergence, 
and keeping track of the radius of convergence throughout the computation can 
be quite difficult. Instead, it is often more convenient to deal with formal 
power series and leave the convergence issues to later.

While Weyl does not have a representation for the power series 
(either convergent or formal), it does provide tools for manipulating truncated 
power series. A truncated power series only contains those monomials of a formal 
power series that have degree less than some bound. Thus, while the function 
sin x has the infinite power series expansion

.. math::

   sin x = x - \frac{x^3}{6} + \frac{x^5}{120} - \frac{x^7}{5040}+\ldots.
   
its truncated power series expansion is

::

     > (setq sinx (taylor (sin qx) domain 5))
     x - (1/6)x^3 + (1/120)x^5 - (1/5040)x^7 + o(x^5)

The small "o" notation indicates that all terms of order greater than 5 have 
been ignored. The argument domain is some domain of truncated power series.

The following sections describe the tools provided by Weyl for computing with 
formal power series.

Creating Truncated Power Series Domains (11.1)
----------------------------------------------
Truncate power series rings and elds can be created using the function 
get-tpower-series.

.. function get-tpower-series-domain coefficient-domain variable  [Function]

   Coefficient-domain must be a ring. If the coefficient-domain is also a 
   field, the constructed truncated power series domain will be a field. 
   variable should be coercible into a general expression variable. (Notice 
   that this is somewhat more restricted than what is permissible for 
   polynomials. At some point in the future this restriction will be lifted.) 
   The variable indicated will be the generator of the truncated power series.
   
The following form creates the ring of power series with rational integer 
coefficients

.. code-block:: lisp

    (weyli::get-tpower-series-domain (get-rational-integers) 'x)
    => Z[[x]]
    
Notice that this domain is actually a ring and not a field is indicated by 
the square brackets. When a power series domain is created over a field, 
round brackets are used:

.. code-block:: lisp

    (defvar  domain (get-tpower-series-domain (get-rational-numbers) 'x))
    => DOMAIN

    domain
    => Q((x))
    
    (defvar  sinx (taylor (sin 'x) domain 5))
    => SINX

    sinx
    => x - (1/6)x^3 + (1/120)x^5 + o(x^5)


Truncated Power Series Operators (11.2)
---------------------------------------
The simplest way to actually create a truncated power series is to use the 
Taylor series expansion function:

.. function:: taylor expr tps-domain order   [Function]

   Returns the power expansion of the general expression expr as a truncated 
   power series of order order in the domain tps-domain. The variable of 
   expansion will be the power series variable of tps-domain.
   
For instance, to compute the power series expansion of sin x one could type

.. code-block:: lisp

    (setq sinx (taylor (sin 'x) domain 8))
    => x - (1/6)x^3 + (1/120)x^5 - (1/5040)x^7 + o(x^8)

The taylor function also works with more complex functions.

.. code-block:: lisp

    (defvar  temp1 (taylor (cos (sin 'x)) domain  5))
    => TEMP1

    temp1
    => 1 - (1/2)x^2 + (5/24)x^4 + o(x^5)

    (defvar  temp2 (taylor (sin (sin 'x)) domain 5))
    => TEMP2

    temp2
    => x - (1/3)x^3 + (1/10)x^5 + o(x^5)

Arithmetic operations can also be performed with power series expressions:

.. code-block:: lisp

    (+ (* temp1 temp1) (expt temp2 2))
    => 1 + o(x^5) 

Although taylor returns a truncated power series approximation to the power 
series expansion of the expression, if the coefficient arithmetic is exact, 
then every coefficient returned will be exact. So, in this case up to order 5, 
the only non-zero term in the power series expansion is 1. Truncated power 
series can be manipulated with the usual arithmetic operations. For binary
operations, the order of the result may be reduced to ensure that only the 
known coefficients are advertised as known. For instance, although temp2 has 
order 5 its sum with a truncated power series of order 3 is only of order 3.

.. code-block::

    (+ temp2 (taylor (sin 'x) domain 3))
    => 2x - (1/2)x^3 + o(x^3)

Notice also, the even unary operations can yield results with different 
orders, viz.,

.. code-block:: lisp

    (* temp2 temp2)
    => x^2 - (2/3)x^4 + (14/45)x^6 + o(x^6)

    (expt temp2 -3)
    => x^(-3) + x^(-1) + (11/30)x + o(x)

Nonetheless, when the taylor function is requested to compute the power series 
expansion to a given order, it computes the internal subexpressions to a 
sufficiently high order, to ensure that the correct coefficients are computed

.. code-block:: lisp

    (expt (- temp1 1) -3)
    => (-8)x^(-6) - 10x^(-4) + o(x^(-3))
   
    (taylor (expt (- (cos (sin 'x)) 1) -3) domain 5)
    => (-8)x^(-6) - 10x^(-4) - (88/15)x^(-2) - 16783/7560 - (32267/50400)x^2 - 
           (1045753/6652800)x^4 + o(x^5)

The power series package can also deal with power series with fractional 
exponents. For instance,

.. code-block:: lisp

    (expt (taylor (sin 'x) domain  5) (- (/ 1 3)))
    => x^(-1/3) + (1/18)x^(5/3) + (11/3240)x^(11/3) + o(x^(11/3))
    
One power series can be substituted into another using the substitute function. 
In addition, derivatives of power series can also be computed using deriv.

.. function:: substitute value var tps   [Function]
 
   Returns the truncated power series computed by substituting the truncated 
   power series value for the ring variable in tps. The general expression 
   variable var should be equal to the ring variable of the domain of tps.
   
.. function:: deriv tps &rest vars    [Function]

   The derivative of the tps with respect to each variable in vars is computed 
   successively and the final truncated power series is returned.

For instance, another way to compute the first few terms of the power series 
of sin sin x is to substitute the power series of sin x into itself

.. code-block:: lisp

    (substitute (taylor (sin 'x) domain  5) 'x (taylor (sin 'x) domain  5))
    => x - (1/3)x^3 + (1/10)x^5 + o(x^5)

As mentioned, before the order of the resulting power series expansion may be 
less than the order of arguments.

.. code-block:: lisp

    (deriv (substitute (taylor (sin 'x) domain  5) 'x (taylor (sin 'x) domain  5))'x)
    => 1 - x^2 + (1/2)x^4 + o(x^4)
    
One of the operations that can be performed with power series that cannot be 
performed with polynomials is the reversion. That is, given a power series 
for :math:`f(x)`, we can compute the power series for :math:`f^{-1} (x)`. 
This computation  is performed by the function revert

.. function:: revert tps      [Function]

   Returns the truncated power series that is the reversion of tps.
   
For instance, given the power series expansion for sin x

.. code-block::lisp

    (setq sinx (taylor (sin 'x) domain 10))
    => x - (1/6)x^3 + (1/120)x^5 - (1/5040)x^7 + (1/362880)x^9 + o(x^10)

we can compute the reversion of sin x as follows:

.. code-block:: lisp

    (reversion sinx)
    => x + (1/6)x^3 + (3/40)x^5 + (5/112)x^7 + (35/1152)x^9 + o(x^10)

If we substitute the reversion of sinx obtained above for x in sinx, we 
should get x, as expected:

.. code-block:: lisp

    (substitute (reversion sinx) 'x sinx)
    => x + o(x^10)


.. function:: solve-diff-eqn diff-eqn coef-ring var order init-list  [Function]

   Solves the differential equation diff-eqn given in the form of a general 
   expression. The solution is given as a truncated power series of order order 
   with coeffcients in the coef-ring with var taken as the ring variable. The 
   init-list is the list of initial values of length equal to the order of the 
   diff-eqn.

The following examples illustrate how to obtain power series solutions to 
ordinary differential equations in one variable:


.. code-block:: lisp

    (defvar  y (funct y 'x))
    => Y

    (defvar y0 (deriv y 'x))
    => Y0

    (defvar y00 (deriv y 'x 'x)))
    => Y00

    (defvar diff-eqn1 (+ y y0 (* -1 'x 'x)))
    => DIFF-EQN1

    diff-eqn1
    => y(x) + y_{0}(x) - x^2
    

    ; (solve-diff-eqn diff-eqn1 (get-rational-numbers) 'x 6 '(1))
    ; undefined ???
    
    is #+ignore !

    (setq diff-eqn2 (- y00 y))
    (solve-diff-eqn diff-eqn2 (get-rational-numbers) 'x 5 '(c0 c1))

Truncated Power Series Internals (11.3)
---------------------------------------
This section describes the internal representation used to implement truncated 
power series and is only of interest to those who want to extend the package.
Truncated power series are currently represented by a single Lisp class, 
weyli::tpower-series, which is a representation in which all of the coefficients
are enumerated in an array. Generative representations are not currently 
available.


Enumerated Truncate Power Series (11.3.1)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
This class, currently the only representation for power series, maintains an 
array of known coefficients and is therefore useful only for power series with 
a finite and typically small number of nonzero coefficients. The power series 
may have an in nite order (in which case the behavior is nearly identical to 
that of polynomials) but in such cases the largest nonzero term must have a
finite exponent. This class will perform best when used with dense power series
(i.e., power series in which the sequences of exponent numerators of nonzero 
terms do not have large skips). 

Truncated power series in Weyl are a little 
more complex than the proceeding discussion might indicate. In particular, we 
permit truncated power series to have negative and fractional exponents in 
order to allow for certain algebraic operations on power series. For instance, 
we would like to represent

..math::

   \sqrt{x+x^2} = x^{\tfrac{1}{2}} - \frac{x^\tfrac{3}{2}}{2}+
        \frac{x^\tfrac{5}{2}}{8} + \ldots.
        
and the reciprocal of the function

.. math::

  \frac{1}{\sqrt{x+x^2}} = x^{-\tfrac{1}{2}} - \frac{x^\tfrac{1}{2}}{2}+
        \frac{3 x^\tfrac{3}{2}}{8} - \frac{5 x^\tfrac{5}{2}}{16}  + \ldots.
   

To deal with these issues the truncated power series representation in Weyl has 
four components. The most obvious is the vector with coefficients of the power 
series. The branch order of the power series is the least common multiple of 
the denominators of the exponents that arise in the power series. Thus in the 
pervious two examples the branch order is 2. 

The valence of the power series 
is the degree of the term with nonzero coefficient with least degree when 
multiplied by the branch order. In the previous two examples, the valences 
are 1 and -1 respectively.

Finally, the product of the branch order and the degree of the highest term 
retained in the truncated power series is called the order of the power series. 
Notice that these definitions have been chosen so that all of the parameters 
are integers. The following routines can be used to access the parameters of 
a truncated power series.

.. function:: branch-order tps   [Function]

   Returns the branching order of tps as a lisp integer. This number is the 
   greatest common divisor of all of the denominators of the exponents in 
   the truncated power series.
   
   
.. function:: order tps       [Function]

   Returns the order of tps as a Lisp integer. This number is the product of 
   the greatest exponent in the truncated power series and the branch order 
   of the power series.


.. function:: valence tps      [Function]

   Returns the valence of tps as a Lisp integer. This number is the product of 
   the least exponent in the truncated power series and the branch order of 
   the power series.
   
The following examples illustrate the use of these routines:

.. code-block:: lisp

    (defvar temp)
    => TEMP
    (setq temp (taylor (expt (+ 'x (* 'x 'x)) -1/2) domain  2))
    => x^(-1/2) - (1/2)x^(1/2) + (3/8)x^(3/2) - (5/16)x^(5/2) + o(x^(5/2))

`temp` is the power series of the expression given above. It has both negative 
and fraction exponents.

.. code-block::

    (describe temp)
    #1=x^(-1/2) - (1/2)#1#^(1/2) + (3/8)#1#^(3/2) - (5/16)#1#^(5/2) + o(#1..
      [standard-object]

    Slots with :INSTANCE allocation:
      DOMAIN                         = Q((x))
      VALENCE                        = -1
      BRANCH-ORDER                   = 2
      ORDER                          = 5
      COEFFS                         = #(1 0 -1/2 0 3/8 0 -5/16)
    *

Notice that the branch order, valence and order are all rational integers.
T
he branch order of the power series can be modified by using setf and the 
branch-order accessor. This operation increases the size of the coefficient 
array of the power series and adjusts the order and valence appropriately. 
This operation is used internally to before combining two power series
with different branching orders.

.. code-block:: lisp

    (setf (weyli::branch-order temp) 4)
    => x^(-1/2) - (1/2)x^(1/2) + (3/8)x^(3/2) - (5/16)x^(5/2) + o(x^(5/2))

Notice that the outward appearance of the power series has not changed, even 
though the branching order has been increased. However, the internal form has 
changed.

When writing low level code using truncated power series, it is more convenient 
not to use the accessors for the slots in a truncated power series, but instead 
to have variables bound to the various pieces. This is done by the following 
special form.

.. function:: weyli::with-tpower-series ((var1 tps1 ) ... (var1 tps1 )) &body body [Function]

   For each truncated power series provided, this form binds four new variables, 
   with names based on vari , one for each of the slots in tpsi . The names of 
   these slots are vari -bo for the branching order, vari -val for the valence, 
   vari -order for the order and vari -coeffs for the coefficient vector.

Similarly, the following function need not be here but should be a setf method 
on order

.. function:: weyli::truncate-order tps integer     [Function]

   Truncates the order of tps so that the largest exponent is no greater than 
   integer. If integer is greater than or equal to the current truncation order, 
   then tps is returned unaltered.


