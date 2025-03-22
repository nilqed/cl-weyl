.. (ql:quickload :weyl)
.. (in-package :weyl)


Linear Spaces
=============
The general approach in Weyl is to use functors to create more complex domains 
from simple domains. The simplest such functors are those that takes a ring or
field R and produces a vector space whose elements lie in R. This chapter 
discusses these domains and the operations that apply to their elements. The 
simplest of these domains are free modules and vector spaces. Both are
modules (you can add their elements and multiply them by elements of R). 
The difference is that elements of a vector space can also be divided by 
elements of R, i.e., R is a field. Free modules and vector spaces are discussed
in Section 7.2 and Section 8.1 respectively.

Vector Spaces (8.1)
-------------------
Vector spaces are free modules where the coefficient domain is a field.

.. function:: get-vector-space domain dim   [Function]

   Create a vector space of dimension dim where the vectors' components are 
   all elements of domain. Domain must be a field.
   
.. code-block:: lisp

   * (get-vector-space (get-rational-numbers) 4)
   => Q^4
   * (get-vector-space (get-real-numbers) 3)
   => R^3
   *
   
   * (dimension-of (get-vector-space (get-real-numbers) 3))
   => 3
   
General Matrices (8.2)
----------------------

.. function:: get-matrix-space ring  [Function]

   Returns a general matrix domain, where the entries in the matrices are 
   elements of ring .

.. code-block:: lisp

   (defvar M (get-matrix-space (get-rational-integers)))
   => M
   
   m
   ==> Mat(Z)

   

.. function:: make-element domain value &rest values    [Function]

   Make an element of the module domain, where the first component is value, etc.

.. code-block:: lisp

    * (defparameter ma  (make-array '(3 2) :initial-element 1))
    MA
    * (make-element m ma)
    Mat<<1,  1>,  <1,  1>,  <1,  1>>
    *

    (defparameter aa  #2A((1 2 3) (4 5 6)))
    AA
    * (make-element m aa)
    Mat<<1,  2,  3>,  <4,  5,  6>>

    (defparameter aaa  #2A((1 2 3) (4 5 6) (3 4 9)))
    AAA
    * (defvar m3 (make-element m aaa))
    M3
    *

The following two functions return square matrices whose off diagonal elements 
are all zero. Its diagonal elements are 0 for zero-matrix and 1 for one-matrix.

.. function:: zero-matrix matrix-domain &optional rank      [Function]

   The zero matrix.
   
.. code-block:: lisp

   * (zero-matrix m 3)
   Mat<<0,  0,  0>,  <0,  0,  0>,  <0,  0,  0>>
   *
 
.. function:: one-matrix matrix-domain &optional rank       [Function]

   The one matrix (identity).
   
.. code-block:: lisp

    (one-matrix m 4)
    Mat<<1,  0,  0,  0>,  <0,  1,  0,  0>,  <0,  0,  1,  0>,  <0,  0,  0,  1>>
    *   

.. function:: matrix-dimensions matrix    [Function]
   
   Returns the dimensions of this particular matrix.

.. code-block:: lisp

    * (matrix-dimensions  m3)
    3
    3
    

.. function:: ref vector i j   [Function]

   Returns the (i, j ) element (zero based) of vector . If i or j is :*, 
   then a row or column vector is returned.
   
.. code-block:: lisp

    * (ref m3 1 2)
    6

    * (ref m3 0 0)
    1
   
    * (ref m3 0 1)
    2
    *

.. function:: with-matrix-dimensions (dim1 dim2 &optional array) matrix &body body [Function]

   This control abstraction binds dim1 and dim2 to the dimensions of matrix. 
   If array is provided, it is bound to an array that whose elements are the 
   elements of the matrix . This control structure can lead to substantially 
   more efficient manipulation of dense matrices.
   
   

Ultimately, array will turn into an efficient accessor for the elements of 
matrix so that this technique will work with other than dense matrices.

.. function:: transpose matrix  [Function]

   Transpose matrix.

.. code-block:: lisp

    * (transpose  m3)
    Mat<<1,  4,  3>,  <2,  5,  4>,  <3,  6,  9>>
    *


.. function:: jacobian function-list var-list     [Function]

   Returns the Jacobian of function-list with respect to vector-list. 
   These should really be vectors, but list will work also.


.. function:: direct-sum &rest matrices   [Function]

   Assumes the matrices all have the same number of rows, n. It returns a 
   matrix of n rows, where each row is the concatenation of the rows of each 
   of its arguments.


.. function:: determinant matrix

   Returns the determinant of the matrix.[Function]

.. code-block:: lisp

    not working ...


.. function:: sparse-determinant matrix     [Function]

.. code-block:: lisp

    * (weyli::sparse-determinant  m3)
    -12

   Returns the determinant of the matrix.

.. function:: subdeterminant matrix    [Function]

   Returns the determinant of a nonsingular submatrix of the matrix of 
   maximum rank.


.. function:: hermite matrix   [Function]

   Returns the Hermite normal form of the matrix. At the moment, the method 
   is implemented only for matrices over the ring of integers.


.. function:: smith matrix  [Function]

   Returns the vector of diagonal elements of the Smith normal form of the 
   matrix. At the moment, the method is implemented only for matrices over 
   the ring of integers.

Matrix Groups (8.3)
-------------------
tbw.


