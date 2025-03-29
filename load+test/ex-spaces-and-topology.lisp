(ql:quickload :weyl)
(in-package :weyl)

#|
Spaces and Topology
===================
Spaces are sets of mathematical objects (domains in Weyl's terminology) that we 
endow with some topological structure. Examples of spaces include n-dimensional 
Euclidean space, the space of square integrable functions and the spectrum of 
a ring. In addition, we often provide a metric to provide the geometric 
structure of a space. This chapter describes the tools Weyl provides for 
creating and manipulating these spaces. 

The basic domains Weyl provides for spaces are shown in Figure 12.1. 
All spaces are subclasses of abstract-space. It would be very useful to be 
able to indicate that a space is Hausdorff, connected, compact or has some 
similar topological property. This is not generally possible at the moment, 
but will be added at some future date.

One property that can be attached to a space is its dimension. Intuitively, the 
dimension of a space can be thought of as the number of orthogonal directions 
that can be taken in the space. However, this does not apply to spaces that 
are not embedded in some Euclidean space.Mathematically, the dimension of a 
topological space is either a non-negative integer or infinity.1For Weyl's 
purposes, it is useful to, in addition, have spaces without a speci c dimension. 
A dimensional-space is a space that has a specific, possibly in infinite, 
dimension.

A very useful, specific type of dimensional space is a Euclidean space. A 
Euclidean space is :math:`\mathbb{R}^n` endowed with the Euclidean metric for 
measuring  distances between points. The open sets of a Euclidean space can be 
constructed  via set union from the open balls :math:`B_d(p)`, the set of 
points of distance less than d from the point p. The open sets are closed 
under union and finite intersection. The closed sets are the complements of 
the open sets.


Closely related to Euclidean spaces are general vector spaces and modules. 
Recall from Section 4.5.3, that M is an R-module if M is an abelian group and 
if multiplication by elements of R is closed in M . A free module is a module 
that is freely generated. As can be seen from Figure 12.1, Weyl only permits 
free modules that have speci ed dimensions. A vector space is a free R-module
where R is a field. From Figure 12.1 we see that Weyl treats a Euclidean space 
as a vector space, where the coefficient domain is the real numbers, R. The 
routines for dealing with free modules are described in Section 7.2.

Finally, Weyl provides a mechanism for dealing with projective spaces. A 
projective space is an n + 1 dimensional space together with an equivalence 
relation that equates points that are nonzero scalar multiples of each other, 
i.e., u ~= v if u = a*v , for a \= 0. Due to the equivalence relation, the
projective space has dimension n.

The actual dimensions of a space can be determined by the function dimensions.

.. function:: dimensions space [Function]

   Returns a list of the dimensions of the space. For free-modules, vector 
   spaces, projective spaces, and so on there is only one element in this list. 
   However for spaces like R3xZ5 there may be more elements.
   

Point Set Topology (12.1)
-------------------------
Objects in topologies are subsets of spaces. The root class of all spaces is 
abstract-space. Abstract spaces do not have a dimension and consist only of 
points. To create an abstract space one must explicitly call make-instance on 
the class abstract-space. The most commonly used spaces, the Euclidean spaces, 
can be created using the function get-euclidean-space:


.. function:: get-euclidean-space dimension &optional (domain *general*) [Function]

   Creates, if needed, an instance of the Euclidean space with the indicated 
   dimension. Elements of the Euclidean space will be represented as n-tuples. 
   The components of these n-tuples will be elements of domain.
   
.. code-block:: lisp

    (defvar E4 (get-euclidean-space 4))
    => E4

    E4
    => E^4

    (dimension-of E4)
    => 4

    (dimensions  E4)
    => (4)



To create points in a space we use the function make-point:


.. function:: make-point space coord1 &rest coords   [Function]

   Creates a point in space. If space is an abstract space, then only one 
   argument is expected and will be treated as a unique identifier for the 
   point in the space. If the first coordinate is nil then a new anonymous 
   point will be created. For vector spaces, the values are expected to be 
   the coordinates of the point.

.. code-block:: lisp

    (make-point E4 1 2 3 4)
    => #P<1, 2, 3, 4>

    (make-point E4 'a 'b 'c 'd)
    => #P<a, b, c, d>


All points in a space are identified by a unique integer identifier. This is 
currently implemented (using the class weyli::has-id-number) by including a 
slot in each point containing an integer unique to that point. When anonymous 
points are generated in an abstract space, the printed representation is 
distinguished by this number. For some spaces, e.g., R, it may be more 
appropriate to use a different ordering.

.. code-block:: lisp

    (defvar abs-space)
    (setq abs-space (make-instance 'abstract-space))
    => #<Domain: ABSTRACT-SPACE>

    (progn
       (setq a (make-point abs-space nil)
             b (make-point abs-space nil)
             c (make-point abs-space 'c))
       (list a b c))

    => (<1> <2> <C>)


Notice that the third point was created with a name and, unlike the other 
two points, its printed form includes that name.
Similarly, we can create points in Euclidean domains. In this case, the 
printed representation of a point includes the point's coordinates.

Affine Spaces (12.2)
--------------------
Affine spaces are created using the function make-affne-space:

.. function:: make-affine-space eld dimension        [Function]

   Create an affine space of dimensions dimension where the components are 
   elements of the field field.

.. code-block:: lisp

    * (defvar  R (get-real-numbers))
    R

    (defvar A2  (weyli::make-affine-space R 2 ))
    =>  There is no applicable method for the generic function

    * (describe 'weyli::make-affine-space)
    WEYLI::MAKE-AFFINE-SPACE
      [symbol]

    MAKE-AFFINE-SPACE names a generic function:
      Lambda-list: (SPACE &OPTIONAL N)
      Derived type: (FUNCTION (T &OPTIONAL T) *)
      Method-combination: STANDARD
      Methods:
        (MAKE-AFFINE-SPACE (PROJECTIVE-SPACE))
    *

Elements of spaces, both affine and projective, are created using the generic 
function make-point:

.. function:: make-element space &rest elements  [Function]

   Creates an element of space. Elements is a list of n elements which can 
   be coerced into the coefficient domain of space.
   
There is also an internal function weyli::make-element that does not do any 
checking of its arguments and can lead to rather subtle problems if used 
incorrectly. On the other hand it is noticeably faster.


.. function:: cross-product u v    [Function]

   This function is only defined for elements of three dimensional vector 
   spaces. If u = (u1, u2, u3) and v = (v1, v2, v3) then
  (cross-product uv ) = (v2 u3 - u3 v2 , u3 u1 - u1 v3 , u1 v2 - u2 v1)..
  


Projective Spaces (12.3)
------------------------

.. function:: make-projective-space eld dimension   [Function]

   Create a projective space of dimensions dimension where the components 
   are elements of the field field.
   
As in the case of regular affine spaces, elements of a projective space are 
created using the generic function make-point.

.. function:: make-point space &rest elements   [Function]

   Creates a point which an element of space. For projective spaces, elements 
   can be a list of either n or n + 1 elements of the coefficient domain of 
   space. If n + 1 elements are provided then these are the full set of 
   elements of the point. If only n elements are provided, the nal missing 
   element is lled out by a 1 from the coefficient domain of space.
   
.. code-block:: lisp

    (defvar  A2 (weyli::make-projective-space R 2))
    => A2

    (make-point A2 1 1 1)
    => There is no applicable method for the generic function
    

Affine spaces can be embedded in projective spaces. For projective spaces of 
dimension n, there are n + 1 canonical embeddings. The function 
make-affine-projection is passed a projective space and creates an affine space 
with an attached homomorphism into the projective space.

.. function:: make-affine-projection space &optional dimension     [Function]

   This function returns an affine space that is the projection of space where 
   we hold the component dimension fixed. For instance, let p = (u; v; w) is 
   an element of a two dimension projective space P 2 . The image of p in the 
   affine projection of P 2 with dimension 0 held fixed is (v/u, w/u). With 
   dimension 1 is held fixed then p maps to (u/v, w/v ). Finally, if dimension 
   2 is held fixed, p maps to (u/w, v/w). If dimension is not provided then 
   we produce an affine space where the last dimension is held fixed.
   
Algebraic Topology (12.4)
-------------------------

Cells and Simplices (12.4.1)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
A k-cell is a region (subset) of a space that is homeomorphic to B_n, the unit 
ball in Rn : B_n = {p in Rn: |p|<=1} That is, a k-cell is a set that is 
topologically equivalent to an n-dimensional ball (it is connected, without 
any holes, etc.).


A k-simplex is a k-cell de ned by the convex hull of k + 1 points, called 
vertices. For example, a 1-simplex is a line segment de ned by two vertices, 
while a 2-simplex is a triangle, defined by three. An oriented k-simplex is 
defined by an ordered list of vertices, and the lists are partitioned
into two orientations those that are even permutations of some reference 
ordering, and those that are odd permutations.

Unlike most objects in Weyl, (oriented) simplices are not domain elements. 
They should be viewed as sorted lists of sets of points.

.. function:: make-simplex vertex0 ... vertexk   [Function]

Creates a k simplex whose components are vertex0 ... vertexk . This routine 
ensures that all of the points are from the same space. Simplices are 
immutable, i.e., once created they can not be modified.

Assuming that the variables a, b and c are points in some abstract space, 
a triangle could be created as follows:
::

    > (setq triangle (make-simplex a b c))
    [<A>, <B>, <C>]

Two simplices are equal if they have the same vertex set, with the same 
orientation.
::

    > (= triangle (make-simplex a b c))
    t


.. function:: vertices-of simplex    [Function]

   Returns a list of the vertices of simplex. For instance,

::

    > (vertices-of triangle)
    (<A> <B> <C>)

Notice that there aren't any commas between the vertices. This is a LISP list 
as opposed to a simplex.

.. function:: face? cell1 cell2     [Function]

   A predicate that returns T if cell1 is a face of cell2. Cells are defined 
   to be faces of themselves.


.. function:: same-cell? cell1 cell2                 [Function]

   A predicate that returns T if cell1 and cell2 are the same cell, independent of orientation.


.. function:: dimension-of cell                   [Function]

   Returns a Lisp integer that is the dimension of cell. Weyl provides a 
   dimension function only for simplices.
   
   
Complexes (12.4.2)
^^^^^^^^^^^^^^^^^^
A simplicial complex K is a set of simplices of the same space with the 
property that if s 2 K ! faces(s) \subset K , that is if a simplex s is in 
the complex, the all of the faces of s must also be in K . It follows that 
there is a set of maximal simplices (those that are not the face of any other 
simplex in K ) that provide a unique minimal representation for K . Although 
the maximal cells in a complex often have the same dimension, this is not 
required.







[1] By using the Hausdorff dimension for the dimension of a space, we can 
construct spaces that have non-integral dimension.


|#