

Generic Tools
=============

Some facilities needed by symbolic computing routines are more generally useful. 
For instance, it is occasionally necessary to perform some function for every 
permutation of the elements of a set. To capture this idiom, Weyl provides a 
new control structure, called permute. This control structure, and other 
combinatorial control structures, are described in Section 3.1. Other data 
and control structures that are not directly tied to mathematical computation 
are given in the other sections.

Combinatorial Tools (3.1)
-------------------------

Weyl provides control abstractions that are often useful for computations 
that involve combinatorial enumeration and searching. These control structures 
are permute and choose, which enumerate all permutations of a sequence and 
all subsets of a fixed size of a sequence. 

.. function:: permute sequence (var . options) &body body  [Special Form]

   sequence is a sequence of elements. The variable var is repeatedly bound to 
   the different permutations of sequence, and body is evaluated each time. 
   The options are provided to specify even and odd permutations, but are not at 
   this point implemented.

For example, the following code will print all permutations of the list 
(a b c) and count their number:
::

    > (let ((count 0))
    (permute ' (a b c) (p)
    (print p)
    (incf count))
    (format t "~%~D permutations total. ~%" count))
    (C B A)
    (B C A)
    (C A B)
    (A C B)
    (B A C)
    (A B C)
    6 permutations total.
    NIL


.. function:: choose set (var n . options) &body body   [Special Form]

   While the rst argument to permute must be ordered, the rst argument to 
   choose need only be a set (but again, only lists are currently implemented). 
   The variable var is bound to each subset of set that has precisely n 
   elements and body is evaluated in each case. 
   At the moment no options are permitted.


A partition of a positive integer n is a representation of n as a sum of 
positive integers. The following control structure is used to enumerate 
partitions.

.. function:: partition (var n . options) &body body  [Special Form]

   The argument n is assumed to be an integer. This control structure 
   repeatedly binds var to additive partitions of n and then evaluates 
   the body. The options allow one to control which partitions of n are 
   produced. The options are

::

     :number-of-parts  The number of parts each partition is allowed to contain.
     :minimum-part     The minimum value each for each of the components of the
                       partition.
     :maximum-part     The maximum value each for each of the components of the
                       partition.
     :distinct?        If specified as T then each of the components of the partition
                       must be distinct.

     Partition returns no values.


The following examples illustrate the use of the partition control structure. 
First, compute all of the partitions of 6:
::

    > (partition (l 6) (print l))
    (1 1 1 1 1 1)
    (2 1 1 1 1)
    (3 1 1 1)
    (2 2 1 1)
    (4 1 1)
    (3 2 1)
    (5 1)
    (2 2 2)
    (4 2)
    (3 3)
    (6)
    
    
Now restrict the partitions to those that do not contain 1's or those that 
consist of precisely 3 components:
::

    > (partition (l 6 :minimum-part 2)
    (print l))
    (2 2 2)
    (4 2)
    (3 3)
    (6)
    
    > (partition (l 6 :number-of-parts 3)
        (print l))
        (4 1 1)
        (3 2 1)
        (2 2 2)


We can further restrict the partitions to only include components that contain 
components no larger than 3 and to those partitions that consist of distinct c
omponents.
::

    > (partition (l 6 :number-of-parts 3 :maximum-part 3)
    (print l))
    (3 2 1)
    (2 2 2)
    
    > (partition (l 6 :number-of-parts 3 :maximum-part 3 :distinct? t)
    (print l))
    (3 2 1)
    
    > (partition (l 6 :distinct? t)
    (print l))
    (3 2 1)
    (5 1)
    (4 2)
    (6)

It is well known, and easy to prove, that the number of partitions of an 
integer n into m parts is equal to the number of partitions of n where the 
largest component of the partition is m. We can check this numerically with 
the following functions. The rst function counts the number of partitions of n 
with exactly m parts. For this the :number-of-parts option sudffices.

.. code-block:: lisp

    (defun partition-count-fixed-parts (n m)
      (let ((cnt 0))
        (partition (part n :number-of-parts m)
          (declare (ignore part))
          (incf cnt))
          cnt))

The function partition-count-exact-max computes the number of partitions where 
the maximum component is exactly m. In this case, the :maximum-part option 
helps lter the partition, but then an additional test needs to be applied to 
ensure that each partition actually has an element of size m.

.. code-block:: lisp

    (defun partition-count-exact-max (n m)
      (let ((cnt 0))
        (partition (part n :maximum-part m)
          (when (= m (apply #'cl::max part))
            (incf cnt)))
       cnt))


Finally we provide a routine for testing the functions given above.

.. code-block::

    (defun partition-test-1 (n m)
      (let ((part-count1 (partition-count-fixed-parts n m))
            (part-count2 (partition-count-exact-max n m)))
        (list (= part-count1 part-count2)
           part-count1 part-count2)))

::

    > (partition-test-1 10 3)
    (T 8 8)
    
    > (partition-test-1 15 3)
    (T 19 19)
    
    > (partition-test-1 15 4)
    (T 27 27)


Memoization (3.2)
-----------------

It often occurs that the straightforward way of expressing an algorithm is 
quite inefficient because it performs a great deal of recomputation of values 
that were previously computed. The simplest example along these lines is the 
Fibonacci function

.. code-block:: lisp

    (defun fib (n)
      (if (< n 2) 1
        (+ (fib (- n 1)) (fib (- n 2)))))

Due to the double recursion, this implementation can take time exponential in n. 
By caching values of (fib n) to avoid recomputation, it is possible to reduce 
this to a computation that is linear in n. This is easily done using the control 
abstraction memoize. For instance, we have the following efficient version of 
the Fibonacci function:

.. code-block:: lisp

    (defun fib-memo (n)
      (memoize `(fib ,n)
        (if (< n 2) 1
          (+ (fib-memo (- n 1)) (fib-memo (- n 2))))))


The memoize form should be understood as follows. The rst argument is an 
expression that identifies a particular computation. The above example, the 
form `(fib ,n)` represents the computation of the n-th Fibonacci number. 
The body of the memoize form contains the code that actually performs the 
computation. The memoize form checks a cache to see if the computation
indicated by the rst argument has been previously performed. If so, the value 
returned by the earlier computation is returned immediately. If not the body 
is executed and the value cached for future use.


The cache used by the memoize control structure is handled by the class 
weyli::has-memoization. The domain of general expressions (discussed in Chapter 6)
is always a subclass of weyli::has-memoization and is the domain with which 
memoization is usually associated. The internal function
weyli::%memoize allows one to associate a memoization with any subclass of 
weyli::has-memoization.

.. function:: weyli::%memoize domain expression &body body  [Method]

   Each time this form is executed, it checks to see if expression is cached 
   in domain's memoization cache. If so, the value in the cache is returned 
   without executing the body. If expression is not found in the cache, then 
   the forms in body are evaluated, and the value of the last form is both 
   returned and saved in domain's memoization cache.

It is usually much more convenient to use the memoize control structure:

.. function:: memoize expression &body body [Special Form]

   Performs the same functions as weyli::%memoize except that the domain used 
   is *gen-eral*.
   
   
One should note that the expression which is used as the index of the cache 
cannot be a constant. It should contain all of the information that can influence 
the value returned by the body. Also, at this writing, only the rst value 
returned by the expression being memoized is actually cached.
The other values are ignored.


Tuples (3.3)
------------
Instances of the class weyli::tuple are simply one-dimensional vectors. 
Generally, one doesn't create instances of bare tuples, but rather includes 
the weyli::tuple class with some other classes to create more interesting objects. 
For instance, Weyl vectors are instances of a class that than includes tuple 
and domain-element. When creating an instance of a weyli::tuple one needs to 
initialize a slot that contains the values of the tuple. This can be done by 
using the :values initialization keyword.
For instance,
:: 

    > (setq tup (make-instance 'weyli::tuple :values '(1 2 3)))
    <1, 2, 3>
    
    
The initialization value can be either a list or a Lisp vector.

Since tuples are instances of a class, various methods can be overloaded to 
work with them. The simplest such function is length, which computes the number 
elements in the tuple. Another useful function is ref, which accesses different
elements of the tuple by their index. The generic function ref is used in place 
of the Common Lisp functions aref or svref.

.. function:: ref sequence &rest indices   [Macro]

   Accesses the indicated elements of sequence. Tuples are one-dimensional 
   arrays so only one index is allowed. The indexing scheme is zero based, 
   so the rst element of the tuple has index 0, the second 1 and so on.
   
For example,
::

    > (list (ref tup 0) (ref tup 2) (ref tup 1))
    (1 3 2)
    
    
It is sometimes useful to be able to convert a tuple into a list of its elements. This can be done
with the following function:

.. function:: list-of-elements tuple   [Method]

   Returns a list of the elements of tuple. For example,

::

    > (list-of-elements tup)
    (1 2 3)
    
The following functions extend the Common Lisp mapping functions to work with 
tuples as well as the usual sequences of Common Lisp.

.. function:: map type function tuple &rest sequences   [Method]

   The number of arguments of function is expected to be one more than the 
   number of elements in sequences. function is applied to each element of 
   tuple and the corresponding elements of each of the elements of sequences. 
   
For instance,
;;

    > (map 'tuple #'cons tup tup)
    <(1 . 1), (2 . 2), (3 . 3)>


Algebraic objects in Weyl have a slot that indicates the algebraic domain of 
which the object is an element. When creating an instance of such an object it 
is necessary to indicate this domain. If the sequence returned by map requires 
this information then the domain will be extracted from the tuple. If it is 
necessary to explicitly specify the domain of the resulting tuple, the following
function may be used.

.. function:: map-with-domain type domain function tuple &rest sequences [Method]

   Similar to map but the domain of the resulting tuple will be domain.
   

Arithmetic with Lists (3.4) 
---------------------------

In Lisp, lists are extremely convenient data structures, especially for 
temporary results in computations. To make them even more useful, Weyl provides 
a couple of functions for performing arithmetic operations with lists. These 
routines use the Weyl arithmetic operations, and thus can be used both for 
arrays and lists that contain Lisp numbers and those those that contain Weyl's
mathematical objects 

.. function:: list-inner-product list1 list2  [Method]

   Computes the sum of the pairwise product the elements of list1 and list2.


.. function:: list-expt base-list expt-list   [Method]

   Returns a list of consisting of the elements of base-list raised to the 
   power of the corresponding element of expt-list.


.. function:: array-times array1 array  [Method]

   Checks to make sure the arguments of the of the right dimensions and then 
   computes a matrix product storing the result in a new, appropriately sized 
   matrix.
   
   
AVL Trees (3.5)
---------------
tbw



