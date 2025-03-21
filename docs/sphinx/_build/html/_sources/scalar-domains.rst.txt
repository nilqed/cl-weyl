.. (ql:quickload :weyl)
.. (in-package :weyl)

Scalar Domains (5)
==================
;;;; Almost all domains that occur in mathematics can be constructed from the 
;;;; rational integers, Z. For instance, the the rational numbers (Q) are the 
;;;; quotient eld of Z, the real numbers are completion of Q using the 
;;;; valuations at in nity and so on. For performance reasons provides direct 
;;;; implementations of four basic scalar domains, the rational integers (Z), 
;;;; the rational numbers (Q), the real numbers (R) and the complex numbers 
;;;; (C ). In addition, a similar direct implementation of finite fields is 
;;;; also provided. These scalar domains are called numeric domains, and the 
;;;; corresponding domain hierarchy is given in Figure 5.1. Instances of the 
;;;; GFp class are finite fields of p elements. (These are parameterized domains.) 
;;;; The GFq class is used to implement elds of q = pr elements. The class GFm 
;;;; is discussed in more detail in Section 5.6. Instances of the characteristic
;;;; zero domains may be created using the functions get-rational-integers, 
;;;; get-rational-numbers, get-real-numbers and get-complex-numbers. To create 
;;;; finite fields one can use the function get-finite-field. Depending on its 
;;;; argument, which must be a power of a single prime, this function will 
;;;; return a domain of type GFp or GFq.
;;;; Elements of these domains are implemented using the hierarchy of structure
;;;; types shown in Figure 5.2. The elements of a rational integer domain are 
;;;; all of structure type rational-integer. However, some numeric domains may
;;;; contain elements of different structure types. For instance, objects of 
;;;; structure type rational-integer as well as rational-number can be elements
;;;; of the rational number domains (Q). This approach allows us to represent 
;;;; exactly integers and rational numbers in R and C . In the future, a similar
;;;; approach will allow us to represent algebraic and transcendental 
;;;; numbers exactly.


;;;; One of the complications Weyl must deal with is that Lisp has its own model
;;;; of the numbers, which must co-exist with Weyl's model. The type structure 
;;;; used by Lisp is given in Figure 5.3. To simplify use of Weyl, we allow 
;;;; users to create Weyl numbers from Lisp numbers, and to incorporate Lisp 
;;;; numbers into their code by providing some automatic coercions. Thus adding
;;;; a Lisp number to an element of R causes the Lisp number to be coerced to 
;;;; an element of R of structure rational-integer.
;;;; There are several different ways to determine if an object is a scalar.

;;;;      (typep obj 'cl:number) ..... a Lisp number,
;;;;      (typep obj 'numeric) ....... a Weyl number,
;;;;      (number? obj) .............. either a Lisp number or a Weyl number.

.. code-block:: lisp

    (typep 1234 'cl:number)
    => T

    (typep 1234 'weyli::numeric)
    => NIL

    (typep (coerce 1234 (get-rational-integers))  'weyli::numeric)
    => T

    (number?  1234 )
    => T

    (number? (coerce 1234 (get-rational-integers)))
    => T

;;;; The following sections are organized by the di erent structure types. 
;;;; Sections 5.1 through 5.6 deal with rational integers, rational numbers, 
;;;; real numbers, complex numbers and elements of finite fields respectively. 
;;;; In each section we discuss the di erent domains these structure elements 
;;;; can be used in followed by a discussion of the operations that can be 
;;;; performed with each structure type.


Rational Integers (5.1)
-----------------------

;;; The rational integers are the integers of elementary arithmetic:
;;;
;;;          Z = {..., -3, -2, -1, 0, 1, 2, 3, ...}
;;;
;;; Other than limitations on the memory of the host computer, there is no 
;;; limitation on the size of the elements of Z. The term rational integer is 
;;; used to distinguish this domain from other domains of algebraic integers, 
;;; e.g., Z[(1 + sqrt(5))/2].

;;; A domain of rational integers can be created using the following function.


.. function::  get-rational-integers                               [Function]

   Returns a domain that is isomorphic to the rational integers, Z. When 
   called repeatedly, it always returns the same value until reset-domains 
   is called.


;;; Most of the time, there only needs to be one rational integer domain. The 
;;; domain of rational integers is a euclidean domain.

;;; Elements of structure type rational-integer can be elements of domains of 
;;; type rational-numbers, real-numbers and complex-numbers. Equivalently, if 
;;; domain is a domain that admits elements of structure type rational-integer,
;;; then one invoke make-element with domain and a Lisp integer

;;; Rational integers are most easily created by coercing a Lisp integer to a 
;;; rational integer domain using the function coerce. Furthermore, the usual 
;;; arithmetic routines (+, -, *, / and expt) work with rational integers that 
;;; are elements of the same domain. If the domain is a field then `/` may
;;; return a rational-number.

;;; For instance, the following routine could be used to compute factorials.

.. code-blocks:: lisp

    (defvar ZZ (get-rational-integers))

    (defun my-factorial (n)
       (if (< n 2) (one ZZ)
         (* (coerce n ZZ) (my-factorial (- n 1)))))


;;; Notice that the unit element of ZZ was created by using the function one, 
;;; rather than (coerce 1 ZZ). In general, this is more efficient.

;;; One of the more commonly used control structures is that used to construct 
;;; exponentiation from multiplication by repeated squaring. This control 
;;; structure is captured by the internal function weyli::repeated-squaring:
;;;
.. function::  weyli::repeated-squaring mult one                   [Function]

   Returns a function of two arguments that is effectively

            (lambda (base exp)
            (declare (integer exp))
            (expt base exp))

   except that the body does the exponentiating by repeated squaring using 
   the operation mult. If exp is 1, then one is returned.


;;; Using this function, one could have de ned exponentiation as

.. code-block:: lisp

       (defun expt (x n)
         (funcall (weyli::repeated-squaring 
            #'weyli::times (coerce 1 (domain-of x))) x n))


;;; However, this routine can be used for operations other than exponentiation.
;;; For instance, if one wanted a routine that replicates a sequence n times, 
;;; one could use the following:

.. code-block:: lisp

        (defun replicate-sequence (x n)
          (funcall (weyli::repeated-squaring #'append ()) x n))



.. function::    isqrt n                                            [Function]

   Returns the integer part of the square root of n.

.. code-block:: lisp

    (isqrt 123456789)
    => 11111

    (- (expt (isqrt 123456789) 2) 123456789)
    => -2468


.. function ::   integer-nth-root m n                             [Function]

   Computes the largest integer not greater than the n-th root of m.


.. code-block:: lisp

    (weyli::integer-nth-root 123456789  5)
    => 42

    (expt (weyli::integer-nth-root 123456789  5) 5)
    => 130691232



.. function::    power-of? number &optional base                  [Function]

   Returns base and k if number = base , otherwise it returns nil. If base is 
   not provided returns the smallest integer of which number is a perfect 
   power.


.. code-block:: lisp

    (power-of? 256 2)
    => 2
    => 8

    (expt 2 8)
    => 256

    (ignore-errors (power-of? 256))
    ; debugger invoked on a SIMPLE-ERROR in thread
    ; #<THREAD "main thread" RUNNING {10048B8113}>:
    ;  Haven't implemented the rest of the cases



.. function::   factor n                                           [Function]

   Factors n into irreducible factors. The value returned is a list of dotted
   pairs. The first component of the dotted pair is the divisor and the second
   is the number of times the divisor divides n. The type of factorization 
   method used, can be controlled by setting the variable *factor-method*. 
   The allowable values are simple-integer-factor and fermat-integer-factor.


.. code-block:: lisp

    (factor 123456789)
    => ((3 . 2) (3607 . 1) (3803 . 1))

    (factor 12345678901234567890)
    => ((2 . 1) (3 . 2) (5 . 1) (101 . 1) (3541 . 1) (3607 . 1) (3803 . 1) (27961 . 1))


    (defvar ff (factor 12345678901234567890))
    => FF

    (defvar ffl (mapcar #'(lambda (x) (expt (car x) (cdr x))) ff))
    => FFL

    ffl
    => (2 9 5 101 3541 3607 3803 27961)

    (reduce #'%times  ffl)
    => 12345678901234567890

;;; note that WEYLI:* is a macro, not a function, i.e. (reduce #'*  ffl)
;;; won't work as described farther above.



.. function::    prime? n                                          [Function]

   Returns true if n is a prime number. (For other domains, if n has no factors
   that are not units.)

.. code-block:: lisp

    (prime? 77731)
    => T

    (factor  77731)
    => ((77731 . 1))

    (prime? 777313713731)
    => T

    (prime? 7773137137317313737)
    => NIL

    (factor  7773137137317313737)
    => ((3 . 2) (2741 . 1) (1485721 . 1) (212083813 . 1))



.. function::  totient n  [Function]

   Returns the Euler totient function of n, the number of positive integers 
   less than n that are relatively prime to n, i.e.:

        totient(n) = n  \Pi_{p} (1 - \frac{1}{p}), 

   where product is over all prime divisors of n.


.. code-block:: lisp

    (totient 122345)
    => 97872

    (totient 122345876689)
    => 116852588160


.. function::    factorial n                                       [Function]

   Computes n!


.. code-block:: lisp

    (factorial 123)
    => 1214630436702532967576624324188129585545421708848338231532891816182923
       5892362167668831156960612640202170735835221294047782591091570411651472
       186029519906261646730733907419814952960000000000000000000000000000



..function::    pochhammer n k                                     [Function]

  Computes the Pochhammer function of n and k, which is closely related to 
  the factorial:

         pochhammer(n; k) = (n)_k = n (n+1) (n+2) ... (n+k-1)

.. code-block:: lisp

    (pochhammer 12 5)
    => 524160

    (pochhammer 33 12)
    => 10102470716719180800



.. function::  combinations n m                                    [Function]

   Computes the number of combinations of n things taken m at a time.

         combinations(n,m) = \binom(n,m) = \frac{n!}{m!(n-m)!}.


.. code-block:: lisp

    (combinations 33 12)
    => 354817320

    (combinations 77 7)
    => 2404808340


.. function::     newprime n                                       [Function]

   Returns the largest prime less than its argument.


.. code-block:: lisp

    (weyli::newprime 12)
    => 7

    (weyli::newprime 1299)
    => 113

;;; **BUG?** never > 113 ....

;;; *** Need to point out that the elements of the second rational integer 
;;;     domain created are totally different from that those that are elements 
;;;     of the rst instance of the rational integers.)
;;;


Rational Numbers (5.2)
----------------------

;; The domain rational numbers, Q, is the quotient eld of the ring of rational 
;; integers. The elements of a rational number domain can have structure type 
;; either rational-integer or rational-number. Elements.
;; As in Common Lisp there is a set of four functions for truncating numbers 
;; and ratios to integers. If the second argument is not provided then it 
;; defaults to 1. If only the rst argument is provided and it is a rational 
;; integer, then all four functions return the same values.
;; A domain of rational integers is created by the following function.

.. function::       get-rational-numbers                            [Function]

   Returns a domain that is isomorphic to the rational numbers, Q. When called
   repeatedly, it always returns the same value until reset-domains is called.

.. function::  floor number &optional divisor                       [Function]


.. function::  ceiling number &optional divisor                     [Function]


.. function::  truncate number &optional divisor                    [Function]


.. function::  round number &optional divisor                       [Function]


.. code-block:: lisp

    (defvar QQ (get-rational-numbers))
    => QQ

    (wtype QQ)
    => RATIONAL-NUMBERS

    (defvar q11/3 (coerce (/ 11 3) QQ))
    => Q11/3

    q11/3
    => 11/3

    (wtype q11/3)
    => RATIONAL-NUMBER

    (numerator q11/3)
    => 11

    (denominator q11/3)
    => 3

    (floor q11/3)
    => 3

    (ceiling q11/3)
    => 4

    (truncate q11/3)
    => 3
    => 2

    (round q11/3)
    => 4
    => -1


Real Numbers (5.3)
------------------

;;; The entire real number situation is somewhat confused. In particular, the 
;;; relationship between floating point numbers and real numbers is jumbled. 
;;; These issues will be fixed at a later date.
 
.. function::     get-real-numbers &optional precision             [Function]

   This returns a domain whose elements are floating point numbers. If precision
   is not specified, then the machines default double precision floating point 
   numbers will be used. If precision is specified, then a special arbitrary 
   precision floating point package will be used. Operations with these numbers 
   will be somewhat slower (and will cause more garbage collection) than when 
   using the machine's floating point data types.


.. code-block:: lisp

    (get-real-numbers )
    => R

    (ignore-errors (get-real-numbers 128))
    => ;   The function GET-REAL-NUMBERS is called with one argument, but wants 
         exactly zero. 
    not implemented yet??

    (describe 'get-real-numbers)
    ; WEYLI:GET-REAL-NUMBERS
    ;  [symbol]

    ; GET-REAL-NUMBERS names a generic function:
    ;  Lambda-list: ()
    ;  Derived type: (FUNCTION NIL *)
    ;  Method-combination: STANDARD
    ;  Methods:
    ;    (GET-REAL-NUMBERS ())

    (documentation 'get-real-numbers 'function)
    ; => NIL



.. function::  floor number &optional divisor                      [Function]

   Computes the floor of number.


.. code-block:: lisp

    (floor (coerce 3.14 (get-real-numbers)))
    => 3
    => 0.1400001


.. function::      ceiling number &optional divisor                [Function]

   Computes the ceiling of number.

.. code-block:: lisp

    (ceiling  (coerce 3.14 (get-real-numbers)))
    => 4
    => -0.8599999


.. function::  truncate number &optional divisor                   [Function]

   Computes the truncate of number.


.. code-block:: lisp

    (truncate  (coerce 3.14 (get-real-numbers)))
    => 3
    => 0.1400001


.. function::     round number &optional divisor                   [Function]

   Computes the round of number.


.. code-block:: lisp

    (round   (coerce 3.14 (get-real-numbers)))
    => 3
    => 0.1400001


    (round  3.14)
    => 3
    => 0.1400001


    (round  3.14 3)
    => 1
    => 0.1400001

    (round  3.14 2)
    => 2
    => -0.8599999



.. function:: sqrt n                                               [Function]

   For positive n returns positive n with the same precision as n.


.. code-block:: lisp

    (sqrt   (coerce 3.14 (get-real-numbers)))
    => 1.7720045

    (sqrt 3.14)
    => 1.7720045

    (sqrt 4.0)
    => 2.0


;;; The following standard trigonometric and hyperbolic routines are provided
;;;   
;;;     sin n asin n sinh n asinh n
;;;     cos n acos n cosh n acosh n
;;;     tan n atan n tanh n atanh n

.. code-block:: lisp

    (defvar mypi (coerce (/ 355 113) (get-real-numbers)))

    (wtype mypi)
    => RATIONAL-NUMBER -- although we coerced to R

    (defvar mypi-r (coerce (/ 355.0 113.0) (get-real-numbers)))
    => MYPI-R

    mypi-r
    => 3.141593

    (wtype mypi-r)
    => WEYLI::FLOATING-POINT-NUMBER

    (defvar trigfuns '(sin cos tan sinh cosh tanh))
    => TRIGFUNS

    (mapcar #'(lambda (x) (funcall x mypi-r)) trigfuns)
    => (-3.2584137e-7 -1.0 3.2584137e-7 11.548743 11.591957 0.9962721)


.. function::     exp n                                            [Function]

   Returns e^n


.. code-block:: lisp

    (exp 2)
    => 7.389056

    (exp (log (exp 1)))
    =>2.7182817



.. function::  log n &optional b                                   [Function]

   For positive n returns the principal part of logb n. If b is not supplied 
   then e, the base of natural logarithms, is used for b.


.. code-block:: lisp

    (log (exp 1))
    => 0.99999994

    (ignore-errors (log (exp 1) 2))
    => The function LOG is called with two arguments, but wants exactly one.


    (describe 'log)
    ; WEYLI::LOG
    ;  [symbol]
    ;
    ; LOG names a generic function:
    ;  Lambda-list: (NUMBER)
    ;  Derived type: (FUNCTION (T) *)
    ;  Documentation:
    ;    Return the natural logarithm of the number.
    ;  Method-combination: STANDARD
    ;  Methods:
    ;    (LOG (FLOATING-POINT-NUMBER))
    ;    (LOG (BIGFLOAT))
    ;    (LOG (GENERAL-EXPRESSION))
    ;    (LOG (NUMERIC))
    ;    (LOG (SYMBOL))
    ;    (LOG (NUMBER))
    ;  Source file: /home/kfp/quicklisp/local-projects/weyl/lisp-numbers.lisp
    ; *


Complex Numbers (5.4)
---------------------

.. code-block:: lisp

    (get-complex-numbers)
    => C

    (defvar c11 (coerce #C(1 1)  (get-complex-numbers)))
    => C11

    c11
    => 1 + i


.. function:: realpart z                                           [Function]

   If z = x + iy returns x.


.. code-block:: lisp
 
    (realpart c11)
    => 1


.. function::     imagpart z                                       [Function]

   If z = x + iy returns y .[Function]


.. code-block:: lisp

    (imagpart c11)
    => 1



.. function:: conjugate z                                          [Function]

   If z = x + iy returns x ? iy .[Function]


.. code-block:: lisp

    (conjugate c11)
    => 1 + -1 i



.. function:: abs z                                                [Function]

   If z = x + iy returns  |z| = sqrt(x^2 + y^2).


.. code-block:: lisp

    (abs  c11)
    => 1.4142135


.. function::     phase z                                          [Function]

   If z = r*e^(it) returns t, where r=|z|.

.. code-block:: lisp

    (phase c11)
    => 0.7853982

    (* c11 c11)
    => 2 i  

    (/ c11 c11)
    => 1

    (+ c11 c11)
    => 2 + 2 i

    (- c11 c11)
    => 0


Quaternions (5.5)
-----------------

;;; Quaternions are a non-commutative algebra over a field, usually the reals, 
;;; that are often used to represent three dimensional rotations. Weyl can 
;;; construct a quaternion algebra over any field F. This algebra is a four 
;;; dimensional vector space over F with the following relations. The
;;; element (1,0,0,0) is the multiplicative identity. If we denote 
;;; i = (0,1,0,0), j = (0,0,1,0) and k = (0,0,0,1), then
;;; 
;;;      i^2 = j^2 = k^2 = -1,  ij = -ji, jk = -kj and ik = -ki.
;;; 


.. function:: get-quaternion-domain field                           [Function]

   Gets a quaternion algebra over field, which must be a field.


.. code-block:: lisp

    (get-quaternion-domain (get-rational-numbers))
    => Quat(Q)

    (get-quaternion-domain (get-real-numbers))
    => Quat(R)

    (get-quaternion-domain (get-complex-numbers))
    => Quat(C)


Quaternions can be created using make-element.

.. function::  make-element quaternion-algebra v1 v2 v3 v4         [Function]
  
   Creates an element of quaternion-algebra from its arguments. The value
   returned will be v1 + i v2 + j v3 + k v4 . As with other versions of 
   make-element, the function weyli::make-element assumes the arguments are 
   all elements of the coefficient domain and is intended only for internal use.


;;; As an algebraic extension of the real numbers, the quaternions are a little
;;; strange. The subfield of quaternions generated by 1 and i, is isomorphic to
;;; the complex numbers. Adding j and k makes the algebra non-commutative and 
;;; causes it to violate some basic intuitions. For instance, 1 hasat least 
;;; three square roots!
;;; 
;;; We illustrate some of these issues computationally. First we create a 
;;; quaternion algebra in which to work.
;;; 
;;;      > (setq q (get-quaternion-domain (get-real-numbers)))
;;;      Quat(R)
;;; 
;;; Next, we can create some elements of the quaternions and do some simple 
;;; calculations with them.
;;; 
;;;     > (setq a (make-element q 1 1 1 1))
;;;     <1, 1, 1, 1>
;;; 
;;;     > (setq b (/ a 2))
;;;     <1/2, 1/2, 1/2, 1/2>
;;; 
;;;     > (* b b b)
;;;     <-1, 0, 0, 0>
;;; 

.. code-block:: lisp

    (defvar q4 (get-quaternion-domain (get-real-numbers)))
    => Q4

    (defvar aq4 (make-element q4 1 1 1 1))
    => AQ4

    (defvar bq4 (/ aq4 2))
    => BQ4

    (* bq4 bq4 bq4)
     => <-1, 0, 0, 0>


;;; As expected, one can multiply quaternions by other quaternions and by 
;;; elements of the coefficient field (or objects that can be coerced into the 
;;; coefficient field).


.. function:: conjugate quaternion                                  [Function]

   This is an extension of the concept of complex conjugation. It negates 
   the coefficients of i, j and k. This is illustrated by the following example.
 
;;;     > (setq c (make-element q 1 2 3 4))
;;;     <1, 2, 3, 4>
;;;
;;;     > (conjugate c)
;;;     <1, -2, -3, -4>
;;; 
;;;      > (* c (conjugate c)
;;;     <30, 0, 0, 0>
;;; 

.. code-block:: lisp

    (defvar cq4 (make-element q4 1 2 3 4))
    => CQ4

    (conjugate cq4)
    => <1, -2, -3, -4>

    (* cq4 (conjugate cq4))
    => <30, 0, 0, 0>


;;; Notice that the components of the product of a quaternion with its conjugate
;;; are all zero except for the very first component. This matches what happens
;;; when one multiplies a complex number with its complex conjugate.


;;; ???? division, expt .... not working

;;; Finite Fields (5.6)
;;; 
;;; The usual finite fields are provided in Weyl, Fp and algebraic extensions 
;;; of Fq . Such domains are called GFp domains. Since all finite fields with 
;;; the same number of elements are isomorphic, fields are created by specifying
;;; the elements in the field.
;;; 
;;;       get-finite-field size         [Function]
;;; 
;;; Size is expected to be a a power of a prime number. This function returns 
;;; a finite field with the indicated number of elements. If size is nil then 
;;; a GFm field is returned.
;;; 
;;;      number-of-elements finite-field [Function]
;;; 
;;; Returns the number of elements in finite-field.
;;;
;;; At the moment Weyl can only deal with the fields F2^k and Fp . For instance,
;;; 
;;;    > (setq F256 (get-finite-field 256))
;;;    GF(2^8)
;;; 
;;;    > (characteristic F256)
;;;    2
;;; 
;;;    > (number-of-elements F256)
;;;   256
;;; 

(defvar  F256 (get-finite-field 256))
; => F256

(characteristic F256)
; => 2

(number-of-elements F256)
; => 256


;;; Elements of a GFp are created by coercing a rational integer into a GFp 
;;; domain. For finite fields with characteristic greater than 2, coercing an 
;;; integer into Fp maps n into n (mod p). For F2^k , the image of an integer 
;;; is a bit more complicated. Let the binary representation of n be
;;; 
;;;     n = n_l ... n_0
;;;
;;; and let alpha be the primitive element of F2^k over F2. Then
;;;
;;;     n -> n_{k-1} alpha^{k-1} + ... + n_1 alpha + n_0.
;;; 
;;; This mapping is particularly appropriate for problems in coding theory.
;;; 
;;; In addition, elements of finite fields can be created using make-element.
;;; 
;;;       make-element finite-field integer &optional rest  [Function]
;;; 
;;; Creates an element of nite- eld from integer . This is the only way to 
;;; create elements of Fp^k . (As with all make-element methods, the argument 
;;; list includes &rest arguments, but for finite fields any additional arguments
;;; are ignored.)
;;; As an example of the use of nite elds, consider the following function, 
;;; which determines the order of an element of a finite field (the hard way).
;;; 
;;;      (defun element-order (n)
;;;        (let* ((domain (domain-of n))
;;;               (one (coerce 1 domain)))
;;;          (loop for i upfrom 1 below (number-of-elements domain)
;;;                for power = n then (* n power)
;;;              do (when (= power one)
;;;                     (return i)))))
;;; 
;;; A more efficient routine is provided by Weyl as multiplicative-order.
;;; 
;;; multiplicative-order elt     [Function]
;;; 
;;; Elt must be an element of a finite field. This routine computes multiplicative 
;;; order of elt. This routine requires factoring the size of the multiplicative 
;;; group of the finite field and thus is appropriate for very large finite fields.
;;; 
;;; The following illustrates use of these routines.
;;; 
;;;      > (element-order (coerce 5 (get-finite-field 41)))
;;;     20
;;;      > (multiplicative-order (coerce 5 (get-finite-field 41)))
;;; 

(defun element-order (n)
        (let* ((domain (domain-of n))
              (one (coerce 1 domain)))
         (loop for i upfrom 1 below (number-of-elements domain)
              for power = n then (* n power)
            do (when (= power one)
                    (return i)))))

(element-order (coerce 5 (get-finite-field 41)))
; => 20

(multiplicative-order (coerce 5 (get-finite-field 41)))
; => 20


;;; Consider what is involved when implementing an algorithm using the Chinese 
;;; remainder theorem. The computation is done in a number of domains like 
;;; Z=(p1), Z=(p2) and Z=(p3). The results are then combined to produce results 
;;; in the domains Z=(p1p2) and Z=(p1p2 p3). Rather than working in several 
;;; different domains and explicitly coercing the elements from one to another, 
;;; it is easier to assume we are working in a single domain that is the union 
;;; of Z=(m) for all integers m and marking the elements of this domain with 
;;; their moduli. We call this domain a GFm.
;;; 
;;; GFm domains are also created using the get-finite-field but by providing 
;;; nil as the number of elements in the field.
;;; 
;;; Elements of GFm are printed by indicating their modulus in a subscript 
;;; surrounded by parentheses. Thus 2_(5) means 2 modulo 5. Combining two 
;;; elements a_(m) and b_(m) that have the same moduli is the same as if they 
;;; were both elements of Z=(m). To combine elements of two different rings, 
;;; we find a ring that contains both as subrings and perform the calculation 
;;; there. Thus combining a_(m) and b_(n) we combine the images of a and b as 
;;; elements of Z=(gcd(m; n)).
;;; FIXTHIS: Need works something out for dealing with completions of the 
;;; integers at primes, and how we are going to compute with elements.

