(ql:quickload :weyl)
(in-package :weyl)

;;;; Scalar Domains (5)
;;;;
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

;;;;    (typep obj 'cl:number) ..... a Lisp number,
;;;;    (typep obj 'numeric) ....... a Weyl number,
;;;;    (number? obj) .............. either a Lisp number or a Weyl number.


(typep 1234 'cl:number)
; => T

(typep 1234 'weyli::numeric)
; => NIL

(typep (coerce 1234 (get-rational-integers))  'weyli::numeric)
; => T

(number?  1234 )
; => T

(number? (coerce 1234 (get-rational-integers)))
; => T

;;;; The following sections are organized by the di erent structure types. 
;;;; Sections 5.1 through 5.6 deal with rational integers, rational numbers, 
;;;; real numbers, complex numbers and elements of finite fields respectively. 
;;;; In each section we discuss the di erent domains these structure elements 
;;;; can be used in followed by a discussion of the operations that can be 
;;;; performed with each structure type.


;;; Rational Integers (5.1)
;;;
;;; The rational integers are the integers of elementary arithmetic:
;;;
;;;          Z = {..., -3, -2, -1, 0, 1, 2, 3, ...}
;;;
;;; Other than limitations on the memory of the host computer, there is no 
;;; limitation on the size of the elements of Z. The term rational integer is 
;;; used to distinguish this domain from other domains of algebraic integers, 
;;; e.g., Z[(1 + sqrt(5))/2].

;;; A domain of rational integers can be created using the following function.


;;;         get-rational-integers                                   [Function]
;;;
;;; Returns a domain that is isomorphic to the rational integers, Z. When 
;;; called repeatedly, it always returns the same value until reset-domains 
;;; is called.


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
;;;    weyli::repeated-squaring mult one     [Function]
;;;
;;;  Returns a function of two arguments that is effectively
;;;
;;;        (lambda (base exp)
;;;          (declare (integer exp))
;;;          (expt base exp))
;;;
;;; except that the body does the exponentiating by repeated squaring using 
;;; the operation mult. If exp is 1, then one is returned.

;;; Using this function, one could have de ned exponentiation as

;;;       (defun expt (x n)
;;;         (funcall (weyli::repeated-squaring 
;;;            #'weyli::times (coerce 1 (domain-of x))) x n))
;;;
;;; However, this routine can be used for operations other than exponentiation.
;;; For instance, if one wanted a routine that replicates a sequence n times, 
;;; one could use the following:
;;;
;;;        (defun replicate-sequence (x n)
;;;          (funcall (weyli::repeated-squaring #'append ()) x n))
;;;
;;;

;;;    isqrt n   [Function]
;;;
;;; Returns the integer part of the square root of n.

(isqrt 123456789)
; => 11111

(- (expt (isqrt 123456789) 2) 123456789)
; => -2468


;;;
;;;
;;;    integer-nth-root m n  [Function]
;;;
;;; Computes the largest integer not greater than the n-th root of m.
;;;

(weyli::integer-nth-root 123456789  5)
; => 42

(expt (weyli::integer-nth-root 123456789  5) 5)
; => 130691232


;;;
;;;    power-of? number &optional base  [Function]
;;;
;;; Returns base and k if number = base , otherwise it returns nil. If base is 
;;; not provided returns the smallest integer of which number is a perfect 
;;; power.
;;;

(power-of? 256 2)
; => 2
; => 8

(expt 2 8)
; => 256

(power-of? 256)
; debugger invoked on a SIMPLE-ERROR in thread
; #<THREAD "main thread" RUNNING {10048B8113}>:
;  Haven't implemented the rest of the cases


;;;    factor n  [Function]
;;;
;;; Factors n into irreducible factors. The value returned is a list of dotted
;;; pairs. The first component of the dotted pair is the divisor and the second
;;; is the number of times the divisor divides n. The type of factorization 
;;; method used, can be controlled by setting the variable *factor-method*. 
;;; The allowable values are simple-integer-factor and fermat-integer-factor.
;;;

(factor 123456789)
; => ((3 . 2) (3607 . 1) (3803 . 1))

(factor 12345678901234567890)
; => ((2 . 1) (3 . 2) (5 . 1) (101 . 1) (3541 . 1) (3607 . 1) (3803 . 1) (27961 . 1))


(defvar ff (factor 12345678901234567890))
; => FF

(defvar ffl (mapcar #'(lambda (x) (expt (car x) (cdr x))) ff))
; => FFL

ffl
; => (2 9 5 101 3541 3607 3803 27961)

(reduce #'%times  ffl)
; => 12345678901234567890

;;; note that WEYLI:* is a macro, not a function, i.e. (reduce #'*  ffl)
;;; won't work as described farther above.


;;;
;;;    prime? n    [Function]
;;;
;;; Returns true if n is a prime number. (For other domains, if n has no factors
;;; that are not units.)
;;;

;(prime? 77731)
; => T

(factor  77731)
; => ((77731 . 1))

(prime? 777313713731)
; => T

(prime? 7773137137317313737)
; => NIL

(factor  7773137137317313737)
; => ((3 . 2) (2741 . 1) (1485721 . 1) (212083813 . 1))


;;;
;;;   totient n  [Function]
;;;
;;; Returns the Euler totient function of n, the number of positive integers 
;;; less than n that are relatively prime to n, i.e.:
;;;
;;;        totient(n) = n  \Pi_{p} (1 - \frac{1}{p}), 
;;;
;;; where product is over all prime divisors of n.
;;;

(totient 122345)
; => 97872

(totient 122345876689)
; => 116852588160

;;;
;;;    factorial n   [Function]
;;;
;;; Computes n!
;;;

(factorial 123)
; => 1214630436702532967576624324188129585545421708848338231532891816182923
;    5892362167668831156960612640202170735835221294047782591091570411651472
;    186029519906261646730733907419814952960000000000000000000000000000





;;;
;;;    pochhammer n k    [Function]
;;;
;;; Computes the Pochhammer function of n and k, which is closely related to 
;;; the factorial:
;;;
;;;         pochhammer(n; k) = (n)_k = n (n+1) (n+2) ... (n+k-1)
;;;

(pochhammer 12 5)
; => 524160

(pochhammer 33 12)
; => 10102470716719180800


;;;
;;;   combinations n m   [Function]
;;;
;;; Computes the number of combinations of n things taken m at a time.
;;;
;;;         combinations(n,m) = \binom(n,m) = \frac{n!}{m!(n-m)!}.
;;;

(combinations 33 12)
; => 354817320

* (combinations 77 7)
; => 2404808340

;;;
;;;     newprime n     [Function]
;;;
;;; Returns the largest prime less than its argument.


(weyli::newprime 12)
; => 7

(weyli::newprime 1299)
; => 113

;;; **BUG?** never > 113 ....

;;; *** Need to point out that the elements of the second rational integer 
;;;     domain created are totally different from that those that are elements 
;;;     of the rst instance of the rational integers.)
;;;





