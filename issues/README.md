Bugs & not implemented
======================

Bug in function `simp-times-terms (general.lisp)`
-------------------------------------------------

    (setf pq (expt p q))  ;--> p^q
    (class-of pq) ;--> #<STANDARD-CLASS WEYLI::GE-EXPT>

    (deriv pq p) ;--> q p
    (class-of (deriv pq p)) ;--> #<STANDARD-CLASS WEYLI::GE-TIMES> 
    
The error is not in  `ge-deriv`, it comes from simplification:

           ;;;; 
			 ((ge-expt? term)
			  (let ((exp (exponent-of term))
				(base (base-of term)))
			    (cond ((number? (exponent-of term))
				   (add-term (list base) exp))
				  (t (add-term (list base)
					       ;orig-- (make-element domain 1)
                           ;-- exp  // not simplified
                           (simplify exp)   ;; ++ simplified 
                           ) 
                              ))) )
            ;;;;

A quick fix might be replacing `(make-element domain 1)` by `(simplify exp)`, however this
has to be tested more carefully.

Not implemented in `ge-deriv` 
-----------------------------

     (deriv pq q) ;--> #<SIMPLE-ERROR "Not yet implemented" {1003CBC7D3}>
     
This is in

    (defmethod-sd ge-deriv ((exp ge-expt) (var ge-atom))
       (let ((base (base-of exp))
	         (power (exponent-of exp)))
             (cond ((depends-on? power var)
	          (error "Not yet implemented / !!!!!!!"))
	         ((and (number? power) (= power 2))
	          (* 2 base (ge-deriv base var)))
	            (t (* power (expt base (- power 1)))))))
	            
and ...



Rewrite ge-deriv ... 

    (defmethod-sd ge-deriv ((exp ge-expt) (var ge-atom))
      (let ((base (base-of exp))
	    (power (exponent-of exp)))
          (+ (* (log base) (expt base power) (ge-deriv power var))
          (* power (expt base (- power 1)) (ge-deriv base var)))))


Better now:


        Unit Test Summary
         | 12 assertions total
         | 12 passed
         | 0 failed
         | 0 execution errors
         | 0 missing tests

        Unit Test Summary
         | 22 assertions total
         | 22 passed
         | 0 failed
         | 0 execution errors
         | 0 missing tests

        Unit Test Summary
         | 16 assertions total
         | 14 passed
         | 2 failed
         | 0 execution errors
         | 0 missing tests

         | Failed Form: (GE-EQUAL GE4 GE5)
         | Expected T but saw NIL
         | GE4 => (-1 q + p) (-1 q^2 + p^2)^-1 (q + p)
         | GE5 => 1
         |
         | Failed Form: (GE-EQUAL GE1 GE2)
         | Expected T but saw NIL
         | GE1 => (-1 q + p) (q + p)
         | GE2 => -1 q^2 + p^2
         |
        GE-BASICS: 3 assertions passed, 2 failed.

        Unit Test Summary
         | 16 assertions total
         | 14 passed
         | 2 failed
         | 0 execution errors
         | 0 missing tests


            

## Taylor series (tpower.lisp)
The problem is that `map array` is used (ARRAY is a bad type specifier for sequences.)
Changing in tpower.lisp to `map vector` resolves issue.

    debugger invoked on a SIMPLE-TYPE-ERROR in thread
    #<THREAD "main thread" RUNNING {10048B8113}>:
    ARRAY is a bad type specifier for sequences.


    (SB-KERNEL:%MAP ARRAY #<FUNCTION (LAMBDA (WEYLI::E) :IN WEYLI::MAKE-TPOWER-SERIES) {100395F15B}> (1 0 -1/6 0 1/120 0 -1/5040 0))0]



## Bug in expand[ge-expt] (general.lisp) 
Line 1550 ff.

    (ge-vars '(x y z p q r a b c))
    (defvar ge1 (+-> "(x+y)^3+2*p/(1-q^2)+a^r"))
    (defvar ge2 (+ (expt (+ x y) 3) (* 2 p (expt (- 1 (expt q 2)) -1)) (expt a r)))



    ;* ge1
    ;(y + x)^3 + 2 (1 - q^2)^-1 p + a^r
    ;* (expand ge1)
    ;3 y x^2 + y^3 + 3 y^2 x + x^3 + a^r
    ; where is 2*p/(1-q^2) ????
    ;(ge-equal ge1 ge2)
    ; T
    ;* (ge-equal ge1 (expand ge2))
    ;NIL

    ; severe bug ********
    ;(expand (+-> "1/(1-q^2)"))
    ; 0
    ; 
    ;(expand (+-> "2*p/(1-q^2)"))
    ;0

    ; (wtype  (+-> "1/(1-q^2)"))
    ; WEYLI::GE-EXPT
    ; ==> (defmethod expand ((exp ge-expt)) l. 1545
    ; 
    #|
    (defmethod expand ((exp ge-expt))
      (let ((base (base-of exp))
        (exponent (exponent-of exp)))
        (cond ((and (ge-plus? base)
            (typep exponent 'rational-integer))
           (simp-plus-terms (domain-of exp)
                                (expand-binomial-form (terms-of base)
                                                      (integer-value exponent))))
          ((ge-times? base)
           (expand (simp-times-terms (domain-of exp)
                                         (loop for term in (terms-of base)
                                               collect (expand (expt term exponent))))))
          (t exp))))
          
    |#



    (defvar gx1 (+-> "1/(1-q^2)"))
    (defvar egx1 (exponent-of gx1))
    ;-1
    (defvar bgx1 (base-of gx1))
    ;1 - q^2

    (ge-plus? bgx1)
    ;T

    (typep egx1 'rational-integer)
    ;T

    ;(simp-plus-terms (domain-of exp)
    ;    (expand-binomial-form (terms-of base)
    ;        (integer-value exponent)))

    (weyli::integer-value egx1)
    ;-1

    (weyli::expand-binomial-form (terms-of bgx1) (weyli::integer-value egx1))
    ; NIL --- that's bad

    (weyli::domain-of gx1)
    ;#<Domain: GENERAL-EXPRESSIONS>

    (weyli::simp-plus-terms (weyli::domain-of gx1) nil)
    ; 0

    ;;; error :: (typep exponent 'rational-integer)) 
    ;;; exponent must be positive, better >=2 of course ....



Fixed


    ;; [-] (typep exponent 'rational-integer)
    ;; [+] (if (typep exponent 'rational-integer) (>= exponent 2) nil)
  

In matrix.lisp:

    (defvar M (get-matrix-space (get-rational-integers)))
     => M
   
gives error (make-space-print-object not defined)
   
    ;;; 22-MAR-2025/kfp
    ;;; replaced make-space-print-object by matrix-space-print-object
    ;;; in define-domain-creator matrix-space
