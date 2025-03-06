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
