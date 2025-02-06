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




    
