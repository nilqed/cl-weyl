<TeXmacs|2.1.4>

<style|<tuple|generic|british>>

<\body>
  <\session|sbcl|default>
    <\output>
      This is SBCL 1.3.4.15614.texmacs.1-0729f5c41-WIP, an implementation of
      ANSI Common Lisp.

      More information about SBCL is available at
      \<less\>http://www.sbcl.org/\<gtr\>.

      \;

      SBCL is free software, provided as is, with absolutely no warranty.

      It is mostly in the public domain; some portions are provided under

      BSD-style licenses. \ See the CREDITS and COPYING files in the

      distribution for more information.
    </output>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ql:quickload :weyl)
    <|unfolded-io>
      To load "weyl":

      \ \ Load 1 ASDF system:

      \ \ \ \ weyl

      ; Loading "weyl"

      ..................................

      (:WEYL)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (in-package :weyl)
    <|unfolded-io>
      #\<less\>PACKAGE "WEYL"\<gtr\>
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defvar x (coerce 'x *general*))

      (defvar y (coerce 'y *general*))

      (defvar z (coerce 'z *general*))

      (defvar p (coerce 'p *general*))

      (defvar q (coerce 'q *general*))

      (defvar r (coerce 'r *general*))

      \;
    <|unfolded-io>
      X

      Y
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (weyli::ge-variables *general*)
    <|unfolded-io>
      (r q p z y x v.1 x)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defvar ge1 (deriv (expt p q) q))
    <|unfolded-io>
      GE1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defvar ge2 (* x (expt y 2) (expt z 3) (sin x)))
    <|unfolded-io>
      GE2
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      ge1
    <|unfolded-io>
      (log(p)) p^q
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      ge2
    <|unfolded-io>
      z^3 y^2 x (sin(x))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv ge2 x)
    <|unfolded-io>
      z^3 y^2 (sin(x)) + (cos(x)) z^3 y^2 x
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv ge2 x x)
    <|unfolded-io>
      2 (cos(x)) z^3 y^2 - ((sin(x)) z^3 y^2 x)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv ge2 x y z)
    <|unfolded-io>
      6 z^2 y (sin(x)) + 6 z^2 y (cos(x)) x
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defun wtype (obj) (cl::type-of obj))

      \;
    <|unfolded-io>
      WTYPE
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defun slot-names (cls)

      \ \ (mapcar #'sb-mop::slot-definition-name\ 

      \ \ \ \ (sb-mop:class-slots (sb-mop::find-class cls ))))

      \;
    <|unfolded-io>
      SLOT-NAMES
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defun slot-iargs (cls)

      \ \ (mapcar #'sb-mop::slot-definition-initargs\ 

      \ \ \ \ (sb-mop:class-slots (sb-mop::find-class cls))))

      \ \ \ \ 
    <|unfolded-io>
      SLOT-IARGS
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defun slot-info (obj &key (prt t))

      \ \ (let* ((tobj (cl-user::type-of obj))

      \ \ \ \ \ \ \ \ \ (sn (slot-names tobj))

      \ \ \ \ \ \ \ \ \ (sv (map 'list (lambda (x) (slot-value obj x)) sn))

      \ \ \ \ \ \ \ \ \ (sa (slot-iargs tobj)))

      \ \ \ \ \ \ \ \ \ \ \ \ (format prt "Obj:Type : ~a : ~a ~%" obj tobj)

      \ \ \ \ \ \ \ \ \ \ \ \ (format prt "Names ...: ~{~a~^, ~} ~%" sn)

      \ \ \ \ \ \ \ \ \ \ \ \ (format prt "Values ..: ~{~a~^, ~} ~%" sv) \ 

      \ \ \ \ \ \ \ \ \ \ \ \ (format prt "InitArgs : ~{~a~^, ~} ~%~%" sa)))
    <|unfolded-io>
      SLOT-INFO
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-info p)
    <|unfolded-io>
      Obj:Type : p : GE-VARIABLE\ 

      Names \<ldots\>: PROPERTY-LIST, DOMAIN, SIMPLIFIED?, SYMBOL, STRING\ 

      Values ..: NIL, #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>, NIL, P, p\ 

      InitArgs : NIL, (DOMAIN), NIL, (SYMBOL), (STRING)\ 

      \;

      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-info (* p q))
    <|unfolded-io>
      Obj:Type : q p : GE-TIMES\ 

      Names \<ldots\>: DOMAIN, SIMPLIFIED?, TERMS\ 

      Values ..: #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>, NIL, (q p)\ 

      InitArgs : (DOMAIN), NIL, (TERMS)\ 

      \;

      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-info (expt p q))
    <|unfolded-io>
      Obj:Type : p^q : GE-EXPT\ 

      Names \<ldots\>: DOMAIN, SIMPLIFIED?, BASE, EXP\ 

      Values ..: #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>, NIL, p, q\ 

      InitArgs : (DOMAIN), NIL, (BASE), (EXP)\ 

      \;

      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-info (sin p) )
    <|unfolded-io>
      Obj:Type : sin(p) : GE-APPLICATION\ 

      Names \<ldots\>: DOMAIN, SIMPLIFIED?, FUNCT, ARGS\ 

      Values ..: #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>, NIL, sin, (p)\ 

      InitArgs : (DOMAIN), NIL, (FUNCT), (ARGS)\ 

      \;

      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-value (sin p) 'funct) \ 
    <|unfolded-io>
      sin
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-value (sin p) 'weyli::domain)
    <|unfolded-io>
      #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (slot-value (sin p) 'weyli::args)
    <|unfolded-io>
      (p)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (make-ge-variable *general* 'g)
    <|unfolded-io>
      g
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (weyli::ge-variables *general*)
    <|unfolded-io>
      (g r q p z y x v.1 x)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (substitute p q (* p q))
    <|unfolded-io>
      p^2
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (substitute p q (+ p q))
    <|unfolded-io>
      2 p
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (substitute 4 q (+ p q))
    <|unfolded-io>
      4 + p
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (substitute x \ q (+ p (sin (cos q)) ))
    <|unfolded-io>
      p + sin(cos(x))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ge-variable? p)
    <|unfolded-io>
      T
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (defvar f1 (weyli::make-app-function '(x y) (+ (* 'x 'y) (* 'x 'y
      'x))))
    <|unfolded-io>
      F1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      f1
    <|unfolded-io>
      (lambda (v.1 v.2) v.2 v.1^2 + v.2 v.1)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv f1 0)
    <|unfolded-io>
      (lambda (v.1 v.2) 2 v.2 v.1 + v.2)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv f1 1)
    <|unfolded-io>
      (lambda (v.1 v.2) v.1 + v.1^2)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (cl-user::type-of f1)
    <|unfolded-io>
      WEYLI::APPLICABLE-FUNCTION
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (apply f1 '(p q))
    <|unfolded-io>
      q p^2 + q p
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (apply (deriv f1 0) '(p q))
    <|unfolded-io>
      2 q p + q
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (documentation 'weyli::make-ge-variable 'function)
    <|unfolded-io>
      "Create a variable in a domain."
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (documentation 'weyli::coerce 'function)
    <|unfolded-io>
      "Coerce the element into the domain."
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (documentation 'weyli::expand 'function)
    <|unfolded-io>
      "Replaces all products of sums in exp by sums of products."
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (documentation 'weyli::memoize 'function)
    <|unfolded-io>
      "Performs the same functions as ``weyli::%memoize`` except that the
      domain

      \ \ \ used is ``*general*``."
    </unfolded-io>

    <\input>
      SBCL\<gtr\>\ 
    <|input>
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>