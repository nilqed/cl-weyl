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
      (ql::quickload :weyl)
    <|unfolded-io>
      To load "weyl":

      \ \ Load 1 ASDF system:

      \ \ \ \ weyl

      ; Loading "weyl"

      .

      (:WEYL)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (declaim (sb-ext:muffle-conditions cl:warning))
    <|unfolded-io>
      ((#\<less\>SB-KERNEL::CONDITION-CLASSOID WARNING\<gtr\> .
      MUFFLE-WARNING))
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
      (reset-domains)
    <|unfolded-io>
      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ge-variable? 'x)
    <|unfolded-io>
      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf ZZ (get-rational-integers))
    <|unfolded-io>
      Z
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf R (get-polynomial-ring ZZ '(x1 x2 x3)))
    <|unfolded-io>
      Z[x1, x2, x3]
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf x (coerce 'x1 R))

      (setf y (coerce 'x2 R))

      (setf z (coerce 'x3 R))
    <|unfolded-io>
      x1

      x2

      x3
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf r1 (* (expt (- x) 3) (+ y (* 2 z))))
    <|unfolded-io>
      (- x2 + -2 x3) x1^3
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (partial-deriv r1 x)
    <|unfolded-io>
      (-3 x2 + -6 x3) x1^2
    </unfolded-io>

    <\input>
      SBCL\<gtr\>\ 
    <|input>
      \;
    </input>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf QQ (get-rational-numbers))
    <|unfolded-io>
      Q
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf RR (get-real-numbers))
    <|unfolded-io>
      R
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf CC (get-complex-numbers))
    <|unfolded-io>
      C
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf Q4 (get-quaternion-domain QQ))
    <|unfolded-io>
      Quat(Q)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf a (coerce 1234567890 ZZ))
    <|unfolded-io>
      1234567890
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf b (coerce 127654 ZZ))
    <|unfolded-io>
      127654
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf c (/ a b))
    <|unfolded-io>
      \;

      #\<less\>SIMPLE-ERROR "No applicable contagion method for ~S and ~S"
      {1003193BF3}\<gtr\>\ 
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf q (coerce (/ 23 789) QQ))
    <|unfolded-io>
      23/789
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (* a q)
    <|unfolded-io>
      9465020490/263
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (prime? 77)
    <|unfolded-io>
      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (factor 1234567890)
    <|unfolded-io>
      ((2 . 1) (3 . 2) (5 . 1) (3607 . 1) (3803 . 1))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setq c 12345)
    <|unfolded-io>
      12345
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (factor c)
    <|unfolded-io>
      ((3 . 1) (5 . 1) (823 . 1))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (factorial 32)
    <|unfolded-io>
      263130836933693530167218012160000000
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (prime? 5678691)
    <|unfolded-io>
      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (totient 123)
    <|unfolded-io>
      80
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (pochhammer 123 6)
    <|unfolded-io>
      3905000064000
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (combinations 23 12)
    <|unfolded-io>
      1352078
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (newprime 2332)
    <|unfolded-io>
      \;

      #\<less\>UNDEFINED-FUNCTION NEWPRIME {1003510CB3}\<gtr\>\ 
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      q
    <|unfolded-io>
      23/789
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (floor q)
    <|unfolded-io>
      0

      23
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ceiling q)
    <|unfolded-io>
      1

      -766
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (truncate q)
    <|unfolded-io>
      0

      23
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (round q)
    <|unfolded-io>
      0

      23
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (+ b (* a q))
    <|unfolded-io>
      9498593492/263
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf q (coerce 'q *general*))
    <|unfolded-io>
      q
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      q
    <|unfolded-io>
      q
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf p (coerce 'p *general*))
    <|unfolded-io>
      p
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (cl-user::type-of q) \ 
    <|unfolded-io>
      WEYLI::GE-VARIABLE
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ge-variable? q)
    <|unfolded-io>
      T
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf ge1 (/ (* (+ 23 (* p q)) p) (- p q)))
    <|unfolded-io>
      (-1 q + p)^-1 (23 + q p) p
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf ge2 (* (+ p q) (- p q)))
    <|unfolded-io>
      (-1 q + p) (q + p)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify ge2)
    <|unfolded-io>
      (-1 q + p) (q + p)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv ge1 'p 'q 'p)
    <|unfolded-io>
      2 (-1 q + p)^-1 - (2 (-1 q + p)^-2 q) - (4 (-1 q + p)^-2 p) + 4 (-1 q +
      p)^-3 (23 + q p) + 4 (-1 q + p)^-3 q p + 2 (-1 q + p)^-3 p^2 - (6 (-1 q
      + p)^-4 (23 + q p) p)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand ge2)
    <|unfolded-io>
      -1 q^2 + p^2
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf ge3 (/ ge1 ge2))
    <|unfolded-io>
      (-1 q + p)^-1 (23 + q p) p ((-1 q + p) (q + p))^-1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify ge3)
    <|unfolded-io>
      (-1 q + p)^-1 (23 + q p) p ((-1 q + p) (q + p))^-1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand ge3)
    <|unfolded-io>
      0
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify (deriv ge1 'p 'q 'p))
    <|unfolded-io>
      2 (-1 q + p)^-1 - (2 (-1 q + p)^-2 q) - (4 (-1 q + p)^-2 p) + 4 (-1 q +
      p)^-3 (23 + q p) + 4 (-1 q + p)^-3 q p + 2 (-1 q + p)^-3 p^2 - (6 (-1 q
      + p)^-4 (23 + q p) p)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand (deriv ge1 'p 'q 'p))
    <|unfolded-io>
      0
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify (/ p p))
    <|unfolded-io>
      1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify (/ ge2 ge2))
    <|unfolded-io>
      (-1 q + p) (q + p) ((-1 q + p) (q + p))^-1
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand (/ ge2 ge2))
    <|unfolded-io>
      0
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (reset-domains)

      (setf R *general*)
    <|unfolded-io>
      NIL

      #\<less\>Domain: GENERAL-EXPRESSIONS\<gtr\>
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf u (coerce 'u R))

      (setf v (coerce 'v R))

      (setf w (coerce 'w R))
    <|unfolded-io>
      u

      v

      w
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (ge-variable? v)
    <|unfolded-io>
      T
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (add-subscripts u '(1 2 'a))
    <|unfolded-io>
      u((1 2 'A))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (get-variable-property R u 'key)
    <|unfolded-io>
      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (declare-dependencies u v w)
    <|unfolded-io>
      (w v)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (depends-on? u v)
    <|unfolded-io>
      T
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf x (+ 2 (* u v)))
    <|unfolded-io>
      2 + v u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (different-kernels x '(v))
    <|unfolded-io>
      (u v V)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (different-kernels x (list v))\ 
    <|unfolded-io>
      (u v)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (setf xx (simplify (- x x)))
    <|unfolded-io>
      2 - (2 + v u) + v u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (simplify xx)
    <|unfolded-io>
      2 - (2 + v u) + v u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand xx)
    <|unfolded-io>
      0
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv x 'v)
    <|unfolded-io>
      u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (deriv (* x x) 'v)
    <|unfolded-io>
      2 (2 + v u) u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand (deriv (* x x) 'v))
    <|unfolded-io>
      2 v u^2 + 4 u
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (expand xx)
    <|unfolded-io>
      0
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (let ((count 0)) (permute '(a b c d) (p) (print p) (incf count))

      \ \ \ \ (format t "~%~D permutations total. ~%" count))
    <|unfolded-io>
      \;

      (D C B A)\ 

      (C D B A)\ 

      (D B C A)\ 

      (B D C A)\ 

      (C B D A)\ 

      (B C D A)\ 

      (D C A B)\ 

      (C D A B)\ 

      (D A C B)\ 

      (A D C B)\ 

      (C A D B)\ 

      (A C D B)\ 

      (D B A C)\ 

      (B D A C)\ 

      (D A B C)\ 

      (A D B C)\ 

      (B A D C)\ 

      (A B D C)\ 

      (C B A D)\ 

      (B C A D)\ 

      (C A B D)\ 

      (A C B D)\ 

      (B A C D)\ 

      (A B C D)\ 

      24 permutations total.\ 

      NIL
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (weyli:partition (l 10) (print l))
    <|unfolded-io>
      \;

      (1 1 1 1 1 1 1 1 1 1)\ 

      (2 1 1 1 1 1 1 1 1)\ 

      (3 1 1 1 1 1 1 1)\ 

      (2 2 1 1 1 1 1 1)\ 

      (4 1 1 1 1 1 1)\ 

      (3 2 1 1 1 1 1)\ 

      (5 1 1 1 1 1)\ 

      (2 2 2 1 1 1 1)\ 

      (4 2 1 1 1 1)\ 

      (3 3 1 1 1 1)\ 

      (6 1 1 1 1)\ 

      (3 2 2 1 1 1)\ 

      (5 2 1 1 1)\ 

      (4 3 1 1 1)\ 

      (7 1 1 1)\ 

      (2 2 2 2 1 1)\ 

      (4 2 2 1 1)\ 

      (3 3 2 1 1)\ 

      (6 2 1 1)\ 

      (5 3 1 1)\ 

      (4 4 1 1)\ 

      (8 1 1)\ 

      (3 2 2 2 1)\ 

      (5 2 2 1)\ 

      (4 3 2 1)\ 

      (7 2 1)\ 

      (3 3 3 1)\ 

      (6 3 1)\ 

      (5 4 1)\ 

      (9 1)\ 

      (2 2 2 2 2)\ 

      (4 2 2 2)\ 

      (3 3 2 2)\ 

      (6 2 2)\ 

      (5 3 2)\ 

      (4 4 2)\ 

      (8 2)\ 

      (4 3 3)\ 

      (7 3)\ 

      (6 4)\ 

      (5 5)\ 

      (10)\ 
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (require :sb-introspect)
    <|unfolded-io>
      ("SB-INTROSPECT")
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (sb-introspect::who-calls \ 'weyli:ge-variable? )
    <|unfolded-io>
      ((WEYLI::STANDARD-DERIVATION

      \ \ \ \ \ \ \ \ \ \ \ \ . #S(SB-INTROSPECT:DEFINITION-SOURCE

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PATHNAME
      #P"/home/kfp/quicklisp/local-projects/weyl/differential-domains.lisp"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-PATH (12)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-NUMBER 20

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :CHARACTER-OFFSET 5097

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FILE-WRITE-DATE 3938251414

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PLIST NIL

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :DESCRIPTION NIL))

      \ \ \ \ \ \ \ \ \ \ \ (WEYLI::STANDARD-DERIVATION

      \ \ \ \ \ \ \ \ \ \ \ \ . #S(SB-INTROSPECT:DEFINITION-SOURCE

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PATHNAME
      #P"/home/kfp/quicklisp/local-projects/weyl/differential-domains.lisp"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-PATH (12)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-NUMBER 33

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :CHARACTER-OFFSET 5097

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FILE-WRITE-DATE 3938251414

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PLIST NIL

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :DESCRIPTION NIL))

      \ \ \ \ \ \ \ \ \ \ \ (WEYLI::SAFE-DISPLAY

      \ \ \ \ \ \ \ \ \ \ \ \ . #S(SB-INTROSPECT:DEFINITION-SOURCE

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PATHNAME
      #P"/home/kfp/quicklisp/local-projects/weyl/general.lisp"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-PATH (100)

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FORM-NUMBER 14

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :CHARACTER-OFFSET 20112

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :FILE-WRITE-DATE 3938251414

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :PLIST NIL

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ :DESCRIPTION NIL)))
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      \ (sb-introspect::function-lambda-list \ 'weyli:deriv)
    <|unfolded-io>
      (WEYLI::EXPRESSION &REST WEYLI::VARIABLES)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      \ (sb-introspect::function-type \ \ 'weyli:deriv)
    <|unfolded-io>
      (FUNCTION (T &REST T) COMMON-LISP:*)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      \ (sb-introspect::function-type \ \ 'weyli:expand)
    <|unfolded-io>
      (FUNCTION (T) COMMON-LISP:*)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      \ (sb-introspect::find-function-callers \ 'weyli:expand)
    <|unfolded-io>
      (#\<less\>FUNCTION MAKE-APP-FUNCTION\<gtr\>

      \ \ \ \ \ \ \ \ \ \ \ #\<less\>FUNCTION (SB-PCL::FAST-METHOD EXPAND
      (WEYLI::GE-EXPT))\<gtr\>

      \ \ \ \ \ \ \ \ \ \ \ #\<less\>FUNCTION (SB-PCL::FAST-METHOD EXPAND
      (WEYLI::GE-PLUS))\<gtr\>

      \ \ \ \ \ \ \ \ \ \ \ #\<less\>FUNCTION WEYLI::EXPAND-PRODUCT1\<gtr\>)
    </unfolded-io>

    <\unfolded-io>
      SBCL\<gtr\>\ 
    <|unfolded-io>
      (cl-user::quit)
    <|unfolded-io>
      <script-busy>
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