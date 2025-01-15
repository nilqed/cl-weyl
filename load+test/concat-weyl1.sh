#!/bin/bash

cp weyl1.lisp weyl1.backup
rm weyl1.lisp

cat ../closer-mop/closer-mop-packages.lisp  \
    ../closer-mop/closer-mop-shared.lisp  \
    ../closer-mop/closer-sbcl.lisp  \
    ../weyl/packages.lisp  \
    ../weyl/lisp-support.lisp  \
    ../weyl/domain-support.lisp  \
    ../weyl/classes/algebraic-domains.lisp  \
    ../weyl/classes/space-classes.lisp  \
    ../weyl/classes/general-classes.lisp  \
    ../weyl/avl.lisp  \
    ../weyl/lisp-numbers.lisp  \
    ../weyl/sets.lisp  \
    ../weyl/morphisms.lisp  \
    ../weyl/quotient-fields.lisp  \
    ../weyl/general.lisp \
    ../weyl/fourier.lisp  \
    ../weyl/functions.lisp  \
    ../weyl/direct-sums.lisp  \
    ../weyl/numbers/bigfloat.lisp  \
    ../weyl/numbers/numbers.lisp  \
    ../weyl/numbers/gfp.lisp  \
    ../weyl/polynomials/poly-tools.lisp  \
    ../weyl/polynomials/mpolynomial.lisp  \
    ../weyl/polynomials/upolynomial.lisp  \
    ../weyl/polynomials/epolynomial.lisp  \
    ../weyl/polynomials/sparsegcd.lisp  \
    ../weyl/polynomials/grobner.lisp  \
    ../weyl/tpower.lisp  \
    ../weyl/taylor.lisp  \
    ../weyl/rational-functions.lisp  \
    ../weyl/differential-domains.lisp  \
    ../weyl/algebraic-extension.lisp  \
    ../weyl/vector-spaces/vector.lisp  \
    ../weyl/vector-spaces/projective-space.lisp  \
    ../weyl/vector-spaces/quaternions.lisp  \
    ../weyl/matrix.lisp  \
    ../weyl/topology.lisp  \
    ../weyl/funct-spaces.lisp  \
    ../weyl/mesh.lisp   >> weyl1.lisp
    
echo weyl1.lisp created!
ls -l weyl1.lisp




