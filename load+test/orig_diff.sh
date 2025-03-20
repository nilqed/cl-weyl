#!/bin/sh
# for a in ../simlab/weyl/src/*.lisp; 
# do echo diff $a  ../weyl/$(basename $a)   ; done > orig_diff.sh

echo Changes to original SimLab distribution of Weyl
echo ===============================================
echo $(date)
echo

alias diff='diff -s -y -b -B -Z -E -i --width=170'

diff ../simlab/weyl/src/algebraic-domains.lisp ../weyl/classes/algebraic-domains.lisp
diff ../simlab/weyl/src/algebraic-extension.lisp ../weyl/algebraic-extension.lisp
diff ../simlab/weyl/src/avl.lisp ../weyl/avl.lisp
diff ../simlab/weyl/src/bigfloat.lisp ../weyl/numbers/bigfloat.lisp
diff ../simlab/weyl/src/differential-domains.lisp ../weyl/differential-domains.lisp
diff ../simlab/weyl/src/direct-sums.lisp ../weyl/direct-sums.lisp
diff ../simlab/weyl/src/domain-support.lisp ../weyl/domain-support.lisp
diff ../simlab/weyl/src/epolynomial.lisp ../weyl/polynomials/epolynomial.lisp
diff ../simlab/weyl/src/fourier.lisp ../weyl/fourier.lisp
diff ../simlab/weyl/src/functions.lisp ../weyl/functions.lisp
diff ../simlab/weyl/src/funct-spaces.lisp ../weyl/funct-spaces.lisp
diff ../simlab/weyl/src/general-classes.lisp ../weyl/classes/general-classes.lisp
diff ../simlab/weyl/src/general.lisp ../weyl/general.lisp
diff ../simlab/weyl/src/gfp.lisp ../weyl/numbers/gfp.lisp
diff ../simlab/weyl/src/grobner.lisp ../weyl/polynomials/grobner.lisp
diff ../simlab/weyl/src/lisp-numbers.lisp ../weyl/lisp-numbers.lisp
diff ../simlab/weyl/src/lisp-support.lisp ../weyl/lisp-support.lisp
diff ../simlab/weyl/src/maintenance.lisp ../weyl/maintenance.lisp
diff ../simlab/weyl/src/matrix.lisp ../weyl/matrix.lisp
diff ../simlab/weyl/src/mesh.lisp ../weyl/mesh.lisp
diff ../simlab/weyl/src/morphisms.lisp ../weyl/morphisms.lisp
diff ../simlab/weyl/src/mpolynomial.lisp ../weyl/polynomials/mpolynomial.lisp
diff ../simlab/weyl/src/multipole.lisp ../weyl/multipole.lisp
diff ../simlab/weyl/src/new-domains.lisp ../weyl/new-domains.lisp
diff ../simlab/weyl/src/new-topology.lisp ../weyl/new-topology.lisp
diff ../simlab/weyl/src/numbers.lisp ../weyl/numbers/numbers.lisp
diff ../simlab/weyl/src/packages.lisp ../weyl/packages.lisp
diff ../simlab/weyl/src/poly-tools.lisp ../weyl/polynomials/poly-tools.lisp
diff ../simlab/weyl/src/projective-space.lisp ../weyl/vector-spaces/projective-space.lisp
diff ../simlab/weyl/src/quaternions.lisp ../weyl/vector-spaces/quaternions.lisp
diff ../simlab/weyl/src/quotient-fields.lisp ../weyl/quotient-fields.lisp
diff ../simlab/weyl/src/rational-functions.lisp ../weyl/rational-functions.lisp
diff ../simlab/weyl/src/sets.lisp ../weyl/sets.lisp
diff ../simlab/weyl/src/space-classes.lisp ../weyl/classes/space-classes.lisp
diff ../simlab/weyl/src/sparsegcd.lisp ../weyl/polynomials/sparsegcd.lisp
diff ../simlab/weyl/src/taylor.lisp ../weyl/taylor.lisp
diff ../simlab/weyl/src/topology.lisp ../weyl/topology.lisp
diff ../simlab/weyl/src/tpower.lisp ../weyl/tpower.lisp
diff ../simlab/weyl/src/upolynomial.lisp ../weyl/polynomials/upolynomial.lisp
diff ../simlab/weyl/src/vector.lisp ../weyl/vector-spaces/vector.lisp
diff ../simlab/weyl/src/walk.lisp ../weyl/walk.lisp

