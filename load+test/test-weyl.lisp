(load "load-weyl")
(load "../weyl/test/defpackage")
(load "lisp-unit")
(load "../weyl/test/f-and-g-series")
(load "../weyl/test/combinatorial-tools")

 (in-package :weyl-test)
 (lisp-unit:run-tests :all)
 
 (print "again ....")
 (lisp-unit:run-tests '(permute choose f-and-g-series))
