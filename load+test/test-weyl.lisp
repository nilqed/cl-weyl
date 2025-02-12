(load "load-weyl")
(load "../weyl/test/defpackage")
(load "lisp-unit")
(load "../weyl/test/f-and-g-series")
(load "../weyl/test/combinatorial-tools")
(load "../weyl/test/ge-tests")

 (in-package :weyl-test)
 (lisp-unit:run-tests :all)
 
 (print "again ....")
 (lisp-unit:run-tests '(permute choose f-and-g-series))
 
 (defvar db (lisp-unit::run-tests '(ge-basics ge-deriv)))
 
(lisp-unit:test-names db)
(lisp-unit:print-failures db)
(lisp-unit:failed-tests db)
(lisp-unit:error-tests db)
(lisp-unit:print-errors db)
(lisp-unit:summarize-results db) 
