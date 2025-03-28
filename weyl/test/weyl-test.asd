;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-
#|

 ===========================================================================
			    Weyl ASDF Definition
 ===========================================================================
 (c) Copyright 1989, 1993 Cornell University

+ ge-tests

|#

(in-package :asdf)

(defsystem :weyl-test
  :description "WEYL Unit Testing"
  :version "0.1.0"
  :license "Custom"
  :depends-on ("lisp-unit" "weyl")
  :components
  ((:file "defpackage")
   (:file "ge-tests" :depends-on ("defpackage"))
   (:file "combinatorial-tools" :depends-on ("defpackage"))
   (:file "f-and-g-series" :depends-on ("defpackage"))
   (:file "test")))
