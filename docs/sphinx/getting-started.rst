Getting Started
===============

The Weyl user manual was written about 30 years ago, so it is not 
surprising when the loading of Common Lisp software has been simplified.


Installation
------------
It is assumed that you have `Quicklisp <https://www.quicklisp.org/beta/>`_
installed and configured, so that `(ql:quickload :package-name)` is working. 


Either change dir to your `quicklisp/local-projects`
::

    $ cd <quicklisp-directory>/local-projects
    $ git clone https://github.com/nilqed/weyl
    
or set a symbolic link after cloning
::

    $ git clone https://github.com/nilqed/weyl
    $ ln -s $PWD/matlisp <quicklisp-directory>/local-projects

where `<quicklisp-directory>` usually is `$HOME/quicklisp`.


Starting Weyl
-------------

Start your Common Lisp interpreter, then try `(ql:quickload :weyl)`:
::

    ~$ sbcl
    This is SBCL 2.2.9.debian, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    * (ql:quickload :weyl)
    To load "weyl":
      Load 1 ASDF system:
        weyl
    ; Loading "weyl"

    ;;; *************************************************************************
    ;;;   Infix notation for Common Lisp.
    ;;;   Version 1.3  28-JUN-96.
    ;;;   Written by Mark Kantrowitz, CMU School of Computer Science.
    ;;;   Copyright (c) 1993-95. All rights reserved.
    ;;;   May be freely redistributed, provided this notice is left intact.
    ;;;   This software is made available AS IS, without any warranty.
    ;;; *************************************************************************
    ..
    (:WEYL)
    * (in-package :weyl)
    #<PACKAGE "WEYL">
    * (cl-user::quit)
    ~$

If the package QUICK-LISP (nickname QL) is not known, then maybe a 
`(load "~/quicklisp/setup")` may help (assuming Quicklisp is installed
in your home directory.






