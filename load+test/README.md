## Load + Test

Loading the source files:

    sbcl --load "load-weyl"
    
Performing the tests (incl. loading)

    sbcl --load "test-weyl"
    

Current state:

    Unit Test Summary
    | 22 assertions total
    | 22 passed
    | 0 failed
    | 1 execution errors
    | 0 missing tests


### Creating weyl1.lisp (all in one file)

    ./concat-weyl1.sh 
    weyl1.lisp created!
    -rw-rw-r-- 1 kfp kfp 774474 Jan 15 13:55 weyl1.lisp

This file will be used for inclusion into __FriCAS__.
