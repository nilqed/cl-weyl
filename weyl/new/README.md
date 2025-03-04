# New files

* ge-support
* ge-latex
* weyl-inifx


## ge-support

in package :weyl

ge-var

    (documentation 'ge-var 'function)
    "Define a variable in the general domain (*general*). This is equivalent to
    (defvar v (coerce 'v *general*)). (CL::TYPE-OF v) ==> WEYLI::GE-VARIABLE."

ge-vars

    (documentation 'ge-vars 'function)
    "Define general variables from the list vl, that is looping of <ge-var> over
    vl. Examples: (ge-vars '(x_0 x_1 x_2 x_3)), (ge-vars '(x y z p q r))."


list-ge-vars

    (documentation 'list-ge-vars 'function)
    "List all defined variables in domain *general*. One may check memmbership
    with (ge-variable? v)."

set-latex-repr

    (documentation 'set-latex-repr 'function)
    "Add a LaTeX representation ltx (string '\\...') to a ge-variable var.
    Example: (set-latex-repr a \"\\alpha\")." 

`+->`
 
    (documentation '+-> 'function)
    "Convert a string containing a valid GE expression to prefix form.
    Examples: (+-> \"p+q\") ==> (+ p q), (+-> \"p*q^2\") ==> (* p (expt q 2))."


## ge-latex

latex




## weyl-infix
