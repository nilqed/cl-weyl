Introduction
============

In the last twenty years the algorithms and techniques for manipulating symbolic mathematical
quantities have improved dramatically. These techniques have been made available to a practi-
tioners through a number of algebraic manipulation systems. Among the most widely distributed
systems are Macsyma [9], Reduce [3], Maple [5] and Mathematica [10]. These systems are designed
to be self-contained and are not intended to be incorporated into larger, more specialized systems.
This is at odds with the experience with numerical computation where libraries of carefully coded
routines like Linpack [2] and Eispack [7] have been of the greatest value because they could be
incorporated in larger systems (like uid dynamics simulators or circuit analyzers). Linear algebra
systems like Matlab [6], though of signi cant value, where developed much later and tend to be
used more for research in linear algebra than as part of large computations.

Another limitation of current symbolic mathematics systems is that they generally deal with
a relatively limited and xed set of algebraic types, which the user is not expected to extend
signi cantly. Thus it may be difficult for a user to experiment with algorithms for dealing with
Poisson series if the algebra system does not already have a data type that matches the behavior of
Poisson series. This situation is exacerbated by the unavailability of the code that implements the
algebraic algorithms of the symbolic manipulation system. Scratchpad [4] is a noticeable exception
to this trend in that the developers plan to make the algebra code for the system widely available,
and the internal data typing mechanisms provided by Scratchpad are designed with the extension
mechanisms just mentioned in mind.

Weyl is an extensible algebraic manipulation substrate that has been designed to represent
all types of algebraic objects. It deals not only with the basic symbolic objects like polynomials,
algebraic functions and di erential forms, but can also deal with higher level objects like groups,
rings, ideals and vector spaces. Furthermore, to encourage the use of symbolic techniques within
other applications, Weyl is implemented as an extension of Common Lisp [8] using the Common
Lisp Object Standard [1] so that all of Common Lisp's facilities and development tools can be used
in concert with Weyl's symbolic tools.

It should be noted that the initial implementation of Weyl is intended to be as clean and
semantically correct as possible. The primary goal of Weyl is provide the tools needed to express
algebraic algorithms naturally and succinctly. Nonetheless, we believe that algebraic algorithms
can be efficiently implemented within the Weyl framework even though that was not a goal of the
initial implementation.

The Domain Concept
------------------

One of the novel concepts in Weyl is that of a domain . This section gives three examples that
illustrate the need for domains.
Consider the problem of integrating the function :math:`\frac{1}{(x^3-2)}`:

.. math::

    \int \frac{dx}{x^3-2} = 
    
  
   



