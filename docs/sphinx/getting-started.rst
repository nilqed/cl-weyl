Getting Started
===============

Weyl is implemented as a large body of functions that extend Lisp's functionality to include non-
numeric algebraic computations. These functions can be loaded into an existing Lisp image using
the tools described in Section 2.1. All of Weyl's extensions are attached to symbols in the weyl or
weyli packages. The philosophy behind these packages and how they are to be used is discussed
in Section 2.2. Some issues with developing programs using Weyl are discussed in Section 2.3. In
Section 2.4 we give a simple of example what is involved when using Weyl.

Creating a Weyl World
---------------------
Common Lisp does not include a defsystem facility, which is often used to manage the compilation
dependencies between di erent les in a Lisp system.1 Although many Lisp vendors provide a
defsystem, there is no standard, and each vendor's version di ers slightly from the others. As a
consequence, we have chosen not to use any of them and instead include with Weyl a copy of Mark
Kantrowitz's version.



