Beagle is a dialect of Lisp.  It is based loosely on the lambda calculus.

It has a small number of built-in special forms.  Each of these has its own evaluation rules.
Other than the special forms, Beagle is an applicative order language:  arguments are evaluated
before function application. 

It lacks many features of other Lisps, notably the `quote` and `eval` special forms and macros.
This is because these are not especially useful outside of metalinguistic programming; for a 
more complete description, see M-Lisp, another Lisp dialect from which Beagle drew
heavy inspiration.

Additionally, Beagle is lexically scoped and dynamically typed.