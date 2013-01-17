# Beagle #
======

Beagle is a minimalist dialect of Lisp inspired by McCarthy's original Lisp, 
lambda calculus, and 
[M-Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.40.4948).


# Getting Started #

There are two quick and easy ways to start using Beagle:

 1. try out the [online REPL](http://mattfenwick.github.com/Beagle/src/repl/repl.html)

 2. pull the github repo, and start a local REPL by opening `src/repl/repl.html`
    in a web browser
    

## Some key points ##

Beagle is similar to other Lisps:

 - semantics based on the lambda calculus

 - prefix notation
 
 - first-class functions


but also lacks some characteristic features:

 - macros
 
 - quote/eval
 
 - homoiconicity

Its concrete reduces syntax overloading by differentiating between:

 - function application `(a b c)`,
 - list literals `[a b c]`
 - object literals `{a b c d}`
 - special form application `,(define a b,)`.
