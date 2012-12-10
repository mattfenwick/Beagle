# Beagle #
======

Beagle is a minimalist dialect of Lisp inspired by McCarthy's original Lisp, 
lambda calculus, and 
[M-Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.40.4948).


# Getting Started #

There are two quick and easy ways to start using Beagle:

 1. try out the [online REPL](http://mattfenwick.github.com/Beagle/src/repl/repl.html)

 2. pull the github repo, and set up a local REPL.  
    The Javascript implementation can run from local files in any web browser;
    once you've pulled all the files, just open up `src/repl/repl.html`
    

## Some key points ##

Beagle is similar to other Lisps:

 - semantics based on the lambda calculus

 - prefix notation
 
 - first-class support for functions
 
 - no distinction between operators, built-in functions and
   user-defined functions
 
but it also lacks some characteristic features:

 - macros
 
 - quote/eval
 
 - homoiconicity
 
Also, Beagle's concrete syntax differentiates between function application `()`,
list literals `[]`, and special form application `{}`.
