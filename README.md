# Beagle #
======

Beagle is a minimalist dialect of Lisp inspired by McCarthy's original Lisp, 
lambda calculus, and 
[M-Lisp](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.40.4948).


# Getting Started #

There are two quick and easy ways to start using Beagle:

 1. try out the [online REPL](http://mattfenwick.github.com/Beagle)

 2. pull the github repo, and start a local REPL by opening `src/javascript/index.html`
    in a web browser
    

## Some key points ##

Beagle is similar to other Lisps:

 - semantics based on the lambda calculus

 - prefix notation
 
 - first-class functions
 
 - dynamically typed
 
 - lexically scoped


but also lacks some characteristic features:

 - macros
 
 - quote/eval
 
 - homoiconicity

Its concrete syntax reduces token overloading by differentiating between:

 - function application uses parentheses: `(a b c)`
 - list literals use square braces: `[a b c]`
 - special form application, as in `{define a b}`, 
   and syntactic grouping, as in `{cond {{(> x 3) [1]}} [1 2]}`
   both use curly braces
