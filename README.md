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
 
 - support for functional programming constructs such as higher order functions
 
 - no distinction between operators and functions, or between built-in 
   functions and user-defined functions
 
 - lack of implicit operator/function precedence:  all precedences are
   made explicit using delimiters

but it also lacks some characteristic features:

 - macros
 
 - quote/eval
 
 - homoiconicity
 
Also, Beagle's concrete syntax differentiates between function application `()`,
list literals `[]`, and special form application `{}`.



## Why doesn't Beagle have X? ##
  
Beagle lacks the 3 major features mentioned above because, in my opinion, their
drawbacks outweigh their advantages.  Perhaps someday I will understand them 
better and realize that they're actually crucial, but for now, here are the major
problems I have with them:

 - macro system
 
   - difficult to use correctly and effectively by all but the very best programmers
   
   - changes focus from building composable abstractions to creating 
     quick-n-dirty, one-off syntax hacks 
 
   - difficult for novices to learn and understand
   
   - usage of macros is visually identical to usage of functions, but
     has very different limitations and results
   
 - eval/quote and homoiconicity

   - complicates formal semantics; makes informal semantics very difficult to describe
 
   - blends abstract syntax with evaluated result, increasing semantic confusion,
     especially for beginners
     
   - complicates both implementation and explanation by necessitating the 
     introduction of "self-evaluating forms" and so on
     
   - not actually necessary for writing production programs
   
   - different things look the same:  function application, special form application, 
     parameters lists to lambda, predicate/result pairs to cond, and evaluated lists
     all look similar, but represent 5 totally different concepts, each with its own
     distinctive evaluation rules
      
Thus, as did [Wadler](http://www.wisdomandwonder.com/link/1055/why-calculating-is-better-than-scheming)
and the creators of M-Lisp, I tip my hat respectfully to features 4-6, but choose to
omit them to instead focus on the expressive power of lambda calculus, functional
programming, and a more explicit syntax.



## Beagle goals ##

 - minimal but complete core

 - no built-in sugar

 - no special cases

 - focus on composable programming

 - deemphasize computational efficiency

