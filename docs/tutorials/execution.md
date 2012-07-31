# Running Lisp code:  from String to result #
---------

How is Lisp code executed?  How does the interpreter take a string,
run some code, and create a result?  How are macros used and applied?

In this article, we'll look at how Beagle, a simple Lisp dialect,
does just this.

We'll divide the process into two major stages:

 - parsing or string interpretation, which involves creating Beagle
   data structures from a string

 - evaluation, which involves transforming Beagle data structures
   into reduced Beagle data structures

Throughout the article, we'll follow the progress of this input string,
assuming that `(define x "new")` has already been executed:

    (cons x ; a comment
     (list   1 "hi there" 3))


## Parsing ##

 - step 1: tokenization

        OPEN       : '('
        symbol     : 'cons'
        whitespace : ' '
        symbol     : 'x'
        whitespace : ' '
        comment    : ' a comment'
        whitespace : '\n '
        OPEN       : '('
        symbol     : 'list'
        whitespace : '   '
        symbol     : 1
        whitespace : ' '
        string     : 'hi there'
        whitespace : ' '
        symbol     : '3'
        CLOSE      : ')'
        CLOSE      : ')'

 - step 2:  whitespace check. passes

 - step 3:  strip whitespace and comment tokens

        OPEN       : '('
        symbol     : 'cons'
        symbol     : 'x'
        OPEN       : '('
        symbol     : 'list'
        symbol     : 1
        string     : 'hi there'
        symbol     : '3'
        CLOSE      : ')'
        CLOSE      : ')'

 - step 4:  build s-expressions

        SList:
          - SSymbol: 'cons'
          - SSymbol: 'x'
          - SList:
            - SSymbol: 'list'
            - SSymbol: '1'
            - SString: 'hi there'
            - SSymbol: '3'

 - step 5:  create Beagle objects

        BList:
          - BSymbol: 'cons'
          - BSymbol: 'x'
          - BList:
            - BSymbol: 'list'
            - BNumber: 1
            - BString: 'hi there'
            - BNumber: 3

## Evaluation ##

 - step 6:  macro expansion.  There are currently **no** macros in
   Beagle, and this may never change.

 - step 7:  evaluate/reduce data structure.  The evaluation rule is
   that if it's a list, we evaluate each of the elements, and then 
   apply the first element as a function with the rest of the list
   as its arguments:

        BList:
          - primitive function 'cons'
          - BString: "new"
          - BList:
            - BNumber: 1
            - BString: 'hi there'
            - BNumber: 3

  final evaluated value:

        BList:
          - BString: "new"
          - BNumber: 1
          - BString: 'hi there'
          - BNumber: 3


## Wrap up ##

We saw how the process of execution of Beagle code works.  What we didn't see
was how special forms and built-in forms are evaluated.  That may be the subject
of a future article.

        
          