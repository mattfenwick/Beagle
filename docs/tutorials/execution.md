# Running Lisp code:  from String to result #
---------

How is Lisp code executed?  How does the interpreter take a string,
run some code, and create a result?  How are macros used and applied?

In this article, we'll look at how Beagle, a simple Lisp dialect,
does just this.

We'll divide the process into two major stages:

 - parsing or string interpretation, which involves creating Beagle
   data structures from a string (AST construction)

 - evaluation, which involves transforming Beagle data structures
   into reduced Beagle data structures (AST reduction)

Throughout the article, we'll follow the progress of this input string:

    (cons 4 ; a comment
      [   1 "hi there" 3])


## Parsing ##

 - step 1: tokenization

        OPEN-PAREN   : '('
        symbol       : 'cons'
        whitespace   : ' '
        integer      : '4'
        whitespace   : ' '
        comment      : ' a comment'
        whitespace   : '\n '
        OPEN-SQUARE  : '['
        whitespace   : '   '
        integer      : '1'
        whitespace   : ' '
        string       : 'hi there'
        whitespace   : ' '
        integer      : '3'
        CLOSE-SQUARE : ']'
        CLOSE-PAREN  : ')'

 - step 1.5:  whitespace check while tokenizing:  checking that two
   of symbol/string/integer/float occur without interspersed 
   whitespace or punctuation

 - step 2:  strip whitespace and comment tokens

        OPEN-PAREN   : '('
        symbol       : 'cons'
        integer      : '4'
        OPEN-SQUARE  : '['
        integer      : '1'
        string       : 'hi there'
        integer      : '3'
        CLOSE-SQUARE : ']'
        CLOSE-PAREN  : ')'

 - step 3:  AST construction

        Application:
          - Function: 'cons'
          - arguments:
            - Number: 4
            - List:
              - Number: 1
              - String: 'hi there'
              - Number: 3

## Evaluation ##

 - step 4:  macro expansion.  There are currently **no** macros in
   Beagle, and this may never change.

 - step 5:  evaluate/reduce data structure.  The evaluation rule is
   that if it's a application, we evaluate each of the elements, and then 
   apply the first element as a function with the rest of the list
   as its arguments, so we'll need to apply `cons` to the number `4` and
   the list `[1, 'hi there', 3]`, which results in:

        List:
          - Number: 4
          - Number: 1
          - String: 'hi there'
          - Number: 3


## Wrap up ##

We saw how the process of execution of Beagle code works.  What we didn't see
was how special forms and built-in forms are evaluated.  That may be the subject
of a future article.

        
          