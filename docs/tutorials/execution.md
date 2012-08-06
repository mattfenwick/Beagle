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
      [   1 "h i" 3])


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
        string       : "h i"
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
        string       : "h i"
        integer      : '3'
        CLOSE-SQUARE : ']'
        CLOSE-PAREN  : ')'

 - step 3:  AST construction

        Application:
          - operator: symbol 'cons'
          - arguments:
            - ASTNumber: 4
            - ASTList:
              - ASTNumber: 1
              - ASTList:
                - ASTChar: 'h'
                - ASTChar: ' '
                - ASTChar: 'i'
              - ASTNumber: 3

## Evaluation ##

 - step 4:  recursive evaluation of AST.  The evaluator has an evaluation
   rule for each ASTNode type (of which there are 5); the meaning of a tree
   is determined from the meanings of its subtrees.  For more information
   about the evaluation rules, please see the informal semantics of Beagle.
   
   The rule for `Application`s is to evaluate the first element to a function,
   and then apply that function to the evaluation of each of the arguments.
   
   The symbol `cons` is bound to a built-in function of two arguments, that 
   prepends its 1st arg onto its 2nd arg, which must be a list. 
   
   To evaluate the arguments, we'd need to lookup the rules for symbols, lists,
   numbers, and chars.  That's too much work, so we'll just skip to the answer:
   
       Number:  4
       
       List:
         - Number: 1
         - List:
           - Char: 'h'
           - Char: ' '
           - Char: 'i'
         - Number: 3
         
   Notice that the evaluation gets rid of all AST information -- whereas we 
   started with `ASTNumber`s, we *ended up* with Lisp `Number`s.
   
   Then, applying the function, we end up with:

        List:
          - Number: 4
         - List:
           - Char: 'h'
           - Char: ' '
           - Char: 'i'
         - Number: 3


## Wrap up ##

We saw how the process of execution of Beagle code works.  What we didn't see
was how special forms and built-in forms are evaluated.  That may be the subject
of a future article.

        
          