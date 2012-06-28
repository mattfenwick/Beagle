
Parsing Beagle: part 1
==============
--------------

### What is Beagle? ###

Beagle is a simple dialect of Lisp.  It is hosted on Github
[here](https://github.com/mattfenwick/Beagle).

Lisps are very easy to parse, due to their use of parentheses.
Parentheses are possibly both the most popular -- because of its uniformity --
and most hated -- because of its ugliness -- feature of Lisp, and
definitely the most visually distinctive!

Parsing is often seen as a black art, involving heavy-duty parser generators,
complicated BNF grammars, abstract syntax trees, symbol tables ....

Much of this is due to the fact that most programming languages have 
very difficult syntax.

My goal in writing this article is to give a clear, practical, and useful
explanation for how to build a parser.  Using a Lisp as an example language
helps keep the discussion simple without creating a need to resort to 
hand-waving.

Thus, in this article, we'll see the basic steps for how to transform a string
of characters into an abstract, executable representation of code.



### Parsing overview ###

The goal of parsing is to transform human- or computer-generated input text -- 
into something the computer can understand and execute.

This is often broken down into 3 main steps:

 1. lexing (AKA tokenization or scanning):  convert an input string into tokens.  

 2. throw away tokens such as whitespace and comments that do not
    contribute to the executable code

 3. syntactic analysis:  assemble the tokens to form phrases and sentences

For the rest of this article, we'll use this simple example to demonstrate
import concepts:

    (define x "hi")



### Stage 1: Lexing ###

Here are Beagle's token definitions:

    STRING:         "[^\"]*"

    SYMBOL:         [^;\"\(\)\s]+

    OPEN:           (

    CLOSE:          )

    WHITESPACE:     /^\s+/

    COMMENT:        /^;+(.*)/

*(We'll write token names in all caps to distinguish them).*

Now, just a quick reminder:  the input for lexing is a string, and the output is a list
of tokens.  So, using the above token definitions, we'll go through the string
`(define x 4)` and match the text to individual tokens.

As it happens, there are 7 tokens:

    ['(', 'define', ' ', 'x', ' ', '"hi"', ')']

But how do we get that answer?

1. the first character is `(` -- this matches the `OPEN` token

2. the next characters -- `define` -- aren't semicolons, open or close parentheses,
   or whitespace, and so they match the `SYMBOL` token

3. a single space matches `WHITESPACE`

4. `x` is a `SYMBOL` token (see 2. for explanation)

5. `WHITESPACE` (see 3.)

6. if it starts with a `"` mark, it must be a `STRING` token

7. a `)` is a CLOSE

Note that our answer is unique -- when I created the token definitions, I tried
to make sure that there was never any possibility for ambiguity when tokenizing
a string.



### Stage 2: get rid of unwanted tokens ###

We don't need the comments or the whitespace for syntac analysis (although they would be
useful to a tool that generates web-based documentation from a source code file, for
instance), so let's get rid of them, leaving us with these tokens:

    ["(", "define", "x", "4", ")"]



### Stage 3: Syntactic analysis ### 

In this step, we assemble the tokens according to our grammar, which is:

    BeagleCode:     SExpression(+)

    SExpression:    Atom  |  List

    List:           OPEN  SExpression(*)  CLOSE

    Atom:           STRING  |  SYMBOL

*(Remember that tokens are in all caps -- these productions have only leading caps).*

Basically, the grammar says that Beagle code is a bunch of s-expressions,
and that s-expressions can be atoms or lists.  An atom is either a string or a 
symbol, and a list is an open-paren and a close-paren surrounding any number
of s-expressions (so the grammar is recursive).

We can match this grammar to our tokens using a strategy called "recursive descent".
Let's try it ourselves:

    1. try BeagleCode
     2. try SExpression
      3. try Atom
       4. try STRING ... failed
       5. try SYMBOL ... failed
      6. try List
       7. try OPEN ... succeeded (matched `(`)
       8. try SExpression
        9. try Atom
         10. try STRING ... failed
         11. try SYMBOL ... succeeded (matched `define`)
       12. try SExpression
        13. try ATOM
         14. try STRING ... failed
         15. try SYMBOL ... succeeded (matched `x`)
       16. try SExpression
        17. try ATOM
         18. try STRING ... succeeded (matched `hi`)
       19. try SExpression
        20. try ATOM
         21. try STRING ... failed
         22. try SYMBOL ... failed
        21. try LIST
         23. try OPEN ... failed
       24. try CLOSE ... succeeded (matched `)`)
      <matched List>
     <matched SExpression>
    <matched BeagleCode> 

Cool, it worked!  Our parse tree now looks like: 

    list: 
      symbol: define 
      symbol: x 
      string: hi


### Dealing with faulty input ###

Real-world (read: useful) parsers will have to deal with problems such as:

 - error-detection

 - error messages

 - error-tolerance

I've heard it said that dealing with errors accounts for ~80% of the code in
typical projects, and indeed, this is no exception, so let's skip it. :)



### Summary ###

What did we do?  We saw that parsing can be split into:

 - tokenizing a string, using regular expressions

 - discarding uninteresting tokens

 - assembling tokens into a parse tree, using a grammar

These are the basics of parsing.  Extending the examples to parse Javascript will
be left as an exercise for the reader!

