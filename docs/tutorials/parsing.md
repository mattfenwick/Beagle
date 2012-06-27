
Parsing Beagle
==============
--------------

### Executing code ###

There are two main steps, starting from the textual form of the code:

 1. creating an abstract representation of the code

 2. executing or evaluating the representation to produce a result

This article will focus on the first.



### Parsing ###

The goal of parsing is to take human- or computer-generated input text -- 
and transform it into something the computer understands.

There are 2 or 3 major steps to this:

 1. lexing:  converting the input string into tokens

 2. throwing away tokens such as whitespace and comments

 3. parsing:  assembling the tokens to form phrases

So if you're creating a parser, what do you need to do?  You need to define your tokens,
decide which tokens are important to you, and define how your tokens are assembled to
build complicated phrases and expressions in your language.

We'll look at two examples throughout this article:

    (define x 4)

and

    (cons 4      ; add an element
      (list 5))  ; to this list



### Grammar ###

Let's take a look at Beagle's grammar, loosely using BNF:

    BeagleCode:     SExpression(+)

    SExpression:    Atom  |  List

    Atom:           String  |  Symbol

    String:         "[^\"]*"

    Symbol:         [^;\"\(\)\s]+

    List:           OPEN  SExpression(*)  CLOSE

    OPEN:           '('

    CLOSE:          ')'

*(+ means one or more; * means 0 or more; | means 'or'; whitespace means 'and')*

So we see tokens:  String, Symbol, OPEN, and CLOSE (there are also two more tokens
missing, comment and whitespace).
Then we see more complicated productions such as Atom, List, and SExpression.  Finally,
we see `BeagleCode`, which is composed of one or more SExpressions.



## Examples and the code ##

### Stage 1: Lexing ###

Just a quick reminder:  the input for lexing is a string, and the output is a list
of tokens.  So we'll expect these tokens from the first example:

    ["(", "define", " ", "x", " ", "4", ")"]

that's 7 tokens -- notice that we're keeping whitespace as a token (for now).  We
can always throw it away later.  And for the second example, try and figure out for
yourself how many tokens there are.

Did you make a guess?

[
 {type:"whitespace", value:"    "}, 
 {type:"open", value:"("}, 
 {type:"symbol", value:"cons"}, 
 {type:"whitespace", value:" "}, 
 {type:"symbol", value:"4"}, 
 {type:"whitespace", value:"      "}, 
 {type:"comment", value:" add an element"}, 
 {type:"whitespace", value:"\n      "}, 
 {type:"open", value:"("}, 
 {type:"symbol", value:"list"}, 
 {type:"whitespace", value:" "}, 
 {type:"symbol", value:"5"}, 
 {type:"close", value:")"}, 
 {type:"close", value:")"}, 
 {type:"whitespace", value:"  "}, 
 {type:"comment", value:" to this list"}
]

There are 16 tokens!  6 whitespace, 2 comments, 2 opens, 2 closes, and 4 symbols.


That's called in a loop that basically says to "keep giving me tokens until you find
the end of the string".  

`nextToken` works by successively trying to match the string to each type of token; 
when one succeeds, it splits a chunk of the string off into the token, and returns
the Token along with the rest of the string that wasn't consumed.  Just in case none
of the token types match (which won't happen in theory but will in practice!), `nextToken`
throws up its hands in despair and makes a helpful error message.


### Stage 2: get rid of unwanted tokens ###

We don't need the comments or the whitespace for parsing -- although they would be
useful to a tool that generates web-based documentation from a source code file, for
instance.  But since we just want to run the code, we can throw them away.

So now our tokens are:

    ["(", "define", "x", "4", ")"]

down to 5 tokens for the first example, and 8 for the second: 

[
 {type:"open", value:"("}, 
 {type:"symbol", value:"cons"}, 
 {type:"symbol", value:"4"}, 
 {type:"open", value:"("}, 
 {type:"symbol", value:"list"}, 
 {type:"symbol", value:"5"}, 
 {type:"close", value:")"}, 
 {type:"close", value:")"}, 
]

this is the code that does that:




### Stage 3: Parsing ###

In this step, we assemble the tokens into s-expressions (lists and atoms).

    {
      type:"list", 
      value:[
        {type:"symbol", value:"define"}, 
        {type:"symbol", value:"x"}, 
        {type:"symbol", value:"4"}
      ]
    }

and for the second example:

    {
      type:"list", 
      value:[
        {type:"symbol", value:"cons"},
        {type:"symbol", value:"4"},
        {type:"list",
         value:[
            {type:"symbol", value:"list"}, 
            {type:"symbol", value:"5"}
         ]
        }
      ]
    }

We saw in the grammar that an SExpression is an atom or a list.



### Dealing with faulty input ###

 - error-detection

 - error messages

 - error-tolerance



### Summary ###

What did we do?

 - we saw how to tokenize a string, using regular expressions

 - we saw how to discard uninteresting tokens

 - we saw how to assemble tokens into complicated data structures -- s-expressions

These are the basics of creating parsers.
The code is relatively small and simple for Beagle, because Lisp is extremely easy to
parse.  Languages such as Javascript, Python, and C have much more complicated syntax
but the basics are the same.

Classification questions:  

 - is this top-down or bottom-up?  (I believe it's recursive descent, so top-down)

 - Is this context-free or context-sensitive?  (context-free ??)

 - Is this LL or LR? (I think it's LL(1))

 - backtracking? (no?)

 - deterministic?  ambiguous?  (yes & no?)
