
Parsing Beagle
==============
--------------

### Executing code ###

There are two main steps, starting from the textual form of the code:

 - creating an abstract representation of the code

 - executing or evaluating the representation to produce a result

This article will focus on the first.



### Parsing ###

The goal of parsing is to take human input -- text -- 
and transform it into input the computer understands.  (although computers can produce code too)

There are 2 or 3 major steps to this:

 1. lexing:  converting the input string into tokens

 2. throwing away tokens such as whitespace and comments

 3. parsing:  assembling the tokens to form phrases

So if you're creating a parser, what do you need to do?  You need to define your tokens,
decide which tokens are important to you, and define how your tokens are assembled to
build complicated phrases and expressions in your language.



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



## The code ##

### Lexing ###

    function nextToken(string) {
      var match;
    
      // 0. empty string
      if( string === "" ) {
        return false;
      } 
      
      // 1. leading whitespace
      if( match = string.match(WHITESPACE) ) {
        return {
          'token': new Token('whitespace', match[0]),
          'rest': string.substring(match[0].length)
        };
      }
      
      // 2. first char is '('
      if( string[0] === OPEN ) {
        return {
          'token': new Token('open', string[0]),
          'rest' : string.substring(1)
        };
      } 
    
      // 3. first char is ')'
      if( string[0] === CLOSE ) {
        return {
          'token': new Token('close', string[0]),
          'rest' : string.substring(1)
        };
      } 
    
      // 4. comment
      if( match = string.match(COMMENT) ) {
        return {
          'token': new Token('comment', match[1]),
          'rest' : string.substring(match[0].length)
        };
      }
    
      // 5. string
      if( string[0] === '"' ) {
        match = string.match(STRING);
        if( match ) {
          return {
            'token': new Token('string', match[1]),
            'rest': string.substring(match[0].length)
          };
        } else {
          throw new ParseError("tokenizer error: end-of-string (\") not found", string);
        }
      }
      
      // 6. symbol
      match = string.match(SYMBOL);
      if( match ) {
        return {
          'token': new Token('symbol', match[0]),
          'rest' : string.substring(match[0].length)
        };
      }
    
      throw new ParseError("unexpected tokenizer error:  no tokens match string", string);
    }


### Parsing ###

We saw in the grammar that an SExpression is an atom or a list.
So here's what that looks like in code:

    function getSExpression(tokens) {
      var sexpr;
    
      if( tokens.length === 0 ) {
        return false;
      }
    
      // an s-expression is either a symbol
      sexpr = getAtom(tokens);
      if( sexpr ) {
        return sexpr;
      }
      
      // or a list
      sexpr = getList(tokens);
      if( sexpr ) {
        return sexpr;
      }
    
      // no other possibilities
      throw new ParseError("unexpected error:  couldn't find s-expression and token stream was not empty", tokens);
    }

First, if the token stream is empty we abort.
Then we try to find an atom; if that succeeds we return it.
If that fails, we try to find a list; if that succeeds we return it.
If we can't find a list, then we assume that something unexpected happened
and throw an error with a (hopefully) meaningful error message.


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
