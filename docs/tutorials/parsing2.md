
Parsing Beagle:  the code
==============
--------------

### Review:  Parsing ###

There are two main steps, starting from the textual form of the code:

 1. creating an abstract representation of the code

 2. executing or evaluating the representation to produce a result

This article will focus on the code that implements the first.


### Stage 1: Lexing ###

Just a quick reminder:  the input for lexing is a string, and the output is a list
of tokens.  So we'll expect these tokens from the first example:


Here's the main chunk of code that tokenizes a string:

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

this is the code that does that:

    function stripTokens(tokens) {
      function isNotCommentNorWS(token) {
        return (token.type !== 'comment' && token.type !== 'whitespace');
      }
      return tokens.filter(isNotCommentNorWS);
    }



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
