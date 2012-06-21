
var Parse = (function() {
"use strict";


// Token types: open, close, symbol, string, comment
function Token(type, value) {
  this.type = type;
  this.value = value;
}


// SEexpression types: symbol, string, list
function SExpression(type, value) {
  this.type = type;
  this.value = value;
}


function ParseError(message, value) {
  this.message = message;
  this.value = value;
}


ParseError.prototype.toString = function() {
  return this.message + " (from " + this.value + ")";
}

	
var SYMBOL  = /^[^\s\(\)"]+/, /* not ws, (, ), or " */
    OPEN    = "(",
    CLOSE   = ")",
    STRING  = /^"([^"]*)"/,
    COMMENT = /^;+(.*)/; /* assumes that: 1) * is greedy; 2) . doesn't match \n */


// String -> Maybe (Token, String)
//   where false is the "empty" value
function nextToken(string) {
  var match;
  
  // throw away leading whitespace -> trim it (also removes trailing ws -- problem?)
  string = string.trim();

  // 1. empty string
  if( string === "" ) {
    return false;
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


function tokenize(string) {
  var tokens = [],
      next;
  while( next = nextToken(string) ) {
    tokens.push(next.token);
    string = next.rest;
  }
  return tokens;
}


// [Token] -> Maybe SExpression
//   returns false if token stream is empty or first token is not a symbol or string
//   returns false if first token is a comment
function getAtom(tokens) {
  if( tokens.length === 0) {
    return false;
  }
	
  var first = tokens[0];

  if( first.type === 'symbol' || first.type === 'string' ) {
    return {
      result: new SExpression(first.type, first.value),
      rest: tokens.slice(1)
    };
  }
  
  return false;
}


// [Token] -> Maybe SExpression
//   returns false if tokens is empty or doesn't start with open
//   throws an error if a properly 'balanced' list can't be found
function getList(tokens) {
  var sexpr, 
      elems = [],
      inputTokens = tokens;
  
  // a list *has* to start with a '('
  if( tokens.length === 0 || tokens[0].type !== 'open' ) {
    return false;
  }

  tokens = tokens.slice(1);
  
  // keep going until a ')'
  while( tokens[0] && (tokens[0].type !== 'close') ) {
    // a list could have as many nested lists or atoms as it pleased
    sexpr = getSExpression(tokens);
    if( !sexpr ) {
      return false;
    }
    elems.push(sexpr.result);
    tokens = sexpr.rest;
  }

  // a list needs a close-paren
  if( tokens[0] && tokens[0].type === 'close' ) {
    return {
      result: new SExpression('list', elems),
      rest: tokens.slice(1)
    };
  }
  
  // uh-oh!  we didn't find a close-paren ...
  throw new ParseError("'(' token found but matching ')' token was not found", inputTokens);
}


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


function stripComments(tokens) {
  function isNotComment(token) {
    return token.type !== 'comment';
  }
  return tokens.filter(isNotComment);
}


// String -> Maybe [SExpression]
//   returns false if, after all s-expressions are extracted,
//   there are still tokens left in the token stream
function parse(string) {
  var allTokens = tokenize(string),
      tokens = stripComments(allTokens),
      sexprs = [],
      sexpr;
	
  while( sexpr = getSExpression(tokens) ) {
    sexprs.push(sexpr.result);
    tokens = sexpr.rest;
  }
	
  if( tokens.length !== 0 ) {
    throw new ParseError('unconsumed tokens remaining after parse', tokens);
  }

  return sexprs;
}


return {
  'Token'          : function(t, v) {return new Token(t, v);},
  'SExpression'    : function(t, v) {return new SExpression(t, v);},
  'ParseError'     : function(m, v) {return new ParseError(m, v);},

  'getAtom'        : getAtom,
  'getList'        : getList,
  'getSExpression' : getSExpression,
  'nextToken'      : nextToken,
  'stripComments'  : stripComments,

  'parse'          : parse,
  'tokenize'       : tokenize
};

})();