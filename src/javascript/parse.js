
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

	
var SYMBOL  = /^[^\s\(\)"]+/, /* not ws, (, ), or " */
    OPEN    = "(",
    CLOSE   = ")",
    COMMENT = /^;+(.*)/; /* assumes that: 1) * is greedy; 2) . doesn't match \n */


function getStringToken(string) {
    var i, isStringEnded = false;
    if( string[0] !== '"' ) {
      throw new ParseError('string must begin with "', string[0]);
    }

    i = 1;
    while( i < string.length ) {
      if( string[i] === '"' ) {
        isStringEnded = true;
        break;
      } else {
        i += 1;
      }
    }
    if( !isStringEnded ) {
      throw new ParseError("tokenizer error: end-of-string (\") not found", string);
    }
    return {
      'token': new Token('string', string.substring(1, i)),
      'rest' : string.substring(i + 1)
    };
}


// String -> Maybe (Token, String)
//   where false is the "empty" value
function nextToken(string) {
  var match;
  
  // throw away leading whitespace -> trim it (also removes trailing ws -- problem?)
  string = string.trim();

  // 1. empty string
  if( !string ) {
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
    return getStringToken(string);
  }
  
  // 6. symbol
  match = string.match(SYMBOL);
  return {
      'token': new Token('symbol', match[0]),
      'rest' : string.substring(match[0].length)
  };
}


function tokenize(string) {
  var tokens = [],
      next;
  while( 1 ) {
    next = nextToken(string);
    if( next ) {
      tokens.push(next.token);
      string = next.rest;
    } else {
      break;
    }
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
//   returns false if a properly 'balanced' list can't be found
function getList(tokens) {
  var sexpr, 
      elems = [];
  
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
  return false;
}


function getSExpression(tokens) {
  var sexpr;

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
  return false;
}


// String -> Maybe [SExpression]
//   returns false if, after all s-expressions are extracted,
//   there are still tokens left in the token stream
function parse(string) {
  var tokens = tokenize(string),
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
  'getAtom'        : getAtom,
  'getList'        : getList,
  'getSExpression' : getSExpression,
  'parse'          : parse,
  'nextToken'      : nextToken,
  'tokenize'       : tokenize
}

})();