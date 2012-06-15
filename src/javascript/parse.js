
var Parse = (function() {
"use strict";
	
var SYMBOL = /^[^\s\(\)]+/,
    OPEN = "(",
    CLOSE = ")";


function nextToken(string) {
  var match;
  
  // throw away leading whitespace -> trim it (also removes trailing ws -- problem?)
  string = string.trim();

  // 1. empty string
  if( !string ) {
    return {
      token: false,
      rest: false
    };
  } 
  
  // 2. first char is '(' or ')'
  if( string[0] === OPEN || string[0] === CLOSE ) {
    return {
      token: string[0],
      rest: string.substring(1)
    };
  } 
  
  // 3. leading chars not in { whitespace or '(' or ')' }
  match = string.match(SYMBOL);
  return {
    token: match[0],
    rest: string.substring(match[0].length)
  };
}


function tokenize(string) {
  var tokens = [],
      next;
  while( 1 ) {
    next = nextToken(string);
    if( next.token ) {
      tokens.push(next.token);
      string = next.rest;
    } else {
      break;
    }
  }
  return tokens;
}


function getSymbol(tokens) {
  if( tokens.length === 0) {
	  return false;
  }
	
  var first = tokens[0];
  
  if( first === OPEN || first === CLOSE ) {
    return false;
  }
  
  return {
    result: first,
    rest: tokens.slice(1)
  };
}


function getList(tokens) {
  var sexpr, elems;
  
  if( tokens[0] !== OPEN ) {
    return false;
  }

  elems = [];
  tokens = tokens.slice(1);
  
  while( tokens[0] && (tokens[0] !== CLOSE) ) {
    sexpr = getSExpression(tokens);
    if(!sexpr) {
      return false;
    }
    elems.push(sexpr.result);
    tokens = sexpr.rest;
  }

  if( tokens[0] === CLOSE ) {
    return {
      result: elems,
      rest: tokens.slice(1)
    };
  }
  
  return false;
}


function getSExpression(tokens) {
  var sexpr;

  // an s-expression is either a symbol
  sexpr = getSymbol(tokens);
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


function parseOne(string) {
  var tokens = tokenize(string),
      sexpr;

  if( tokens.length === 0 ) {
    return false;
  }

  sexpr = getSExpression(tokens);
  
  // got an s-expression and nothing but an s-expression
  if( sexpr && (sexpr.rest.length === 0) ) {
    return sexpr.result;
  } 
  
  // got an s-expression but there was stuff after it
  if ( sexpr ) {
    return false;
  } 
  
  // couldn't get an s-expression
  return false;
}


function parseAll(string) {
	var tokens = tokenize(string),
	    sexprs = [],
	    sexpr;
	
	while( sexpr = getSExpression(tokens) ) {
		sexprs.push(sexpr.result);
		tokens = sexpr.rest;
	}
	
	if( tokens.length !== 0 ) {
		return false;
	}
	
	return {
		'sexprs': sexprs
	};
}


return {
  // [Token] -> ParseResult
  'getSymbol'      : getSymbol,
  'getList'        : getList,
  'getSExpression' : getSExpression,
  'parse'          : parseOne,
  'parseAll'       : parseAll,
  'nextToken'      : nextToken,
  'tokenize'       : tokenize
}

})();