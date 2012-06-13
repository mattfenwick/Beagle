
var Parse = (function() {

var SYMBOL = /^[^\s\(\)]+/;

function nextToken(string) {
  // cases:
  //   1. string is empty
  //   2. first char is ( or )
  //   3. leading whitespace -> taken care of by trimming
  //   4. leading chars not in {whitespace or ( or ) }
  string = string.trim();
  if( !string ) {
    return {
      token: false,
      rest: false
    };
  } else if( string[0] === "(" || string[0] === ")" ) {
    return {
      token: string[0],
      rest: string.substring(1)
    };
  } else {
    var match = string.match(SYMBOL);
    return {
      token: match[0],
      rest: string.substring(match[0].length)
    };
  }
}


function tokenize(string) {
  var tokens = [],
      next;
  while( 1 ) {
    next = nextToken(string);
    if(next.token) {
      tokens.push(next.token);
      string = next.rest;
    } else {
      break;
    }
  }
  return tokens;
}


function getSymbol(tokens) {
  var first = tokens[0];
  if( first === "(" || first === ")" ) {
    return false;
  } else {
    return {
      result: first,
      rest: tokens.slice(1)
    };
  }
}


function getList(tokens) {
  if( tokens[0] !== "(" ) {
    return false;
  }

  var elems = [];
  tokens = tokens.slice(1);
  
  while( tokens[0] && (tokens[0] !== ")") ) {
    sexpr = getSExpression(tokens);
    if(!sexpr) {
      return false;
    }
    elems.push(sexpr.result);
    tokens = sexpr.rest;
  }

  if( tokens[0] === ")" ) {
    return {
      result: elems,
      rest: tokens.slice(1)
    };
  } else {
    return false;
  }
}


function getSExpression(tokens) {
  var res = getSymbol(tokens);
  if(res) {
    return res;
  }
  res = getList(tokens);
  if( res ) {
    return res;
  }
  return false;
}


function parse(string) {
  var tokens = tokenize(string);

  if( tokens.length === 0 ) {
    return false;
  }

  var sexpr = getSExpression(tokens);
  if( sexpr && (sexpr.rest.length === 0) ) { // got an s-expression and nothing but an s-expression
    return sexpr.result;
  } else if ( sexpr ) { // got an s-expression but there was stuff after it
    return false;
  } else { // couldn't get an s-expression
    return false;
  }    
}


return {
  'getSymbol'      : getSymbol,
  'getList'        : getList,
  'getSExpression' : getSExpression,
  'parse'          : parse,
  'nextToken'      : nextToken,
  'tokenize'       : tokenize
}

})();