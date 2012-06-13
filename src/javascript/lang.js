

var lang = (function() {

var SYMBOL = /^[^\s\(\)]+/;

function nextToken(string) {
  // cases:
  //   1. string is empty
  //   2. first char is ( or )
  //   3. leading whitespace -> taken care of by trimming
  //   4. leading chars not in {whitespace or ( or ) }
  string = string.trim();
  if(!string) {
    return {
      token: false,
      rest: false
    };
  } else if(string[0] === "(" || string[0] === ")") {
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
  while(1) {
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

function getSymbol(string) {
  
}

function getList(string) {

}

function getSExpression(string) {
  // returns {
  //   parsed: ...
  //   remaining: ...string...
  // } if successful ...
  // but {
  //   error: ...
  //   ??
  // } if not !
  var sym = getSymbol(string);
  if(sym.parsed) {
    return sym;
  }
  var list = getList(string);
  if(list.parsed) {
    return list;
  }
  return {
    error: 'could not parse <' + string + '> as symbol or list'
  };
}

function parse(string) {
  // trim whitespace
  var trimmed = string.trim();
  // get an s-expression
  var sexpr = getSExpression(trimmed);
  if(sexpr.error) {
    return sexpr;
  } else if (sexpr.remaining) {
    // should only be whitespace left
    return {
      error: 'tried to parse string but found extra content: ' + sexpr.remaining
    };
  } else {// success?
    return sexpr;
  }    
}


return {
  'parse': parse,
  'nextToken': nextToken,
  'tokenize': tokenize
}

})();