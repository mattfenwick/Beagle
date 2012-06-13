
var evaluate = (function() {

var INTEGER = /^\d+$/;

var FLOAT = /^(?:\d*\.\d+|\d+\.\d*)$/;

var STRING = /^"(.*)"$/;

function makePrimitives(sexpr) {
  var i, value, elems;

  if(typeof(sexpr) === "string") {
    if( value = sexpr.match(INTEGER) ) {
      return {
        'type': 'number',
        'value': Number(value[0])
      };
    } else if( value = sexpr.match(FLOAT) ) {
      return {
        'type': 'number',
        'value': Number(value[0])
      };
    } else if( value = sexpr.match(STRING) ) {
      return {
        'type': 'string',
        'value': value[1] // the second value is the match *without* the " marks
      };
    } else if( sexpr.length > 0 ) {
      return {
        'type': 'symbol',
        'value': sexpr
      };
    } else { // empty string -> Error
      throw new Error("can't extract primitive:  empty value");
    }
  } else { // assume it's a list/array
    elems = [];
    
    for(i = 0; i < sexpr.length; i++) {
      elems.push(makePrimitives(sexpr[i]));
    }
    
    return {
      'type': 'list',
      'value': elems
    };
  }
}


function myapply() {

}


function evaluate(sexpr) {
//  if(sexpr

}


return {
  'makePrimitives': makePrimitives,
  'apply'  : myapply,
  'eval' : evaluate
};
})();