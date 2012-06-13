
var evaluate = (function() {

function MyNumber(value) {
  this.value = value;
  this.type = 'number';
}


function MyString(value) {
  this.value = value;
  this.type = 'string';
}


function Symbol(value) {
  this.value = value;
  this.type = 'symbol';
}


function List(value) {
  this.value = value;
  this.type = 'list';
}


function MyFunction(value) {
  this.value = value;
  this.type = 'function';
}


function Nil() {
  this.value = false;
  this.type = 'nil';
}



var INTEGER = /^\d+$/;

var FLOAT = /^(?:\d*\.\d+|\d+\.\d*)$/;

var STRING = /^"(.*)"$/;

function makePrimitives(sexpr) {
  var i, value, elems;

  if(typeof(sexpr) === "string") {
      if( value = sexpr.match(INTEGER) ) {
          return new MyNumber(Number(value[0]));
      } 
    
      if( value = sexpr.match(FLOAT) ) {
          return new MyNumber(Number(value[0]));
      } 
    
      if( value = sexpr.match(STRING) ) {
          return new MyString(value[1]); // the second value is the match *without* the " marks
      }
    
      if( sexpr.length > 0 ) {
          return new Symbol(sexpr);
      } 
    
      // empty string -> Error
      throw new Error("can't extract primitive:  empty value");
  }
  
  // assume it's a list/array
  elems = [];
    
  for(i = 0; i < sexpr.length; i++) {
    elems.push(makePrimitives(sexpr[i]));
  }
    
  return new List(elems);
}


function cons(elem, list) {
  var newList = [elem];
  for(var i = 0; i < list.value.length; i++) {
    newList.push(list.value[i]);
  }
  return new List(newList);
};


function car(list) {
  if( list.value.length > 0 ) {
    return list.value[0];
  }
  
  return new Nil();
}


function cdr(list) {
  if( list.value.length == 0 ) {
    return new Nil();
  }
  
  return new List(list.value.slice(1));
}


function list() {
  var args = [];
  for(var i = 0; i < arguments.length; i++) {
    args.push(arguments[i]);
  }
  return new List(args);
}


var environment = {

  'cons': new MyFunction(cons),
  
  'car': new MyFunction(car),
  
  'cdr': new MyFunction(cdr),
  
  'list': new MyFunction(list)
  
};


function myapply(f, args) {
  return f.apply(null, args);
}



function evaluate(sexpr, env) {
  if( sexpr.type === 'list' ) {
    // what if it's empty?
    if( !sexpr.value[0] ) {
      throw new Error("cannot evaluate empty list");
    }
    
    var first = evaluate(sexpr.value[0], env);
    
    if( first.type !== 'function' ) {
      throw new Error("first element in list must be function");
    }
    
    var args = sexpr.value.slice(1);

    var evaledArgs = args.map(function(a) {
        return evaluate(a, env);
    });

    var func = first.value;
    var val = myapply(func, evaledArgs);
    return val;
  }
  
  if ( sexpr.type === 'symbol' ) {
    if( sexpr.value in env ) { // uh ... has own property?
      return env[sexpr.value];
    } else {
      throw new Error("could not find symbol " + sexpr.value);
    }
  } 
  
  if ( sexpr.type === 'number' || sexpr.type === 'string' ) {
    return sexpr;
  } 

  throw new Error("unrecognized type: " + sexpr.type);
}


function evalHead(sexpr) {
  return evaluate(sexpr, environment);
}


return {
  'makePrimitives' : makePrimitives,
  'apply'          : myapply,
  'eval'           : evalHead,
  'environment'    : environment,
  'Number'         : function(x) {return new MyNumber(x)},
  'String'         : function(x) {return new MyString(x)},
  'Function'       : function(x) {return new MyFunction(x)},
  'List'           : function(x) {return new List(x)},
  'Symbol'         : function(x) {return new Symbol(x)},
  'Nil'            : function(x) {return new Nil(x)}
};
})();
