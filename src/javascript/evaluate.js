
var Evaluate = (function(Data, Functions) {

var INTEGER = /^\d+$/;

var FLOAT = /^(?:\d*\.\d+|\d+\.\d*)$/;

var STRING = /^"(.*)"$/;

function makePrimitives(sexpr) {
  var i, value, elems;

  if(typeof(sexpr) === "string") {
      if( value = sexpr.match(INTEGER) ) {
          return Data.Number(Number(value[0]));
      } 
    
      if( value = sexpr.match(FLOAT) ) {
          return Data.Number(Number(value[0]));
      } 
    
      if( value = sexpr.match(STRING) ) {
          return Data.String(value[1]); // the second value is the match *without* the " marks
      }
    
      if( sexpr.length > 0 ) {
          return Data.Symbol(sexpr);
      } 
    
      // empty string -> Error
      throw new Error("can't extract primitive:  empty value");
  }
  
  // assume it's a list/array
  elems = [];
    
  for(i = 0; i < sexpr.length; i++) {
    elems.push(makePrimitives(sexpr[i]));
  }
    
  return Data.List(elems);
}



function Env(parent, bindings) {
  this._parent = parent;
  this._bindings = bindings;
}


Env.prototype.addBinding = function(name, value) {
  if( this.hasBinding(name) ) {
    throw new Error("environment already has binding for " + name);
  }
  this._bindings[name] = value;
}


Env.prototype.hasBinding = function(name) {
  return this._bindings.hasOwnProperty(name);
}


Env.prototype.getBinding = function(name) {
  if( this.hasBinding(name) ) {
    return this._bindings[name];
  }
  if( this.parent ) {
    return this.parent.getBinding(name);
  }
  throw new Error("could not find value for " + name);
}


function getDefaultEnv() {
  var bindings = {},
      funcNames = ['cons', 'car', 'cdr', 'list'];

  funcNames.map(function(name) {
    bindings[name] = Data.Function(Functions[name]);
  });

  return new Env(null, bindings);
}

var defaultEnv = getDefaultEnv();




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
    if( env.hasBinding(sexpr.value) ) { // uh ... has own property?
      return env.getBinding(sexpr.value);
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
  return evaluate(sexpr, defaultEnv);
}


return {
  'makePrimitives' : makePrimitives,
  'eval'           : evalHead,
  'evalEnv'        : evaluate,
  'defaultEnv'     : defaultEnv,
  'Environment'    : function(parent, bindings) {return new Env(parent, bindings);}
};

})(Data, Functions);
