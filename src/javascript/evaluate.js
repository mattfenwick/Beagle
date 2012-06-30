var Evaluate = (function(Data, Functions) {
"use strict";

var INTEGER = /^\d+$/;

var FLOAT = /^(?:\d*\.\d+|\d+\.\d*)$/;

function makePrimitives(sexpr) {
  var i, value, elems;
  
  if( sexpr.type === 'list' ) {
    elems = [];
    
    for(i = 0; i < sexpr.value.length; i++) {
      elems.push(makePrimitives(sexpr.value[i]));
    }
    
    return Data.List(elems);
  }

  if( sexpr.type === "string" ) {
    return Data.String(sexpr.value);
  }

  if( sexpr.type === 'symbol' ) {
      if( value = sexpr.value.match(INTEGER) ) {
          return Data.Number(Number(value[0]));
      } 
    
      if( value = sexpr.value.match(FLOAT) ) {
          return Data.Number(Number(value[0]));
      } 
    
      if( sexpr.value.length > 0 ) {
          return Data.Symbol(sexpr.value);
      } 
    
      // empty string is an error
      throw new Error("can't extract primitive:  empty value");
  }

  throw new Error("unrecognized s-expression type: " + sexpr.type);
}



//////// Special forms

function define(env, name, sexpr) {
  if( name.type !== "symbol" ) {
    throw new Error("define needs a symbol as its first argument (got " + name.type + ")");
  }
  var value = evaluate(sexpr, env);
  env.addBinding(name.value, value);
  return Data.Nil();
}


function myif(env, condition, ifTrue, ifFalse) {
  var cond = evaluate(condition, env);

  if( cond.type !== 'boolean' ) {
    throw new Error("if needs a boolean as first argument");
  }

  if( cond.value ) {
    return evaluate(ifTrue, env);
  }
  
  return evaluate(ifFalse, env);
}


function lambda(env, args, body) {
  if( args.type !== 'list' ) {
    throw new Error("lambda requires a list as first argument");
  }
  
  var names = [],
      i, sym;
      
  // get the list of arguments
  for(i = 0; i < args.value.length; i++) {
    sym = args.value[i];
    if( sym.type !== 'symbol' ) {
      throw new Error("all parameter names in lambda must be symbols");
    }
    names.push(sym);
  }

  // create the closure,
  //   which stores a reference to the environment,
  //   and when evaluated, creates a new environment
  //   and puts its arguments in the environment
  //   then it evaluates its body in the new environment
  function closure() {
      var ln = names.length,
          la = arguments.length,
          newEnv = new Env(env, {});

      if( ln !== la ) {
          throw new Error("length of parameter list of lambda does not match arguments list: " + ln + " vs " + la);
      }

      for(var j = 0; j < names.length; j++) {
          // arguments already evaluated
          newEnv.addBinding(names[j].value, arguments[j]);
      }

      return evaluate(body, newEnv);
  }
  
  // could create a new data type (closure) later if desired
  return Data.Function(closure);
}


///////////


function Env(parent, bindings) {
  this._parent = parent;
  this._bindings = bindings;
}


Env.prototype.hasOwnBinding = function(name) {
  return this._bindings.hasOwnProperty(name);
}


Env.prototype.hasBinding = function(name) {
  if( this.hasOwnBinding(name) ) {
    return true;
  }
  if( this._parent ) {
    return this._parent.hasBinding(name);
  }
  return false;
}


Env.prototype.addBinding = function(name, value) {
  if( this.hasOwnBinding(name) ) {
    throw new Error("environment already has binding for " + name);
  }
  this._bindings[name] = value;
}


Env.prototype.getBinding = function(name) {
  if( this.hasOwnBinding(name) ) {
    return this._bindings[name];
  }
  if( this._parent ) {
    return this._parent.getBinding(name);
  }
  throw new Error("could not find value for " + name);
}


function getDefaultEnv() {
  var bindings = {},
      funcNames = ['cons', 'car', 'cdr', 'list', '=', '+', 'neg'];

  funcNames.map(function(name) {
    bindings[name] = Data.Function(Functions[name]);
  });
  
  bindings['define'] = Data.SpecialForm(define);
  bindings['if']     = Data.SpecialForm(myif);
  bindings['lambda'] = Data.SpecialForm(lambda);

  bindings['true']   = Data.Boolean(true);
  bindings['false']  = Data.Boolean(false);

  return new Env(null, bindings);
}




function myapply(f, args) {
  return f.apply(null, args);
}


function specialapply(f, env, args) {
  return f.apply(null, [env].concat(args));
}


function evaluateList(sexpr, env) {
    // what if it's empty?
    if( !sexpr.value[0] ) {
      throw new Error("cannot evaluate empty list");
    }

    var first = evaluate(sexpr.value[0], env), 
        args = sexpr.value.slice(1),
        func = first.value,
        evaledArgs;
        
    if( first.type === 'function' ) {  

      evaledArgs = args.map(function(a) {
          return evaluate(a, env);
      });

      return myapply(func, evaledArgs);
    }
    
    if( first.type === 'specialform' ) {
      return specialapply(func, env, args);
    }

    throw new Error("first element in list must be function or special form (was " + first.type + ")");
}


function evaluateAtom(sexpr, env) {
  if ( sexpr.type === 'symbol' ) {
    if( env.hasBinding(sexpr.value) ) { // uh ... has own property?
      return env.getBinding(sexpr.value);
    } else {
      throw new Error("could not find symbol " + sexpr.value);
    }
  } 
  
  if ( sexpr.type === 'number' || sexpr.type === 'string' || sexpr.type === 'boolean' ) {
    return sexpr;
  } 

  throw new Error("unrecognized type: " + sexpr.type + " in " + JSON.stringify(sexpr));
}


function evaluate(sexpr, env) {      
  if( !env || !sexpr ) {
    throw new Error("evaluate missing sexpr or environment");
  }
      
  if( sexpr.type === 'list' ) {
    return evaluateList(sexpr, env);
  }
  
  return evaluateAtom(sexpr, env);
}



return {
  'makePrimitives' : makePrimitives,
  'eval'           : evaluate,
  'getDefaultEnv'  : getDefaultEnv,
  'Environment'    : function(parent, bindings) {return new Env(parent, bindings);},
  'define'         : define
};

})(Data, Functions);
