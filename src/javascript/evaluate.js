
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
      
  for(i = 0; i < args.value.length; i++) {
    sym = args.value[i];
    if( sym.type !== 'symbol' ) {
      throw new Error("all parameter names in lambda must be symbols");
    }
    names.push(sym);
  }

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
      funcNames = ['cons', 'car', 'cdr', 'list', '='];

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


function evaluate(sexpr, env) {
  var first, 
      args,
      func,
      evaledArgs;
      
  if( !env || !sexpr ) {
    throw new Error("evaluate missing sexpr or environment");
  }
      
  if( sexpr.type === 'list' ) {
    // what if it's empty?
    if( !sexpr.value[0] ) {
      throw new Error("cannot evaluate empty list");
    }
    
    first = evaluate(sexpr.value[0], env);
    func = first.value;
    args = sexpr.value.slice(1);
    
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



return {
  'makePrimitives' : makePrimitives,
  'eval'           : evaluate,
  'getDefaultEnv'  : getDefaultEnv,
  'Environment'    : function(parent, bindings) {return new Env(parent, bindings);},
  'define'         : define
};

})(Data, Functions);
