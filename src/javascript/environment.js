var Environment = (function(Data) {

function cons(elem, list) {
  var newList = [elem];
  for(var i = 0; i < list.value.length; i++) {
    newList.push(list.value[i]);
  }
  return Data.List(newList);
};


function car(list) {
  if( list.value.length > 0 ) {
    return list.value[0];
  }
  
  return Data.Nil();
}


function cdr(list) {
  if( list.value.length == 0 ) {
    return Data.Nil();
  }
  
  return Data.List(list.value.slice(1));
}


function list() {
  var args = [];
  for(var i = 0; i < arguments.length; i++) {
    args.push(arguments[i]);
  }
  return Data.List(args);
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


var defaultEnv = new Env(null, {

  'cons': Data.Function(cons),
  
  'car': Data.Function(car),
  
  'cdr': Data.Function(cdr),
  
  'list': Data.Function(list)
  
});


return {
  'defaultEnv': defaultEnv
};

})(Data);
