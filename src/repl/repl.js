
var Repl = (function(_, Backbone, Beagle) {
"use strict";


function Evaluator() {
  this._history = [];
  this._lisp = Beagle.exec;
  this._environment = Beagle.environment;
}


Evaluator.prototype.evalString = function(str) {
  var evaled, obj;
  try {
    evaled = this._lisp(str);
    if( evaled.result.length !== 1) {
      throw new Error("needed exactly one form (found " + evaled.result.length + ")");
    }
    this._history.push(evaled);
    this.trigger("success", evaled);
  } catch(e) {
    obj = {
      'string': str,
      'error': e
    };
    this._history.push(obj);
    this.trigger("error", obj);
    throw e; // give the browser the chance to do something with it
  };
}


Evaluator.prototype.getEnvironment = function() {
  return this._environment;
}


_.extend(Evaluator.prototype, Backbone.Events);


return {
  'Evaluator': function() {return new Evaluator();}
};

})(_, Backbone, Beagle);
