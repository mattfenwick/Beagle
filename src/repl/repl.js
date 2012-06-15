
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
    this._history.push(evaled);
    this.trigger("success", evaled);
  } catch(e) {
    obj = {
      'string': str,
      'errormessage': e
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
