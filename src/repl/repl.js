
var Repl = (function(_, Backbone, Beagle) {
"use strict";


function Evaluator() {
  this._history = [];
  this._lisp = Beagle.lisp;
  this._environment = Beagle.environment;
}


Evaluator.prototype.evalString = function(str) {
  var evaled, obj;
  try {
    evaled = this._lisp(str);
    obj = {
      'expression': str,
      'result': evaled
    };
    this._history.push(obj);
    this.trigger("success", obj);
  } catch(e) {
    obj = {
      'expression': str,
      'errormessage': e
    };
    this._history.push(obj);
    this.trigger("error", obj);
  };
}


Evaluator.prototype.getEnvironment = function() {
  return this._environment;
}


_.extend(Evaluator.prototype, Backbone.Events);


return {
  'Evaluator': function() {return new Evaluator();},
  'getLastParse': Beagle.getLastParse,
  'getLastPrims': Beagle.getLastPrims
};

})(_, Backbone, Beagle);
